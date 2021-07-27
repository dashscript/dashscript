extern crate alloc;

use std::{ptr};
use std::path::PathBuf;
use std::convert::TryInto;
use std::collections::HashMap;
use super::memory::*;
use crate::{
    Value, RuntimeResult, RuntimeError, ObjectTrait, Chunk, TinyString, Function, Upvalue, 
    UpvalueState, ValueIter, ValuePtr, Instance, Map, Fiber, FiberHandle, ResourceTable, 
    Generator, opcode, core
};

// Used to read a byte forward in the bytecode.
macro_rules! read_u8 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.fiber.ip) {
            Some(byte) => {
                $self.fiber.ip += 1;
                *byte
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 1 more byte to form u8."))
        }
    };
}

// Used to read u16 by reading 2 bytes ahead.
macro_rules! read_u16 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.fiber.ip..$self.fiber.ip + 2) {
            Some(byte) => {
                $self.fiber.ip += 2;
                u16::from_le_bytes(byte.try_into().unwrap())
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to form u16."))
        }
    };
}

// Used to read u32 by reding 4 bytes ahead.
macro_rules! read_u32 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.fiber.ip..$self.fiber.ip + 4) {
            Some(byte) => {
                $self.fiber.ip += 4;
                u32::from_le_bytes(byte.try_into().unwrap())
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 4 more bytes to form u32."))
        }
    };
}

// Used to read a configurable number with two cases.
// Case 1: 0 | [1 byte]   If the first byte is 0, then need to read 1 more byte to form u8.
// Case 2: 1 | [4 bytes]  If the second byte is 1, then need to read 4 more bytes to from u32.
macro_rules! read_auto {
    ($self:expr) => {
        match $self.chunk.bytes[$self.fiber.ip] {
            0 => {
                $self.fiber.ip += 1;
                read_u8!($self) as u32
            },
            1 => {
                $self.fiber.ip += 1;
                read_u32!($self)
            },
            _ => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 1 more byte as either 1[LONG] or 0[SHORT]."))
        }
    };
}

// Used to read a string by a paticular index.
macro_rules! read_string {
    ($self:expr, $index:expr) => {{
        let bytes = match $self.chunk.constants.strings.get($index as usize) {
            Some(string) => string.as_bytes(),
            None => &[]
        };

        #[allow(mutable_borrow_reservation_conflict)]
        let constant = $self.allocate_with(TinyString::new(bytes));
        $self.push(Value::String(constant));
    }};
}

pub type MethodFn<T> = fn (&mut Vm, &mut T, *const u8, &[Value]) -> RuntimeResult<Value>;
pub type MethodMap<T> = HashMap<TinyString, MethodFn<T>>;
pub type Flags = HashMap<TinyString, TinyString>;

// The permissions which are used by the standard native library. Mainly focused to remove 
// of unwanted modules which will not be used in the runtime. 
#[derive(Debug, Clone, Copy, Default)]
pub struct Permissions {
    pub read: bool,
    pub write: bool,
    pub memory: bool,
    pub process: bool,
    pub _unsafe: bool
}

// A dictionary of strings which are been mostly used by the native standard library.
// Instead of allocating the same strings everytime, these are some constant strings which
// is allocated when the VM is created.
#[derive(Default)]
pub struct Constants {
    init: Value,
    pub(super) prototype: Value,
    pub(super) __listeners: Value,
    pub(super) __time: Value,
    pub(super) __date: Value,
    pub(super) rid: Value,
    pub(super) pid: Value,
    pub(super) stdin: Value,
    pub(super) stdout: Value,
    pub(super) stderr: Value,
    pub(super) cwd: Value,
    pub(super) cmd: Value,
    pub(super) env: Value,
    pub(super) process_prototype: ValuePtr<Map>
}

// The main vm which contains all the necessities to execute the bytecode.
pub struct Vm {
    pub(crate) chunk: Chunk,
    pub(crate) permissions: Permissions,
    pub(crate) bytes_allocated: usize,
    pub(crate) next_gc: usize,
    pub(crate) objects: Vec<GcHandle>,
    pub(crate) path: PathBuf,
    pub(crate) fiber: FiberHandle,
    pub(crate) constants: Constants,
    pub iterator_methods: MethodMap<ValueIter>,
    pub string_methods: MethodMap<TinyString>,
    pub array_methods: MethodMap<Vec<Value>>,
    pub globals: HashMap<u32, (Value, bool)>,
    pub resource_table: ResourceTable,
    flags: Flags
}

impl Vm {

    pub fn new(
        chunk: Chunk, 
        flags: Flags, 
        path: PathBuf,
        permissions: Option<Permissions>
    ) -> RuntimeResult<Self> {
        let permissions = match permissions {
            Some(permissions) => permissions,
            None => permission_from_flags(&flags)
        };

        let mut vm = Self {
            fiber: FiberHandle::new(Fiber::new("runtime", chunk.bytes[0], 1)),
            chunk,
            flags,
            path,
            permissions,
            array_methods: MethodMap::new(),
            string_methods: MethodMap::new(),
            iterator_methods: MethodMap::new(),
            constants: Constants::default(),
            globals: HashMap::new(),
            resource_table: ResourceTable::default(),
            next_gc: u16::MAX as usize,
            bytes_allocated: 0,
            objects: Vec::new(),
        };

        vm.constants = load_constants(&mut vm);
        core::init(&mut vm);

        vm.execute()?;
        match vm.execute() {
            Ok(_) => Ok(vm),
            Err(error) => Err(error)
        }
    }

    pub fn handle_error(&mut self, error: RuntimeError) -> RuntimeResult<()> {
        // Check if the errors are catchable, catchable errors are basically panics whereas
        // uncatchable errors are bytecode disruption, internal errors which cannot be catched.
        if error.catchable {
            let mut block = None;

            for &try_block in &self.chunk.try_blocks {
                if try_block.0 < error.ip {
                    block = Some(try_block);
                }
            }

            match block {
                Some((_, jump_at, slot)) => {
                    let value = error.to_value(self);
                    self.fiber.ip = jump_at;
                    self.fiber.set_slot(slot, value);
                    Ok(())
                },
                None => return Err(error)
            }
        } else {
            Err(error)
        }
    }

    pub fn execute(&mut self) -> RuntimeResult<()> {
        while self.fiber.ip < self.chunk.bytes.len() {
            match self.execute_byte(self.chunk.bytes[self.fiber.ip]) {
                Ok(_) => (),
                Err(error) => self.handle_error(error)?
            }
        }

        Ok(())
    }

    pub fn execute_byte(&mut self, byte: u8) -> RuntimeResult<()> {
        use opcode::*;

        // TODO(Scientific-Guy): Instead of incrementing the ip here, increment it before calling
        // the function to execute bytes which are not even the bytecode.
        self.fiber.ip += 1;

        match byte {
            TRUE => self.push(Value::Bool(true)),
            FALSE => self.push(Value::Bool(false)),
            NULL => self.push(Value::Null),
            STRING => read_string!(self, read_u8!(self)),
            STRING_LONG => read_string!(self, read_u32!(self)),
            INT => {
                let constant = self.chunk.constants.ints[read_u8!(self) as usize];
                self.push(Value::Int(constant))
            },
            INT_LONG => {
                let constant = self.chunk.constants.ints[read_u32!(self) as usize];
                self.push(Value::Int(constant))
            },
            FLOAT => {
                let constant = self.chunk.constants.floats[read_u8!(self) as usize];
                self.push(Value::Float(constant))
            },
            FLOAT_LONG => {
                let constant = self.chunk.constants.floats[read_u32!(self) as usize];
                self.push(Value::Float(constant))
            },
            POP => {
                self.fiber.stack.pop();
            },
            ADD => {
                let (lhs, rhs) = self.fiber.pop_twice();
                let value = lhs.add(self, rhs);
                self.push(value);
            },
            SUB => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs - rhs);
            },
            MUL => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs * rhs);
            },
            DIV => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs / rhs);
            },
            REM => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs % rhs);
            },
            SET_LOCAL => {
                let slot = read_u8!(self);
                let value = self.fiber.pop();
                self.fiber.set_slot(slot, value);
            },
            SET_GLOBAL => {
                let id = read_auto!(self);
                let value = self.fiber.pop();
                self.globals.insert(id, (value, false));
            },
            SET_UPVALUE => {
                let index = read_u8!(self);
                let value = self.fiber.pop();
                self.fiber.set_upslot(index as usize, value);
            },
            SET_ATTR => {
                let (a, b, c) =  self.fiber.pop_thrice();
                return self.set_attr(a, b, c, false);
            },
            GET_LOCAL => {
                let index = read_u8!(self);
                let value = self.fiber.slot(index);
                self.push(value);
            },
            GET_UPVALUE => {
                let index = read_u8!(self);
                let value = self.fiber.upvalue(index);
                self.push(value);
            },
            GET_GLOBAL => {
                let value = match self.globals.get(&read_auto!(self)) {
                    Some(x) => x.0,
                    None => Value::Null
                };

                self.push(value);
            },
            GET_ATTR => {
                let (target, attr) = self.fiber.pop_twice();
                let result = self.resolve_attr(target, attr);
                self.push(result);
            },
            CLOSE_UPVALUE => {
                self.fiber.close_upvalue();
            },
            ITER => {
                let value = self.fiber.pop();
                let iter = self.iter_value(value);
                self.push(iter)
            },
            ITER_NEXT => {
                return self.iter_next();
            },
            EQ => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs == rhs));
            },
            NEQ => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs != rhs));
            },
            GT => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs > rhs));
            },
            LT => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs < rhs));
            },
            GTE => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs >= rhs));
            },
            LTE => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(Value::Bool(lhs <= rhs));
            },
            JUMP => {
                self.fiber.ip += read_u16!(self) as usize
            },
            JUMP_BACK => {
                self.fiber.ip -= read_u16!(self) as usize - 2;
            },
            JUMP_IF => {
                if self.fiber.pop().to_bool() {
                    self.fiber.ip += read_u16!(self) as usize;
                } else {
                    self.fiber.ip += 2;
                }
            },
            JUMP_NOT_IF => {
                if !self.fiber.pop().to_bool() {
                    self.fiber.ip += read_u16!(self) as usize;
                } else {
                    self.fiber.ip += 2;
                }
            },
            CALL => {
                let args_len = read_u8!(self);
                let target = self.fiber.pop();
                return self.call_function(target, args_len);
            },
            CALL_CHILD => {
                let args_len = read_u8!(self);
                let (parent, attr) = self.fiber.pop_twice();
                return self.call_inst_function(parent, attr, args_len);
            },
            ARRAY => {
                let count = read_auto!(self) as usize;
                let array = self.fiber.drain_strict(count);
                let ptr = self.allocate_with(array);
                self.push(Value::Array(ptr));
            },
            DICT => {
                let count = read_auto!(self) as usize;
                let length = self.fiber.stack.len();
                let actual_count = count * 2;

                if length >= actual_count {
                    let mut entries = HashMap::new();

                    for _ in 0..actual_count {
                        let value = self.fiber.pop();
                        entries.insert(self.fiber.pop(), (value, false));
                    }

                    self.fiber.stack.truncate(length - actual_count);
                    let ptr = self.allocate_with(entries);
                    self.push(Value::Dict(ptr));
                } else {
                    // Actually, this meant to be in fiber.rs file.
                    panic!("[Fiber]: Stack went {} values short in count.", actual_count)
                }
            },
            FUNC => {
                return self.function_value();
            },
            RETURN => {
                self.fiber.close_frame();
            },
            AND => {
                let (lhs, rhs) = self.fiber.pop_twice(); 
                self.push(Value::Bool(lhs.to_bool() && rhs.to_bool()));
            },
            NOT => {
                let boolean = !self.fiber.last_or_null().to_bool();
                self.push(Value::Bool(boolean));
            },
            AWAIT => {
                let target = self.fiber.pop();
                let result = self.await_value(target);
                self.push(result);
            },
            POW => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs.pow(rhs));
            },
            BITAND => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs & rhs);
            },
            BITOR => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs | rhs);
            },
            BITXOR => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs ^ rhs);
            },
            SHR => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs >> rhs);
            },
            SHL => {
                let (lhs, rhs) = self.fiber.pop_twice();
                self.push(lhs << rhs);
            },
            _ => return Err(RuntimeError::new_uncatchable(self, format!("[BytecodeReader]: Found an unknown byte {}.", byte)))
        }

        Ok(())
    }

    pub fn function_value(&mut self) -> RuntimeResult<()> {
        let start = self.fiber.ip + 2;
        self.fiber.ip += read_u16!(self) as usize;
        
        let (has_yield, max_slots, upvalue_len) = match self.chunk.bytes.get(self.fiber.ip..self.fiber.ip + 3) {
            Some(bytes) => {
                self.fiber.ip += 3;
                (bytes[0] != 0, bytes[1], bytes[2])
            },
            None => return Err(RuntimeError::new_uncatchable(self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to get [MAX_SLOTS, UPVALUES_COUNT]."))
        };

        let mut upvalues = Vec::with_capacity(upvalue_len as usize);
        let mut ip = self.fiber.ip;
        let current_frame = unsafe { &*(self.fiber.frame) };

        for _ in 0..upvalue_len {
            match self.chunk.bytes.get(ip..ip + 2) {
                Some(bytes) => {
                    ip += 2;
                    let upvalue = if bytes[0] != 0 {
                        let upvalue = Upvalue::new_open(current_frame.stack_start + bytes[1] as usize);
                        self.fiber.open_upvalues.push(upvalue);
                        upvalue
                    } else {
                        current_frame.upvalues[bytes[1] as usize]
                    };

                    upvalues.push(upvalue);
                },
                None => return Err(RuntimeError::new_uncatchable(self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to get [IS_UPVALUE, UPVALUE_SLOT]."))
            };
        }

        self.fiber.ip = ip;
        let name = read_auto!(self);
        let function = Function {
            name: self.chunk.constants.get_string(name),
            upvalues: upvalues.into_boxed_slice(),
            start,
            max_slots
        };

        let value = if has_yield {
            Value::Generator(self.allocate_with(Generator(function)))
        } else {
            Value::Function(self.allocate_with(function))
        };

        self.push(value);
        Ok(())
    }

    pub fn call_function(&mut self, target: Value, args_len: u8) -> RuntimeResult<()> {
        match target {
            Value::NativeFn(ptr) => unsafe {
                let nf = ptr.unwrap_ref();
                let offset = self.fiber.stack.len() - args_len as usize;
                let args = ptr::slice_from_raw_parts(self.fiber.stack.as_mut_ptr().add(offset), args_len as usize);
                self.fiber.new_frame(nf.name.clone(), 0, &[]);

                match (nf.func)(self, &*args) {
                    Ok(value) => {
                        self.fiber.pop_frame();
                        self.fiber.stack.set_len(offset);
                        ptr::drop_in_place(args as *mut [Value]);
                        self.push(value);
                    },
                    Err(error) => return Err(error)
                }

                Ok(())
            },
            Value::Function(ptr) => {
                let Function { name, max_slots, start, upvalues } = ptr.unwrap_ref();

                self.fiber.new_frame_with_offset(name, *max_slots, upvalues, args_len as usize);
                self.fiber.ip = *start;

                Ok(())
            },
            Value::Generator(ptr) => {
                let Generator(Function { name, max_slots, start, upvalues }) = ptr.unwrap_ref();
                let mut fiber = self.fiber.child(name, *max_slots, *start);
                fiber.new_frame_with_offset_and_ip(name, *max_slots, upvalues, args_len as usize, self.fiber.ip);
                let fiber = Value::Fiber(self.allocate_with(FiberHandle::new(fiber)));
                self.push(fiber);
                Ok(())
            },
            Value::Dict(ptr) => {
                let map = ptr.unwrap_ref();
                let self_ = match map.get(&self.constants.prototype) {
                    Some(&(Value::Dict(ptr), _)) => {
                        let instance = Instance {
                            properties: Map::new(),
                            methods: ptr
                        };

                        Value::Instance(self.allocate_with(instance))
                    },
                    _ => return Err(RuntimeError::new(self, "Cannot call object which has no prototype initiated."))
                };

                let index = self.fiber.stack.len() - args_len as usize;
                match map.get(&self.constants.init) {
                    Some(init_) => {
                        self.fiber.stack.insert(index, self_);
                        self.call_function_with_returned_value(init_.0, args_len + 1)?;
                    },
                    None => {
                        self.fiber.stack.truncate(index);
                    }
                }

                self.push(self_);
                Ok(())
            },
            value => Err(RuntimeError::new(self, format!("You cannot call a {}.", value.get_type())))
        }
    }

    pub fn call_function_with_returned_value(&mut self, target: Value, args_len: u8) -> RuntimeResult<Value> {
        match target {
            Value::NativeFn(ptr) => unsafe {
                let nf = ptr.unwrap_ref();
                let offset = self.fiber.stack.len() - args_len as usize;
                let args = ptr::slice_from_raw_parts(self.fiber.stack.as_mut_ptr().add(offset), args_len as usize);
                self.fiber.new_frame(nf.name.clone(), 0, &[]);

                match (nf.func)(self, &*args) {
                    Ok(value) => {
                        self.fiber.pop_frame();
                        self.fiber.stack.set_len(offset);
                        ptr::drop_in_place(args as *mut [Value]);
                        Ok(value)
                    },
                    Err(error) => Err(error)
                }
            },
            Value::Function(ptr) => {
                let Function { name, max_slots, start, upvalues } = ptr.unwrap_ref();
                let current_ip = self.fiber.ip;

                self.fiber.new_frame_with_offset(name, *max_slots, upvalues, args_len as usize);
                self.fiber.ip = *start;

                self.execute_till_ip_is(current_ip)?;
                Ok(Value::Null)
            },
            Value::Generator(ptr) => {
                let Generator(Function { name, max_slots, start, upvalues }) = ptr.unwrap_ref();
                let mut fiber = self.fiber.child(name, *max_slots, *start);
                fiber.new_frame_with_offset_and_ip(name, *max_slots, upvalues, args_len as usize, self.fiber.ip);
                Ok(Value::Fiber(self.allocate_with(FiberHandle::new(fiber))))
            },
            Value::Dict(ptr) => {
                let map = ptr.unwrap_ref();
                let self_ = match map.get(&self.constants.prototype) {
                    Some(&(Value::Dict(ptr), _)) => {
                        let instance = Instance {
                            properties: Map::new(),
                            methods: ptr
                        };

                        Value::Instance(self.allocate_with(instance))
                    },
                    _ => return Err(RuntimeError::new(self, "Cannot call object which has no prototype initiated."))
                };

                let index = self.fiber.stack.len() - args_len as usize;
                match map.get(&self.constants.init) {
                    Some(init_) => {
                        self.fiber.stack.insert(index - 1, self_);
                        self.call_function_with_returned_value(init_.0, args_len + 1)?;
                    },
                    None => self.fiber.stack.truncate(index)
                }

                Ok(self_)
            },
            value => Err(RuntimeError::new(self, format!("You cannot call a {}.", value.get_type())))
        }
    }

    pub fn call_inst_function(&mut self, self_: Value, attr: Value, args_len: u8) -> RuntimeResult<()> {
        macro_rules! call_instance_method {
            ($ptr:expr, $attr:ident) => {
                match attr {
                    Value::String(string) => {
                        match self.$attr.get(string.unwrap_ref()) {
                            Some(&function) => {
                                match self.call_native_method($ptr, args_len as usize, function) {
                                    Ok(value) => {
                                        self.push(value);
                                        Ok(())
                                    },
                                    Err(error) => return Err(error)
                                }
                            },
                            None => Err(RuntimeError::new(self, "You cannot call a null."))
                        }
                    },
                    value => Err(RuntimeError::new(self, format!("You cannot call a {}.", value.get_type())))
                }
            };
        }

        match self_ {
            Value::Dict(ptr) => {
                match ptr.unwrap_ref().get(&attr) {
                    Some((value, _)) => self.call_function(*value, args_len),
                    None => Err(RuntimeError::new(self, "You cannot call a null"))
                }
            },
            Value::Array(ptr) => {
                let array = ptr.unwrap_ref();

                match attr {
                    Value::Int(int) => {
                        match array.get(int as usize) {
                            Some(value) => self.call_function(*value, args_len),
                            None => Err(RuntimeError::new(self, "You cannot call a null."))
                        }
                    },
                    Value::String(string) => {
                        match self.array_methods.get(string.unwrap_ref()) {
                            Some(&function) => {
                                match self.call_native_method(ptr, args_len as usize, function) {
                                    Ok(value) => {
                                        self.push(value);
                                        Ok(())
                                    },
                                    Err(error) => return Err(error)
                                }
                            },
                            None => Err(RuntimeError::new(self, "You cannot call a null."))
                        }
                    },
                    _ => Err(RuntimeError::new(self, "Cannot call a null."))
                }
            },
            Value::Instance(ptr) => {
                let instance = ptr.unwrap_ref();
                if let Some(method) = instance.methods.unwrap_ref().get(&attr) {
                    let index = self.fiber.stack.len() - args_len as usize;
                    self.fiber.stack.insert(index, self_);
                    self.call_function(method.0, args_len + 1)
                } else if let Some(property) = instance.properties.get(&attr) {
                    self.call_function(property.0, args_len)
                } else {
                    Err(RuntimeError::new(self, "Cannot call a null."))
                }
            },
            Value::Iterator(ptr) => call_instance_method!(ptr, iterator_methods),
            Value::String(ptr) => call_instance_method!(ptr, string_methods),
            _ => Err(RuntimeError::new(self, format!("Cannot call a {}.", self_.get_type())))
        }
    }

    pub fn call_native_method<T>(
        &mut self,
        ptr: ValuePtr<T>, 
        args_len: usize, 
        method: MethodFn<T>
    ) -> RuntimeResult<Value> {
        unsafe {
            let offset = self.fiber.stack.len() - args_len;
            let args = ptr::slice_from_raw_parts(self.fiber.stack.as_ptr().add(offset), args_len as usize);
            let result = method(self, ptr.unwrap_mut(), ptr.0, &*args);

            self.fiber.stack.set_len(offset);
            self.fiber.pop_frame();
            ptr::drop_in_place(args as *mut [Value]);
            result
        }
    }

    pub fn resolve_attr(&mut self, target: Value, attr: Value) -> Value {
        match target {
            Value::Dict(ptr) => {
                match ptr.unwrap_ref().get(&attr) {
                    Some((value, _)) => *value,
                    None => Value::Null
                }
            },
            Value::Array(ptr) => {
                match attr {
                    Value::Int(int) => {
                        match ptr.unwrap_ref().get(int as usize) {
                            Some(value) => *value,
                            None => Value::Null
                        }
                    },
                    _ => Value::Null
                }
            },
            Value::Instance(ptr) => {
                let instance = ptr.unwrap_ref();
                
                match instance.properties
                        .get(&attr)
                        .or_else(|| instance.methods.unwrap_ref().get(&attr)) {
                    Some((value, _)) => *value,
                    None => Value::Null
                }
            },
            _ => Value::Null
        }
    }

    pub fn set_attr(&mut self, target: Value, attr: Value, value: Value, readonly: bool) -> RuntimeResult<()> {
        match target {
            Value::Dict(ptr) => {
                if let Some((_, true)) = ptr.unwrap_mut().insert(attr, (value, readonly)) {
                    return Err(RuntimeError::new(self, format!("Cannot assign value to property {} which is a readonly property.", attr)))
                }
            },
            Value::Array(ptr) => {
                if let Value::Int(int) = attr {
                    let array = ptr.unwrap_mut();
                    if int < 0 {
                        return Ok(());
                    }

                    let index = int as usize;
                    if index > array.len() {
                        array.resize_with(index + 1, || Value::Null);
                    }

                    array[index] = value;
                }
            },
            Value::Instance(ptr) => {
                if let Some((_, true)) = ptr.unwrap_mut().properties.insert(attr, (value, readonly)) {
                    return Err(RuntimeError::new(self, format!("Cannot assign value to property {} which is a readonly property.", attr)))
                }
            },
            _ => return Err(RuntimeError::new(self, format!("Cannot set property {} to {}.", attr, target)))
        }

        Ok(())
    }

    pub fn await_value(&mut self, value: Value) -> Value {
        match value {
            _ => value
        }
    }

    pub fn iter_value(&mut self, value: Value) -> Value {
        match value {
            Value::Array(ptr) => Value::Iterator(self.allocate_with(ValueIter::new(ptr.unwrap_ref()))),
            Value::Iterator(_) | Value::Fiber(_) => value,
            _ => Value::Iterator(self.allocate_with(ValueIter::default()))
        }
    }

    pub fn iter_next(&mut self) -> RuntimeResult<()> {
        let slot = read_u8!(self);
        let jump_index = read_u16!(self);

        match self.fiber.last().unwrap() {
            Value::Iterator(ptr) => {
                match ptr.unwrap_mut().next() {
                    Some(value) => self.fiber.set_slot(slot, value),
                    None => {
                        self.fiber.set_slot(slot, Value::Null);
                        self.fiber.ip += jump_index as usize;
                    }
                }
            },
            Value::Fiber(ptr) => {
                let current_ip = self.fiber.ip;
                self.fiber = ptr.unwrap();

                loop {
                    if self.fiber.ip < self.chunk.bytes.len() {
                        let byte = self.chunk.bytes[self.fiber.ip];

                        match byte {
                            opcode::YIELD => {
                                let value = self.fiber.pop();
                                self.fiber = self.fiber.upper();
                                self.fiber.ip = current_ip;
                                self.fiber.set_slot(slot, value);
                                self.execute_till_ip_is(current_ip - 4)?;
                                self.fiber = ptr.unwrap();
                                self.fiber.ip += 1;
                            },
                            opcode::RETURN => {
                                self.fiber = self.fiber.upper();
                                self.fiber.set_slot(slot, Value::Null);
                                self.fiber.ip = current_ip + jump_index as usize;
                            },
                            _ => {
                                if let Err(error) = self.execute_byte(byte) {
                                    self.handle_error(error)?
                                }
                            }
                        }
                    } else {
                        break;
                    }
                }
            },
            _ => ()
        }

        Ok(())
    }

    pub fn add_global(&mut self, name: &str, value: Value) {
        let constant_id = self.chunk.constants.add_string(TinyString::new(name.as_bytes()));
        self.globals.insert(constant_id, (value, true));
    }

    pub fn allocate<O>(&mut self, object: O) -> *mut u8
    where
        O: ObjectTrait
    {
        let GcLayout { layout, offset, size } = create_gc_layout::<O>();
        self.bytes_allocated += size;

        unsafe {
            let pointer = alloc::alloc::alloc(layout);
            if pointer.is_null() {
                alloc::alloc::handle_alloc_error(layout);
            }

            ptr::write(pointer as *mut GcHeader, GcHeader(false));
            ptr::write(pointer.add(offset) as *mut O, object);
            self.objects.push(GcHandle(pointer as *const GcHeader, O::KIND));

            #[cfg(feature = "stress_gc")]
            self.collect_garbage();

            #[cfg(not(feature = "stress_gc"))]
            if self.bytes_allocated >= self.next_gc {
                self.collect_garbage();
            }

            pointer
        }
    }

    pub fn allocate_with<O>(&mut self, object: O) -> ValuePtr<O>
    where
        O: ObjectTrait
    {
        ValuePtr::new_unchecked(self.allocate(object))
    }

    pub fn allocate_string_bytes(&mut self, bytes: &[u8]) -> ValuePtr<TinyString> {
        let string = TinyString::new(bytes);
        ValuePtr::new_unchecked(self.allocate(string))
    }

    pub fn allocate_static_str(&mut self, string: &str) -> ValuePtr<TinyString> {
        ValuePtr::new_unchecked(self.allocate(TinyString::new(string.as_bytes())))
    }

    pub fn allocate_string(&mut self, string: String) -> ValuePtr<TinyString> {
        ValuePtr::new_unchecked(self.allocate(TinyString::new(string.as_bytes())))
    }

    pub fn collect_garbage(&mut self) {
        fn mark_value(value: &Value) {
            match value {
                Value::String(ptr) => ptr.mark(),
                Value::Array(ptr) => {
                    for value in ptr.unwrap_ref() {
                        mark_value(value);
                    }

                    ptr.mark()
                },
                Value::Dict(ptr) => {
                    for (key, value) in ptr.unwrap_ref() {
                        mark_value(key);
                        mark_value(&value.0)
                    }

                    ptr.mark()
                },
                Value::Function(ptr) => {
                    for upvalue in &*ptr.unwrap_ref().upvalues {
                        if let UpvalueState::Closed(value) = upvalue.state() {
                            mark_value(&value);
                        }
                    }

                    ptr.mark()
                },
                Value::NativeFn(ptr) => ptr.mark(),
                Value::Iterator(ptr) => {
                    for value in &ptr.unwrap_ref().to_vec() {
                        mark_value(value);
                    }

                    ptr.mark()
                },
                Value::Instance(ptr) => {
                    let instance = ptr.unwrap_ref();

                    for (key, value) in &instance.properties {
                        mark_value(key);
                        mark_value(&value.0);
                    }

                    if !instance.methods.marked() {
                        for (key, value) in instance.methods.unwrap_ref() {
                            mark_value(key);
                            mark_value(&value.0);
                        }
    
                        instance.methods.mark();
                    }

                    ptr.mark();
                },
                // Not completed yet
                _ => ()
            }
        }

        unsafe {
            for value in &self.fiber.stack {
                mark_value(value);
            }

            for (_, value) in &self.globals {
                mark_value(&value.0);
            }

            self.fiber.collect_upvalues(mark_value);

            macro_rules! mark_vm_constants {
                ($($attr:ident)+) => {
                    $(mark_value(&self.constants.$attr);)+
                };
            }

            mark_vm_constants! {
                init prototype rid pid stdin stdout stderr cwd cmd env
                __listeners __time __date
            }

            self.constants.process_prototype.mark();

            // Clear all unreacable objects
            let mut index = 0;

            for handle in self.objects.clone() {
                if let (true, size) = handle.dealloc_if_unreachable() {
                    self.bytes_allocated -= size;
                    self.objects.remove(index);
                    continue;
                }

                index += 1;
            }
        }

        self.next_gc = self.bytes_allocated * 2;
    }

    fn push(&mut self, value: Value) {
        self.fiber.stack.push(value);
    }

    fn execute_till_ip_is(&mut self, ip: usize) -> RuntimeResult<Value> {
        while self.fiber.ip < self.chunk.bytes.len() {
            // Ip would reach paticular ip if the RETURN opcode appeared
            if self.fiber.ip == ip {
                return Ok(self.fiber.pop());
            }

            if let Err(error) = self.execute_byte(self.chunk.bytes[self.fiber.ip]) {
                self.handle_error(error)?;
            }
        }

        Ok(Value::Null)
    }

}

fn flags_has_permission(flags: &Flags, string: &str) -> bool {
    flags.contains_key(&TinyString::new(&[b"use-", string.as_bytes()].concat()))
}

fn permission_from_flags(flags: &Flags) -> Permissions {
    Permissions {
        read: flags_has_permission(flags, "read"),
        write: flags_has_permission(flags, "write"),
        memory: flags_has_permission(flags, "memory"),
        process: flags_has_permission(flags, "process"),
        _unsafe: flags.contains_key(&TinyString::new(b"unsafe"))
    }
}

fn load_constants(vm: &mut Vm) -> Constants {
    macro_rules! constants {
        ($($name:ident)+) => {
            Constants {
                $($name: Value::String(vm.allocate_static_str(stringify!($name))),)+
                process_prototype: ValuePtr::default()
            }
        };
    }

    constants! { 
        init prototype rid pid stdin stdout stderr cwd cmd env
        __listeners __time __date
    }
}