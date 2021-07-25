extern crate alloc;

use std::{ptr};
use std::path::PathBuf;
use std::convert::TryInto;
use std::collections::HashMap;
use super::memory::*;
use crate::{
    Value, RuntimeResult, RuntimeError, ObjectTrait, Chunk, TinyString, Function, Upvalue, 
    UpvalueState, ValueIter, ValuePtr, Instance, Map, Fiber, Promise, PromiseState, 
    ResourceTable, opcode, core
};

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

macro_rules! read_string {
    ($self:expr, $index:expr) => {{
        let bytes = match $self.chunk.constants.strings.get($index as usize) {
            Some(string) => string.as_bytes(),
            None => &[]
        };

        #[allow(mutable_borrow_reservation_conflict)]
        let constant = $self.allocate_value_ptr(TinyString::new(bytes));
        $self.push(Value::String(constant));
    }};
}

macro_rules! pop_two {
    ($self:expr) => {
        match $self.fiber.pop_two() {
            Some(popped) => popped,
            _ => return Err(RuntimeError::new_uncatchable($self, "[VM]: Stack Manipulation Failed. Expected stack length with minimum size as 2 to pop two elements."))
        }
    };
}

pub type MethodFn<T> = fn (&mut Vm, &mut T, *const u8, &[Value]) -> RuntimeResult<Value>;
pub type MethodMap<T> = HashMap<TinyString, MethodFn<T>>;
type Flags = HashMap<TinyString, TinyString>;

#[derive(Debug, Clone, Copy, Default)]
pub struct Permissions {
    pub read: bool,
    pub write: bool,
    pub memory: bool,
    pub child_process: bool,
    pub unsafe_libs: bool
}

#[derive(Default)]
pub struct Constants {
    init: Value,
    pub(super) prototype: Value,
    pub(super) __listeners: Value,
    pub(super) __time: Value,
    pub(super) __date: Value,
    pub(super) __promise: Value,
    pub(super) rid: Value,
    pub(super) pid: Value,
    pub(super) stdin: Value,
    pub(super) stdout: Value,
    pub(super) stderr: Value,
    pub(super) cwd: Value,
    pub(super) cmd: Value,
    pub(super) env: Value,
    pub(super) process_prototype: ValuePtr<Map>,
    pub(super) promise_handler_prototype: ValuePtr<Map>
}

pub struct Vm {
    pub(crate) chunk: Chunk,
    pub(crate) permissions: Permissions,
    pub(crate) bytes_allocated: usize,
    pub(crate) next_gc: usize,
    pub(crate) objects: Vec<GcHandle>,
    pub(crate) path: PathBuf,
    pub(crate) fiber: Fiber,
    pub(crate) constants: Constants,
    pub iterator_methods: MethodMap<ValueIter>,
    pub string_methods: MethodMap<TinyString>,
    pub array_methods: MethodMap<Vec<Value>>,
    pub promise_methods: MethodMap<Promise>,
    pub globals: HashMap<u32, (Value, bool)>,
    pub resource_table: ResourceTable,
    flags: Flags,
    open_upvalues: Vec<Upvalue>
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
            fiber: Fiber::new("runtime", chunk.bytes[0], 1),
            chunk,
            flags,
            path,
            permissions,
            array_methods: MethodMap::new(),
            string_methods: MethodMap::new(),
            promise_methods: MethodMap::new(),
            iterator_methods: MethodMap::new(),
            constants: Constants::default(),
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
            resource_table: ResourceTable::default(),
            next_gc: u16::MAX as usize,
            bytes_allocated: 0,
            objects: Vec::new(),
        };

        macro_rules! constants {
            ($($name:ident)+) => {
                Constants {
                    $($name: Value::String(vm.allocate_static_str(stringify!($name))),)+
                    process_prototype: ValuePtr::default(),
                    promise_handler_prototype: ValuePtr::default()
                }
            };
        }

        vm.constants = constants! { 
            init prototype rid pid stdin stdout stderr cwd cmd env
            __listeners __time __date __promise
        };

        core::init(&mut vm);

        vm.execute()?;
        
        Ok(vm)
    }

    pub fn handle_error(&mut self, error: RuntimeError) -> RuntimeResult<()> {
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

        // Increment the ip whenever executing the byte
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
                let (lhs, rhs) = pop_two!(self);
                let value = lhs.add(self, rhs);
                self.push(value);
            },
            SUB => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs - rhs);
            },
            MUL => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs * rhs);
            },
            DIV => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs / rhs);
            },
            REM => {
                let (lhs, rhs) = pop_two!(self);
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
                return match self.fiber.pop_three() {
                    Some((a, b, c)) => self.set_attr(a, b, c, false),
                    _ => Err(RuntimeError::new_uncatchable(self, "[VM]: Stack Manipulation Failed. Expected stack length with minimum size as 3."))
                }
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
                let (target, attr) = pop_two!(self);
                let result = self.resolve_attr(target, attr);
                self.push(result);
            },
            CLOSE_UPVALUE => {
                let closed_index = self.fiber.stack.len() - 1;
                let closed = self.fiber.pop();
                let mut retain = self.open_upvalues.len();

                for upvalue in self.open_upvalues.iter().rev() {
                    match upvalue.state() {
                        UpvalueState::Open(index) if index == closed_index => upvalue.close(closed),
                        UpvalueState::Closed(_) => (),
                        _ => break
                    }

                    retain -= 1;
                }

                self.open_upvalues.truncate(retain);
            },
            ITER => {
                let iter = self.fiber.pop().into_iter();
                let ptr = self.allocate_value_ptr(iter);
                self.push(Value::Iterator(ptr))
            },
            ITER_NEXT => {
                let slot = read_u8!(self);
                let jump_index = read_u16!(self);

                match self.fiber.last().unwrap().iter_next() {
                    Some(value) => self.fiber.set_slot(slot, value),
                    None => {
                        self.fiber.set_slot(slot, Value::Null);
                        self.fiber.ip += jump_index as usize;
                    }
                }
            },
            EQ => {
                let (lhs, rhs) = pop_two!(self);
                self.push(Value::Bool(lhs == rhs));
            },
            NEQ => {
                let (lhs, rhs) = pop_two!(self);
                self.push(Value::Bool(lhs != rhs));
            },
            GT => {
                let (lhs, rhs) = pop_two!(self);
                self.push(Value::Bool(lhs > rhs));
            },
            LT => {
                let (lhs, rhs) = pop_two!(self);
                self.push(Value::Bool(lhs < rhs));
            },
            GTE => {
                let (lhs, rhs) = pop_two!(self);
                self.push(Value::Bool(lhs >= rhs));
            },
            LTE => {
                let (lhs, rhs) = pop_two!(self);
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
                let (parent, attr) = pop_two!(self);
                return self.call_inst_function(parent, attr, args_len);
            },
            ARRAY => {
                let count = read_auto!(self) as usize;

                if self.fiber.stack.len() >= count {
                    let array = self.fiber.drain(count);
                    let ptr = self.allocate_value_ptr(array);
                    self.push(Value::Array(ptr));
                } else {
                    return Err(RuntimeError::new(self, format!("[VM]: Stack Manipulation Failed. Expected stack length with minimum size as {}", count)))
                }
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
                    let ptr = self.allocate_value_ptr(entries);
                    self.push(Value::Dict(ptr));
                } else {
                    return Err(RuntimeError::new(self, format!("[VM]: Stack Manipulation Failed. Expected stack length with minimum size as {}", actual_count)))
                }
            },
            FUNC => {
                let start = self.fiber.ip + 2;
                self.fiber.ip += read_u16!(self) as usize;
                
                let (max_slots, upvalue_len) = match self.chunk.bytes.get(self.fiber.ip..self.fiber.ip + 2) {
                    Some(bytes) => {
                        self.fiber.ip += 2;
                        (bytes[0], bytes[1])
                    },
                    None => return Err(RuntimeError::new_uncatchable(self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to get [MAX_SLOTS, UPVALUES_COUNT]."))
                };

                let mut upvalues = Vec::with_capacity(upvalue_len as usize);
                let mut ip = self.fiber.ip;
                let current_frame = self.fiber.frame();

                for _ in 0..upvalue_len {
                    match self.chunk.bytes.get(ip..ip + 2) {
                        Some(bytes) => {
                            ip += 2;
                            let upvalue = if bytes[0] != 0 {
                                let upvalue = Upvalue::new_open(current_frame.stack_start + bytes[1] as usize);
                                self.open_upvalues.push(upvalue);
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
                let ptr = self.allocate_value_ptr(
                    Function {
                        name: self.chunk.constants.get_string(name),
                        upvalues: upvalues.into_boxed_slice(),
                        start,
                        max_slots
                    }
                );

                self.push(Value::Function(ptr))
            },
            RETURN => {
                let frame = self.fiber.pop_frame();
                let mut retain = self.open_upvalues.len();

                for upvalue in self.open_upvalues.iter().rev() {
                    if let UpvalueState::Open(index) = upvalue.state() {
                        if frame.stack_start <= index {
                            upvalue.close(self.fiber.stack[index])
                        } else { 
                            break 
                        }
                    }

                    retain -= 1;
                }

                self.fiber.stack.drain(frame.stack_start..);
                self.open_upvalues.truncate(retain);
                self.fiber.ip = frame.ip;
            },
            AND => {
                let (lhs, rhs) = pop_two!(self); 
                self.push(Value::Bool(lhs.to_bool() && rhs.to_bool()));
            },
            NOT => {
                let boolean = !self.fiber.last_or_null().to_bool();
                self.push(Value::Bool(boolean));
            },
            AWAIT => {
                let target = self.fiber.pop();
                let result = if let Value::Promise(ptr) = target {
                    match self.await_(ptr.unwrap_mut()) {
                        Ok(value) => value,
                        Err(error) => return Err(error)
                    }
                } else { target };

                self.push(result);
            },
            POW => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs.pow(rhs));
            },
            BITAND => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs & rhs);
            },
            BITOR => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs | rhs);
            },
            BITXOR => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs ^ rhs);
            },
            SHR => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs >> rhs);
            },
            SHL => {
                let (lhs, rhs) = pop_two!(self);
                self.push(lhs << rhs);
            },
            _ => return Err(RuntimeError::new_uncatchable(self, format!("[BytecodeReader]: Found an unknown byte {}.", byte)))
        }

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
                let stack_start = self.fiber.stack.len() - args_len as usize;

                self.fiber.new_frame_with_offset(name, *max_slots, upvalues, args_len as usize);
                self.fiber.ip = *start;

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

                        Value::Instance(self.allocate_value_ptr(instance))
                    },
                    _ => return Err(RuntimeError::new(self, "Cannot call object which has no prototype initiated."))
                };

                match map.get(&self.constants.init) {
                    Some(init_) => {
                        self.fiber.stack.insert(self.fiber.stack.len() - args_len as usize, self_);
                        match self.call_function_with_returned_value(init_.0, args_len + 1) {
                            Ok(_) => {
                                self.push(self_);
                                Ok(())
                            },
                            Err(error) => Err(error)
                        }
                    },
                    None => {
                        self.fiber.stack.truncate(self.fiber.stack.len() - args_len as usize);
                        self.push(self_);
                        Ok(())
                    }
                }
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
                let stack_start = self.fiber.stack.len() - args_len as usize;
                let current_ip = self.fiber.ip;

                self.fiber.new_frame_with_offset(name, *max_slots, upvalues, args_len as usize);
                self.fiber.ip = *start;

                while self.fiber.ip < self.chunk.bytes.len() {
                    // Ip would reach current ip if the RETURN opcode appeared
                    if self.fiber.ip == current_ip {
                        return Ok(self.fiber.pop());
                    }

                    match self.execute_byte(self.chunk.bytes[self.fiber.ip]) {
                        Ok(_) => (),
                        Err(error) => self.handle_error(error)?
                    }
                }

                Ok(Value::Null)
            },
            Value::Dict(ptr) => {
                let map = ptr.unwrap_ref();
                let self_ = match map.get(&self.constants.prototype) {
                    Some(&(Value::Dict(ptr), _)) => {
                        let instance = Instance {
                            properties: Map::new(),
                            methods: ptr
                        };

                        Value::Instance(self.allocate_value_ptr(instance))
                    },
                    _ => return Err(RuntimeError::new(self, "Cannot call object which has no prototype initiated."))
                };

                match map.get(&self.constants.init) {
                    Some(init_) => {
                        self.fiber.stack.insert((self.fiber.stack.len() - args_len as usize) - 1, self_);
                        match self.call_function_with_returned_value(init_.0, args_len + 1) {
                            Ok(_) => Ok(self_),
                            Err(error) => Err(error)
                        }
                    },
                    None => {
                        self.fiber.stack.truncate(self.fiber.stack.len() - args_len as usize);
                        Ok(self_)
                    }
                }
            },
            value => Err(RuntimeError::new(self, format!("You cannot call a {}.", value.get_type())))
        }
    }

    pub fn call_inst_function(&mut self, self_: Value, attr: Value, args_len: u8) -> RuntimeResult<()> {
        macro_rules! inst_method {
            ($ptr:expr, $attr:ident) => {
                match attr {
                    Value::String(string) => {
                        let name = string.unwrap_ref();

                        match self.$attr.get(name) {
                            Some(&function) => {
                                match self.call_native_method(name.clone(), $ptr, args_len as usize, function) {
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
                        let string = string.unwrap_ref();

                        match self.array_methods.get(string) {
                            Some(&function) => {
                                match self.call_native_method(string.clone(), ptr, args_len as usize, function) {
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
                    self.fiber.stack.insert(self.fiber.stack.len() - args_len as usize, self_);
                    self.call_function(method.0, args_len + 1)
                } else if let Some(property) = instance.properties.get(&attr) {
                    self.call_function(property.0, args_len)
                } else {
                    Err(RuntimeError::new(self, "Cannot call a null."))
                }
            },
            Value::Iterator(ptr) => inst_method!(ptr, iterator_methods),
            Value::String(ptr) => inst_method!(ptr, string_methods),
            Value::Promise(ptr) => inst_method!(ptr, promise_methods),
            _ => Err(RuntimeError::new(self, format!("Cannot call a {}.", self_.get_type())))
        }
    }

    pub fn call_native_method<T>(
        &mut self, 
        name: TinyString, 
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
                match attr {
                    Value::Int(int) => {
                        let array = ptr.unwrap_mut();
                        if int < 0 {
                            return Ok(());
                        }

                        let index = int as usize;
                        if index > array.len() {
                            array.resize_with(index + 1, || Value::Null);
                        }

                        array[index] = value;
                    },
                    _ => ()
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

    pub fn await_(&mut self, promise: &mut Promise) -> RuntimeResult<Value> {
        loop {
            match promise.state {
                PromiseState::Pending => (),
                PromiseState::Fulfilled(result) => return Ok(result),
                PromiseState::Rejected(result) => return Err(RuntimeError::new(self, result))
            }
        }
    }

    pub fn add_global(&mut self, name: &str, value: Value) {
        let constant_id = self.chunk.constants.add_string(TinyString::new(name.as_bytes()));
        self.globals.insert(constant_id, (value, true));
    }

    pub fn allocate<O: ObjectTrait>(&mut self, object: O) -> *mut u8 {
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

    pub fn allocate_value_ptr<O: ObjectTrait>(&mut self, object: O) -> ValuePtr<O> {
        ValuePtr::new_unchecked(self.allocate(object))
    }

    pub fn allocate_str_bytes(&mut self, bytes: &[u8]) -> ValuePtr<TinyString> {
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
                // Not completed here
                Value::Promise(ptr) => ptr.mark(),
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
                __listeners __time __date __promise
            }

            self.constants.process_prototype.mark();
            self.constants.promise_handler_prototype.mark();

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

}

fn flags_has_permission(flags: &Flags, string: &str) -> bool {
    flags.contains_key(&TinyString::new(&[b"use-", string.as_bytes()].concat()))
}

fn permission_from_flags(flags: &Flags) -> Permissions {
    Permissions {
        read: flags_has_permission(flags, "read"),
        write: flags_has_permission(flags, "write"),
        memory: flags_has_permission(flags, "memory"),
        child_process: flags_has_permission(flags, "process"),
        unsafe_libs: flags.contains_key(&TinyString::new(b"unsafe"))
    }
}