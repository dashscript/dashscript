extern crate alloc;

use std::rc::Rc;
use std::{mem, ptr};
use std::any::TypeId;
use std::path::PathBuf;
use std::convert::TryInto;
use std::collections::{HashMap, BTreeMap};
use super::memory::*;
use crate::{
    Value, RuntimeResult, RuntimeError, ObjectTrait, Chunk, TinyString, Function, Upvalue, 
    UpvalueState, ValueIter, ValuePtr, Instance, Map, Resource, IoResource, ResourceKind,
    opcode, core
};

macro_rules! read_u8 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.ip) {
            Some(byte) => {
                $self.ip += 1;
                *byte
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 1 more byte to form u8."))
        }
    };
}

macro_rules! read_u16 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.ip..$self.ip + 2) {
            Some(byte) => {
                $self.ip += 2;
                u16::from_le_bytes(byte.try_into().unwrap())
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to form u16."))
        }
    };
}

macro_rules! read_u32 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.ip..$self.ip + 4) {
            Some(byte) => {
                $self.ip += 4;
                u32::from_le_bytes(byte.try_into().unwrap())
            },
            None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 4 more bytes to form u32."))
        }
    };
}

macro_rules! read_auto {
    ($self:expr) => {
        match $self.chunk.bytes[$self.ip] {
            0 => {
                $self.ip += 1;
                match $self.chunk.bytes.get($self.ip) {
                    Some(byte) => {
                        $self.ip += 1;
                        *byte as u32
                    },
                    None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 1 more byte to form u8."))
                }
            },
            1 => {
                $self.ip += 1;
                match $self.chunk.bytes.get($self.ip..$self.ip + 4) {
                    Some(byte) => {
                        $self.ip += 4;
                        u32::from_le_bytes(byte.try_into().unwrap())
                    },
                    None => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 4 more bytes to form u32."))
                }
            },
            _ => return Err(RuntimeError::new_uncatchable($self, "[BytecodeReader]: Corrupted Bytecode. Expected 1 more byte as either 1[LONG] or 0[SHORT]."))
        }
    };
}

macro_rules! pop_two {
    ($self:expr) => {
        match ($self.stack.pop(), $self.stack.pop()) {
            (Some(rhs), Some(lhs)) => (lhs, rhs),
            _ => return Err(RuntimeError::new_uncatchable($self, "[VM]: Stack Manipulation Failed. Expected stack length with minimum size as 2 to pop two elements."))
        }
    };
}

macro_rules! handle_error {
    ($error:expr, $self:expr, $vm:expr) => {
        if $error.catchable {
            let mut block = None;
            for &try_block in &$vm.chunk.try_blocks {
                if try_block.0 < $self.ip {
                    block = Some(try_block);
                }
            }

            match block {
                Some((_, jump_at, slot)) => {
                    let value = $error.to_value($vm);
                    $self.ip = jump_at;
                    $vm.add_local(slot as usize, value);
                    Ok(())
                },
                None => return Err($error)
            }
        } else {
            Err($error)
        }
    };
}

pub type MethodFn<T> = fn (&mut Vm, &mut T, *const u8, &[Value]) -> RuntimeResult<Value>;
pub type MethodMap<T> = HashMap<TinyString, MethodFn<T>>;

#[derive(Debug, Clone, Copy, Default)]
pub struct Permissions {
    pub read: bool,
    pub write: bool,
    pub memory: bool,
    pub child_process: bool,
    pub unsafe_libs: bool
}

#[derive(Debug, Clone, Default)]
pub struct CallFrame {
    name: TinyString,
    upvalues: Vec<Upvalue>,
    stack_start: usize,
    max_slots: u8,
    ip: usize // This would be 0 if the call frame belongs to a native function
}

impl CallFrame {
    pub fn name(&self) -> TinyString {
        self.name.clone()
    }
}

#[derive(Default)]
pub struct VmConstants {
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

#[derive(Default)]
pub struct Vm {
    pub(crate) chunk: Chunk,
    pub(crate) ip: usize,
    pub(crate) stack: Vec<Value>,
    pub(crate) globals: HashMap<u32, (Value, bool)>,
    pub(crate) bytes_allocated: usize,
    pub(crate) permissions: Permissions,
    pub(crate) next_gc: usize,
    pub(crate) objects: Vec<GcHandle>,
    pub(crate) path: PathBuf,
    pub(crate) iterator_methods: MethodMap<ValueIter>,
    pub(crate) string_methods: MethodMap<TinyString>,
    pub(crate) array_methods: MethodMap<Vec<Value>>,
    pub(super) call_stack: Vec<CallFrame>,
    pub(super) constants: VmConstants,
    pub(super) resource_table: BTreeMap<u32, Rc<dyn Resource>>,
    next_rid: u32,
    flags: HashMap<TinyString, TinyString>,
    open_upvalues: Vec<Upvalue>
}

impl Vm {

    // The pointer of null in pointers of vm by default
    pub const NULL_POINTER: u32 = 0;

    pub fn new(chunk: Chunk, flags: HashMap<TinyString, TinyString>, path: PathBuf) -> RuntimeResult<Self> {
        let mut vm = Self {
            chunk,
            flags,
            path,
            call_stack: vec![CallFrame { name: TinyString::new(b"runtime"), ..Default::default() }],
            next_gc: u16::MAX as usize,
            ..Default::default()
        };

        macro_rules! vm_constants {
            ($($name:ident)+) => {
                VmConstants {
                    $($name: Value::String(vm.allocate_static_str(stringify!($name))),)+
                    process_prototype: ValuePtr::default()
                }
            };
        }

        vm.constants = vm_constants! { 
            init prototype rid pid stdin stdout stderr cwd cmd env
            __listeners __time __date 
        };

        vm.init_permissions();
        core::init(&mut vm);
        vm.execute()?;
        
        Ok(vm)
    }

    pub fn init_permissions(&mut self) {
        self.permissions = Permissions {
            read: self.has_permission("read"),
            write: self.has_permission("write"),
            memory: self.has_permission("memory"),
            child_process: self.has_permission("child-process"),
            unsafe_libs: self.flags.contains_key(&TinyString::new(b"unsafe"))
        };
    }

    pub fn handle_error(&mut self, error: RuntimeError) -> RuntimeResult<()> {
        handle_error!(error, self, self)
    }

    pub fn execute(&mut self) -> RuntimeResult<()> {
        self.stack.resize_with(self.chunk.bytes[self.ip] as usize, Default::default);
        self.ip += 1;

        while self.ip < self.chunk.bytes.len() {
            match self.execute_byte(self.chunk.bytes[self.ip]) {
                Ok(_) => (),
                Err(error) => self.handle_error(error)?
            }
        }

        Ok(())
    }

    pub fn execute_byte(&mut self, byte: u8) -> RuntimeResult<()> {
        use opcode::*;
        self.ip += 1;

        match byte {
            TRUE => self.stack.push(Value::Bool(true)),
            FALSE => self.stack.push(Value::Bool(false)),
            NULL => self.stack.push(Value::Null),
            STRING => {
                let bytes = match self.chunk.constants.strings.get(read_u8!(self) as usize) {
                    Some(string) => string.as_bytes(),
                    None => &[]
                };

                #[allow(mutable_borrow_reservation_conflict)]
                let constant = self.allocate_value_ptr(TinyString::new(bytes));
                self.stack.push(Value::String(constant));
            },
            STRING_LONG => {
                let bytes = match self.chunk.constants.strings.get(read_u32!(self) as usize) {
                    Some(string) => string.as_bytes(),
                    None => &[]
                };
        
                #[allow(mutable_borrow_reservation_conflict)]
                let constant = self.allocate_value_ptr(TinyString::new(bytes));
                self.stack.push(Value::String(constant));
            },
            INT => {
                let constant = self.chunk.constants.ints[read_u8!(self) as usize];
                self.stack.push(Value::Int(constant))
            },
            INT_LONG => {
                let constant = self.chunk.constants.ints[read_u32!(self) as usize];
                self.stack.push(Value::Int(constant))
            },
            FLOAT => {
                let constant = self.chunk.constants.floats[read_u8!(self) as usize];
                self.stack.push(Value::Float(constant))
            },
            FLOAT_LONG => {
                let constant = self.chunk.constants.floats[read_u32!(self) as usize];
                self.stack.push(Value::Float(constant))
            },
            POP => {
                self.stack.pop();
            },
            ADD => {
                let (lhs, rhs) = pop_two!(self);
                let value = lhs.add(self, rhs);
                self.stack.push(value);
            },
            SUB => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs - rhs);
            },
            MUL => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs * rhs);
            },
            DIV => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs / rhs);
            },
            REM => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs % rhs);
            },
            SET_LOCAL => {
                let slot = read_u8!(self);
                let value = self.stack.pop().unwrap_or_default();
                self.add_local(slot as usize, value);
            },
            SET_GLOBAL => {
                let id = read_auto!(self);
                self.globals.insert(id, (self.stack.pop().unwrap_or_default(), false));
            },
            SET_UPVALUE => {
                let upval = read_u8!(self);
                let value = self.stack.pop().unwrap();
                let frame = self.call_stack.last_mut().unwrap();
                frame.upvalues[upval as usize].close(value);
            },
            SET_ATTR => {
                return match (self.stack.pop(), self.stack.pop(), self.stack.pop()) {
                    (Some(a), Some(b), Some(c)) => self.set_attr(b, a, c, false),
                    _ => return Err(RuntimeError::new_uncatchable(self, "[VM]: Stack Manipulation Failed. Expected stack length with minimum size as 3."))
                }
            },
            GET_LOCAL => {
                let frame = self.call_stack.last().unwrap();
                let local = self.stack[frame.stack_start + read_u8!(self) as usize];
                self.stack.push(local);
            },
            GET_UPVALUE => {
                let slot = read_u8!(self);
                let value = match self.call_stack.last().unwrap().upvalues[slot as usize].state() {
                    UpvalueState::Closed(value) => value,
                    UpvalueState::Open(index) => self.stack[index]
                };

                self.stack.push(value)
            },
            GET_GLOBAL => {
                match self.globals.get(&read_auto!(self)) {
                    Some((global, _)) => self.stack.push(*global),
                    None => self.stack.push(Value::Null)
                }
            },
            GET_ATTR => {
                let (target, attr) = pop_two!(self);
                let result = self.resolve_attr(target, attr);
                self.stack.push(result);
            },
            CLOSE_UPVALUE => {
                let closed_index = self.stack.len() - 1;
                let closed = self.stack.pop().unwrap();
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
                let value = self.stack.pop().unwrap();
                let ptr = self.allocate_value_ptr(value.into_iter());
                self.stack.push(Value::Iterator(ptr))
            },
            ITER_NEXT => {
                let slot = read_u8!(self);
                let jump_index = read_u16!(self);

                match self.stack.last().unwrap().iter_next() {
                    Some(value) => self.stack[self.call_stack.last().unwrap().stack_start + slot as usize] = value,
                    None => self.ip += jump_index as usize
                }
            },
            EQ => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs == rhs));
            },
            NEQ => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs != rhs));
            },
            GT => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs > rhs));
            },
            LT => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs < rhs));
            },
            GTE => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs >= rhs));
            },
            LTE => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(Value::Bool(lhs <= rhs));
            },
            JUMP => {
                self.ip += read_u16!(self) as usize
            },
            JUMP_BACK => {
                self.ip -= read_u16!(self) as usize - 2;
            },
            JUMP_IF => {
                if self.stack.last().unwrap_or_default().to_bool() {
                    self.ip += read_u16!(self) as usize;
                } else {
                    self.ip += 2;
                }
            },
            JUMP_NOT_IF => {
                if !self.stack.pop().unwrap_or_default().to_bool() {
                    self.ip += read_u16!(self) as usize;
                } else {
                    self.ip += 2;
                }
            },
            CALL => {
                let args_len = read_u8!(self);
                let target = self.stack.pop().unwrap();
                return self.call_function(target, args_len);
            },
            CALL_CHILD => {
                let args_len = read_u8!(self);
                let (parent, attr) = pop_two!(self);
                return self.call_inst_function(parent, attr, args_len);
            },
            ARRAY => {
                let array_len = read_auto!(self) as usize;
                let len = self.stack.len();
                let offset_ip = len - array_len;

                if self.stack.len() >= array_len {
                    let array = self.stack[offset_ip..].to_vec();
                    self.stack.truncate(offset_ip);

                    let ptr = self.allocate_value_ptr(array);
                    self.stack.push(Value::Array(ptr));
                } else {
                    return Err(RuntimeError::new(self, format!("[VM]: Stack Manipulation Failed. Expected stack length with minimum size as {}", array_len)))
                }
            },
            DICT => {
                let actual_len = read_auto!(self) as usize;
                let entries_len = actual_len * 2;
                let len = self.stack.len();
                let offset_ip = len - entries_len;

                if self.stack.len() >= entries_len {
                    let mut entries = HashMap::new();

                    for _ in 0..actual_len {
                        let value = self.stack.pop().unwrap();
                        entries.insert(self.stack.pop().unwrap(), (value, false));
                    }

                    self.stack.truncate(offset_ip);
                    let ptr = self.allocate_value_ptr(entries);
                    self.stack.push(Value::Dict(ptr));
                } else {
                    return Err(RuntimeError::new(self, format!("[VM]: Stack Manipulation Failed. Expected stack length with minimum size as {}", entries_len)))
                }
            },
            FUNC => {
                let start = self.ip + 2;
                self.ip += read_u16!(self) as usize;
                
                let (max_slots, upvalue_len, is_async) = match self.chunk.bytes.get(self.ip..self.ip + 3) {
                    Some(bytes) => {
                        self.ip += 3;
                        (bytes[0], bytes[1], bytes[2] != 0)
                    },
                    None => return Err(RuntimeError::new_uncatchable(self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to get [MAX_SLOTS, UPVALUES_COUNT, IS_ASYNC[bool]]."))
                };

                let mut upvalues = Vec::with_capacity(upvalue_len as usize);
                let current_frame = self.call_stack.last().unwrap();

                for _ in 0..upvalue_len {
                    match self.chunk.bytes.get(self.ip..self.ip + 2) {
                        Some(bytes) => {
                            let upvalue = if bytes[0] != 0 {
                                let upvalue = Upvalue::new_open(current_frame.stack_start + bytes[1] as usize);
                                self.open_upvalues.push(upvalue);
                                upvalue
                            } else {
                                current_frame.upvalues[bytes[1] as usize]
                            };

                            upvalues.push(upvalue);
                            self.ip += 2;
                        },
                        None => return Err(RuntimeError::new_uncatchable(self, "[BytecodeReader]: Corrupted Bytecode. Expected 2 more bytes to get [IS_UPVALUE, UPVALUE_SLOT]."))
                    };
                }

                let name = read_auto!(self);
                let ptr = self.allocate_value_ptr(
                    Function {
                        name: self.chunk.constants.get_string(name),
                        upvalues: upvalues.into_boxed_slice(),
                        start,
                        max_slots,
                        is_async
                    }
                );

                self.stack.push(Value::Function(ptr))
            },
            RETURN => {
                let frame = self.call_stack.pop().unwrap();
                let mut retain = self.open_upvalues.len();

                for upvalue in self.open_upvalues.iter().rev() {
                    match upvalue.state() {
                        UpvalueState::Open(index) => {
                            if frame.stack_start <= index {
                                upvalue.close(self.stack[index])
                            } else { break }
                        },
                        UpvalueState::Closed(_) => ()
                    }

                    retain -= 1;
                }

                self.stack.drain(frame.stack_start..frame.stack_start + frame.max_slots as usize);
                self.open_upvalues.truncate(retain);
                self.ip = frame.ip;
            },
            AND => {
                let (lhs, rhs) = pop_two!(self); 
                self.stack.push(Value::Bool(lhs.to_bool() && rhs.to_bool()));
            },
            NOT => {
                let boolean = !self.stack.last().unwrap_or_default().to_bool();
                self.stack.push(Value::Bool(boolean));
            },
            POW => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs.pow(rhs));
            },
            BITAND => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs & rhs);
            },
            BITOR => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs | rhs);
            },
            BITXOR => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs ^ rhs);
            },
            SHR => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs >> rhs);
            },
            SHL => {
                let (lhs, rhs) = pop_two!(self);
                self.stack.push(lhs << rhs);
            },
            _ => return Err(RuntimeError::new_uncatchable(self, format!("[BytecodeReader]: Found an unknown byte {}.", byte)))
        }

        Ok(())
    }

    pub fn call_function(&mut self, target: Value, args_len: u8) -> RuntimeResult<()> {
        match target {
            Value::NativeFn(ptr) => unsafe {
                let nf = ptr.unwrap_ref();
                let stack_offset_index = self.stack.len() - args_len as usize;
                let args = ptr::slice_from_raw_parts(self.stack.as_mut_ptr().add(stack_offset_index), args_len as usize);

                self.call_stack.push(CallFrame { name: nf.name.clone(), ..Default::default() });
                match (nf.func)(self, &*args) {
                    Ok(value) => {
                        self.call_stack.pop();
                        self.stack.set_len(stack_offset_index);
                        ptr::drop_in_place(args as *mut [Value]);
                        self.stack.push(value);
                    },
                    Err(error) => return Err(error)
                }

                Ok(())
            },
            Value::Function(ptr) => {
                let Function { name, max_slots, start, upvalues, is_async } = ptr.unwrap_ref();
                let stack_start = self.stack.len() - args_len as usize;

                if *is_async {
                    self.stack.resize(stack_start + *max_slots as usize, Value::Null);
                    return Ok(());
                }

                self.call_stack.push(CallFrame { 
                    ip: self.ip, 
                    stack_start, 
                    name: name.clone(), 
                    upvalues: upvalues.to_vec(), 
                    max_slots: *max_slots 
                });

                self.stack.resize(stack_start + *max_slots as usize, Value::Null);
                self.ip = *start;

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
                        self.stack.insert(self.stack.len() - args_len as usize, self_);
                        match self.call_function_with_returned_value(init_.0, args_len + 1) {
                            Ok(_) => {
                                self.stack.push(self_);
                                Ok(())
                            },
                            Err(error) => Err(error)
                        }
                    },
                    None => {
                        self.stack.truncate(self.stack.len() - args_len as usize);
                        self.stack.push(self_);
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
                let stack_offset_index = self.stack.len() - args_len as usize;
                let args = ptr::slice_from_raw_parts(self.stack.as_ptr().add(stack_offset_index), args_len as usize);

                self.call_stack.push(CallFrame { name: nf.name.clone(), ..Default::default() });

                match (nf.func)(self, &*args) {
                    Ok(value) => {
                        self.call_stack.pop();
                        self.stack.set_len(stack_offset_index);
                        ptr::drop_in_place(args as *mut [Value]);
                        Ok(value)
                    },
                    Err(error) => Err(error)
                }
            },
            Value::Function(ptr) => {
                let Function { 
                    name, 
                    max_slots, 
                    start,
                    upvalues,
                    ..
                } = ptr.unwrap();

                let stack_start = self.stack.len() - args_len as usize;
                let current_ip = self.ip;

                self.call_stack.push(CallFrame { ip: self.ip, stack_start, name, upvalues: upvalues.to_vec(), max_slots });
                self.stack.resize(stack_start + max_slots as usize, Value::Null);
                self.ip = start;

                while self.ip < self.chunk.bytes.len() {
                    // Ip would reach current ip if the RETURN opcode appeared
                    if self.ip == current_ip {
                        return Ok(self.stack.pop().unwrap_or(Value::Null));
                    }

                    match self.execute_byte(self.chunk.bytes[self.ip]) {
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
                        self.stack.insert((self.stack.len() - args_len as usize) - 1, self_);
                        match self.call_function_with_returned_value(init_.0, args_len + 1) {
                            Ok(_) => Ok(self_),
                            Err(error) => Err(error)
                        }
                    },
                    None => {
                        self.stack.truncate(self.stack.len() - args_len as usize);
                        Ok(self_)
                    }
                }
            },
            value => Err(RuntimeError::new(self, format!("You cannot call a {}.", value.get_type())))
        }
    }

    fn call_inst_function(&mut self, self_: Value, attr: Value, args_len: u8) -> RuntimeResult<()> {
        macro_rules! inst_method {
            ($ptr:expr, $attr:ident) => {
                match attr {
                    Value::String(string) => {
                        let name = string.unwrap_ref();

                        match self.$attr.get(name) {
                            Some(&function) => {
                                match self.call_native_method(name.clone(), $ptr, args_len as usize, function) {
                                    Ok(value) => {
                                        self.stack.push(value);
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
                                        self.stack.push(value);
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
                    self.stack.insert(self.stack.len() - args_len as usize, self_);
                    self.call_function(method.0, args_len + 1)
                } else if let Some(property) = instance.properties.get(&attr) {
                    self.call_function(property.0, args_len)
                } else {
                    Err(RuntimeError::new(self, "Cannot call a null."))
                }
            },
            Value::Iterator(ptr) => inst_method!(ptr, iterator_methods),
            Value::String(ptr) => inst_method!(ptr, string_methods),
            _ => Err(RuntimeError::new(self, format!("Cannot call a {}.", self_.get_type())))
        }
    }

    fn call_native_method<T>(
        &mut self, 
        name: TinyString, 
        ptr: ValuePtr<T>, 
        args_len: usize, 
        method: MethodFn<T>
    ) -> RuntimeResult<Value> {
        unsafe {
            let stack_offset_index = self.stack.len() - args_len;
            let args = ptr::slice_from_raw_parts(self.stack.as_ptr().add(stack_offset_index), args_len as usize);

            self.call_stack.push(CallFrame { name, ..Default::default() });
            let result = method(self, ptr.unwrap_mut(), ptr.0, &*args);

            self.stack.set_len(stack_offset_index);
            self.call_stack.pop();
            ptr::drop_in_place(args as *mut [Value]);
            result
        }
    }

    fn resolve_attr(&mut self, target: Value, attr: Value) -> Value {
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

    pub(super) fn set_attr(&mut self, target: Value, attr: Value, value: Value, readonly: bool) -> RuntimeResult<()> {
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

    pub fn has_permission(&self, string: &str) -> bool {
        self.flags.contains_key(&TinyString::new(&[b"use-", string.as_bytes()].concat()))
    }

    pub fn add_global(&mut self, name: &str, value: Value) {
        let constant_id = self.chunk.constants.add_string(TinyString::new(name.as_bytes()));
        self.globals.insert(constant_id, (value, true));
    }

    fn add_local(&mut self, slot: usize, value: Value) {
        let frame = self.call_stack.last().unwrap();
        self.stack[frame.stack_start + slot] = value;
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

    pub(crate) fn allocate_value_ptr<O: ObjectTrait>(&mut self, object: O) -> ValuePtr<O> {
        ValuePtr::new_unchecked(self.allocate(object))
    }

    pub(crate) fn allocate_str_bytes(&mut self, bytes: &[u8]) -> ValuePtr<TinyString> {
        let string = TinyString::new(bytes);
        ValuePtr::new_unchecked(self.allocate(string))
    }

    pub(crate) fn allocate_static_str(&mut self, string: &str) -> ValuePtr<TinyString> {
        ValuePtr::new_unchecked(self.allocate(TinyString::new(string.as_bytes())))
    }

    pub(crate) fn allocate_string(&mut self, string: String) -> ValuePtr<TinyString> {
        ValuePtr::new_unchecked(self.allocate(TinyString::new(string.as_bytes())))
    }

    pub(crate) fn get_resource<T: Resource>(&self, resource_id: u32) -> Option<Rc<T>> {
        self.resource_table.get(&resource_id)
            .and_then(|rc| unsafe {
                if rc.type_id() == TypeId::of::<T>() {
                    Some((*(rc as *const Rc<_> as *const Rc<T>)).clone())
                } else { None }
            })
    }

    pub(crate) fn get_io_resource(&self, resource_id: u32) -> Option<Rc<dyn IoResource>> {
        self.resource_table.get(&resource_id)
            .and_then(|rc| unsafe {
                if rc.kind() == ResourceKind::Io {
                    Some((*(rc as *const Rc<_> as *const Rc<dyn IoResource>)).clone())
                } else { None }
            })
    }

    pub(crate) fn add_resource<T: Resource>(&mut self, resource: T) -> u32 {
        let rid = self.next_rid;
        assert!(self.resource_table.insert(self.next_rid, Rc::new(resource)).is_none());
        self.next_rid += 1;
        rid
    }

    pub fn collect_garbage(&mut self) {
        unsafe fn mark_value(value: &Value) {
            match value {
                Value::Array(ptr) => {
                    let pointer = ptr.as_ptr();
                    if pointer.is_null() {
                        panic!("Could not mark the pointer while sweeping garbage.");
                    }

                    let items = ptr::read(pointer.add(next_alignment(GcHeader::SIZE, mem::align_of::<Vec<Value>>())) as *const Vec<Value>);
                    for item in &items {
                        mark_value(item)
                    }

                    ptr::write(pointer as *mut bool, true);
                },
                Value::String(ptr) => {
                    ptr::write(ptr.as_ptr() as *mut bool, true);
                },
                _ => ()
            }
        }

        unsafe {
            for value in &self.stack {
                mark_value(value);
            }

            for value in &self.stack {
                mark_value(value);
            }

            for (_, (value, _)) in &self.globals {
                mark_value(&value);
            }

            // Clear all unreacable objects
            let mut index = 0;

            for handle in self.objects.clone() {
                if handle.dealloc_if_unreachable() {
                    self.objects.remove(index);
                }

                index += 1;
            }
        }

        self.next_gc = self.bytes_allocated * 2;
    }

}

// Basically will execute bytes until there is a return opcode asynchronously. Using a 
// mutable reference to the vm and a seperate stack and ip
pub struct VmAsyncExecution<'a> {
    pub(crate) vm: &'a mut Vm,
    pub(crate) stack: Vec<Value>,
    pub(crate) ip: usize
}

impl<'a> VmAsyncExecution<'a> {
    pub fn new(vm: &'a mut Vm, ip: usize) -> Self {
        Self { stack: vm.stack.clone(), ip, vm }
    }

    pub async fn execute(&mut self) -> RuntimeResult<Value> {
        Ok(Value::Null)
    }

    pub async fn handle_error(&mut self, error: RuntimeError) -> RuntimeResult<()> {
        handle_error!(error, self, self.vm)
    }
}