extern crate alloc;

use std::mem;
use std::ptr::{self, NonNull};
use std::path::PathBuf;
use std::convert::TryInto;
use std::collections::HashMap;
use super::memory::*;
use crate::{
    Value, RuntimeResult, RuntimeErrorKind, RuntimeError, ObjectTrait, Chunk, TinyString, 
    NativeFunction, Map, Function, Upvalue, UpvalueState, FunctionFlags, ValueIter, 
    opcode, core
};

macro_rules! read_u8 {
    ($self:expr) => {
        match $self.chunk.bytes.get($self.ip) {
            Some(byte) => {
                $self.ip += 1;
                *byte
            },
            None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 1 }))
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
            None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 2 }))
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
            None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 4 }))
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
                    None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 1 }))
                }
            },
            1 => {
                $self.ip += 1;
                match $self.chunk.bytes.get($self.ip..$self.ip + 4) {
                    Some(byte) => {
                        $self.ip += 4;
                        u32::from_le_bytes(byte.try_into().unwrap())
                    },
                    None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 4 }))
                }
            },
            byte => return Err(RuntimeError::new_untraced(
                RuntimeErrorKind::UnexpectedByte {
                    found: byte,
                    wanted: (0, 1)
                }
            ))
        }
    };
}

macro_rules! pop_two {
    ($self:expr) => {
        match ($self.stack.pop(), $self.stack.pop()) {
            (Some(rhs), Some(lhs)) => (lhs, rhs),
            _ => return Err(RuntimeError::new_untraced(
                RuntimeErrorKind::TooSmallStack {
                    minimum_len: 2,
                    actual_len: $self.stack.len()
                }
            ))
        }
    };
}

pub type MethodMap<T> = HashMap<TinyString, fn (&mut Vm, &mut T, Vec<Value>) -> RuntimeResult<Value>>;

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

#[derive(Default)]
pub struct Vm {
    pub chunk: Chunk,
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
    flags: HashMap<TinyString, TinyString>,
    call_stack: Vec<CallFrame>,
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

    pub fn execute(&mut self) -> RuntimeResult<()> {
        self.stack.resize_with(self.chunk.bytes[self.ip] as usize, Default::default);
        self.ip += 1;

        while self.ip < self.chunk.bytes.len() {
            match self.execute_byte(self.chunk.bytes[self.ip]) {
                Ok(_) => (),
                Err(error) => {
                    match &error.kind {
                        RuntimeErrorKind::Panic { value } => {
                            let mut block = None;
                            for &try_block in &self.chunk.try_blocks {
                                if try_block.0 < self.ip {
                                    block = Some(try_block);
                                }
                            }

                            match block {
                                Some((_, jump_at, slot)) => {
                                    self.ip = jump_at;
                                    self.add_local(slot as usize, *value);
                                },
                                None => return Err(error)
                            }
                        },
                        _ => return Err(error)
                    }
                }
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
                let constant = self.allocate_str(TinyString::new(bytes));
                self.stack.push(Value::String(constant));
            },
            STRING_LONG => {
                let bytes = match self.chunk.constants.strings.get(read_u32!(self) as usize) {
                    Some(string) => string.as_bytes(),
                    None => &[]
                };
        
                #[allow(mutable_borrow_reservation_conflict)]
                let constant = self.allocate_str(TinyString::new(bytes));
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
                // println!("{:?}", self.stack);
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
                    (Some(a), Some(b), Some(c)) => self.set_attr(b, a, c),
                    _ => Err(RuntimeError::new_untraced(
                        RuntimeErrorKind::TooSmallStack {
                            minimum_len: 2,
                            actual_len: self.stack.len()
                        }
                    ))
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
                let ptr = self.allocate(value.into_iter());
                self.stack.push(Value::Iterator(unsafe { NonNull::new_unchecked(ptr) }))
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
                    let ptr = self.allocate(array);
                    self.stack.push(Value::Array(unsafe { NonNull::new_unchecked(ptr) }));
                } else {
                    return Err(RuntimeError::new_untraced(
                        RuntimeErrorKind::TooSmallStack {
                            minimum_len: array_len as u32,
                            actual_len: len
                        }
                    ))
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
                    let ptr = self.allocate(entries);
                    self.stack.push(Value::Dict(unsafe { NonNull::new_unchecked(ptr) }));
                } else {
                    return Err(RuntimeError::new_untraced(
                        RuntimeErrorKind::TooSmallStack {
                            minimum_len: entries_len as u32,
                            actual_len: len
                        }
                    ))
                }
            },
            FUNC => {
                match self.chunk.bytes.get(self.ip..self.ip + 3) {
                    Some(&[flags, u16_1, u16_2]) => {
                        let start = self.ip + 3;
                        self.ip = start + u16::from_le_bytes([u16_1, u16_2]) as usize;

                        let (max_slots, upvalue_len) = match self.chunk.bytes.get(self.ip..self.ip + 2) {
                            Some(bytes) => {
                                self.ip += 2;
                                (bytes[0], bytes[1])
                            },
                            None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 2 }))
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
                                None => return Err(RuntimeError::new_untraced(RuntimeErrorKind::InsufficientBytes { needed: 2 }))
                            };
                        }

                        let name = read_auto!(self);
                        let ptr = self.allocate(
                            Function {
                                name: self.chunk.constants.get_string(name),
                                flags,
                                start,
                                upvalues,
                                max_slots
                            }
                        );

                        self.stack.push(Value::Function(unsafe { NonNull::new_unchecked(ptr) }))
                    },
                    _ => return Err(RuntimeError::new_untraced(
                        RuntimeErrorKind::TooSmallStack {
                            actual_len: self.chunk.bytes.len(),
                            minimum_len: 4
                        }
                    ))
                }
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
                let (lhs, rhs) = match (self.stack.pop(), self.stack.pop()) {
                    (Some(rhs), Some(lhs)) => (lhs, rhs),
                    _ => return Err(RuntimeError::new_untraced(
                        RuntimeErrorKind::TooSmallStack {
                            minimum_len: 2,
                            actual_len: self.stack.len()
                        }
                    ))
                };

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
            _ => return Err(RuntimeError::new_untraced(RuntimeErrorKind::UnknownByte { byte }))
        }

        Ok(())
    }

    pub(crate) fn call_function(&mut self, target: Value, args_len: u8) -> RuntimeResult<()> {
        match target {
            Value::NativeFn(ptr) => {
                let nf = unsafe { GcHeader::unwrap::<NativeFunction>(ptr.as_ptr()) };
                let stack_offset_index = self.stack.len() - args_len as usize;
                let args = self.stack.get(stack_offset_index..).unwrap_or_default().to_vec();
                if nf.is_instance {
                    return Err(RuntimeError::new_untraced(RuntimeErrorKind::SelfNotFound { name: nf.name.to_string() }));
                }

                self.call_stack.push(CallFrame { name: nf.name, ..Default::default() });
                self.stack.truncate(stack_offset_index);

                match (nf.func)(self, args) {
                    Ok(value) => {
                        self.call_stack.pop();
                        self.stack.push(value);
                    },
                    Err(error) => return Err(error)
                }

                Ok(())
            },
            Value::Function(ptr) => {
                let Function { 
                    name, 
                    max_slots, 
                    start,
                    upvalues,
                    flags
                } = unsafe { GcHeader::unwrap::<Function>(ptr.as_ptr()) };

                if flags & FunctionFlags::INSTANCE == FunctionFlags::INSTANCE {
                    return Err(RuntimeError::new_untraced(RuntimeErrorKind::SelfNotFound { name: name.to_string() }));
                }

                let stack_start = self.stack.len() - args_len as usize;
                self.call_stack.push(CallFrame { ip: self.ip, stack_start, name, upvalues, max_slots });
                self.stack.resize_with(stack_start + max_slots as usize, Default::default);
                self.ip = start;

                Ok(())
            },
            _ => Err(RuntimeError::new_untraced(
                RuntimeErrorKind::CalledAnUncallable {
                    value_type: "null".to_owned()
                }
            ))
        }
    }

    fn call_inst_function(&mut self, self_: Value, attr: Value, args_len: u8) -> RuntimeResult<()> {
        macro_rules! inst_method {
            ($ptr:expr, $name:expr, $attr:ident) => {
                match attr {
                    Value::String(string) => {
                        let string = unwrap_tiny_string_ref(string.as_ptr());
                        match self.$attr.get(string) {
                            Some(&function) => {
                                let name = TinyString::new(&[$name, string.to_bytes()].concat());
                                match self.call_native_method(name, $ptr.as_ptr(), args_len as usize, function) {
                                    Ok(value) => {
                                        self.stack.push(value);
                                        Ok(())
                                    },
                                    Err(error) => return Err(error)
                                }
                            },
                            None => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                        }
                    },
                    _ => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                }
            };
        }

        let mut call = |value: Value| match value {
            Value::NativeFn(ptr) => {
                let nf = unsafe { GcHeader::unwrap::<NativeFunction>(ptr.as_ptr()) };
                let stack_offset_index = self.stack.len() - args_len as usize;
                let mut args = self.stack.get(stack_offset_index..).unwrap_or_default().to_vec();

                if nf.is_instance {
                    args.insert(0, self_);
                }

                self.call_stack.push(CallFrame { name: nf.name, ..Default::default() });
                self.stack.truncate(stack_offset_index);

                match (nf.func)(self, args) {
                    Ok(value) => {
                        self.call_stack.pop();
                        self.stack.push(value);
                    },
                    Err(error) => return Err(error)
                }

                Ok(())
            },
            Value::Function(ptr) => {
                let Function { 
                    name, 
                    max_slots, 
                    start,
                    upvalues,
                    flags,
                    ..
                } = unsafe { GcHeader::unwrap::<Function>(ptr.as_ptr()) };

                let stack_start = self.stack.len() - args_len as usize;
                if flags & FunctionFlags::INSTANCE == FunctionFlags::INSTANCE {
                    self.stack.insert(stack_start, self_);
                }

                self.call_stack.push(CallFrame { ip: self.ip, stack_start, name, upvalues, max_slots });
                self.stack.resize_with(stack_start + max_slots as usize, Default::default);
                self.ip = start;
                Ok(())
            },
            _ => Err(RuntimeError::new_untraced(
                RuntimeErrorKind::CalledAnUncallable {
                    value_type: "null".to_owned()
                }
            ))
        };

        match self_ {
            Value::Dict(ptr) => {
                let entries = unsafe { GcHeader::unwrap::<Map>(ptr.as_ptr()) };
                match entries.get(&attr) {
                    Some((value, _)) => call(*value),
                    None => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                }
            },
            Value::Array(ptr) => {
                let array = unsafe { GcHeader::unwrap::<Vec<Value>>(ptr.as_ptr()) };
                match attr {
                    Value::Int(int) => {
                        match array.get(int as usize) {
                            Some(value) => call(*value),
                            None => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                        }
                    },
                    Value::String(string) => {
                        let string = unwrap_tiny_string_ref(string.as_ptr());
                        match self.array_methods.get(string) {
                            Some(&function) => {
                                let name = TinyString::new(&[b"String.", string.to_bytes()].concat());
                                match self.call_native_method(name, ptr.as_ptr(), args_len as usize, function) {
                                    Ok(value) => {
                                        self.stack.push(value);
                                        Ok(())
                                    },
                                    Err(error) => return Err(error)
                                }
                            },
                            None => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                        }
                    },
                    _ => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
                }
            },
            Value::Iterator(ptr) => inst_method!(ptr, b"Iterator.", iterator_methods),
            Value::String(ptr) => inst_method!(ptr, b"String.", string_methods),
            _ => Err(RuntimeError::new_untraced(RuntimeErrorKind::CalledAnUncallable { value_type: "null".to_owned() }))
        }
    }

    fn call_native_method<T>(
        &mut self, 
        name: TinyString, 
        ptr: *const u8, 
        args_len: usize, 
        method: fn (&mut Vm, &mut T, Vec<Value>) -> RuntimeResult<Value>
    ) -> RuntimeResult<Value> {
        let stack_offset_index = self.stack.len() - args_len;
        let args = self.stack.drain(stack_offset_index..).collect();
        self.call_stack.push(CallFrame { name, ..Default::default() });
        let result = method(self, unsafe { GcHeader::unwrap_mut::<T>(ptr) }, args);
        self.call_stack.pop();
        result
    }

    fn resolve_attr(&mut self, target: Value, attr: Value) -> Value {
        match target {
            Value::Dict(ptr) => {
                let entries = unsafe { GcHeader::unwrap::<Map>(ptr.as_ptr()) };
                match entries.get(&attr) {
                    Some((value, _)) => *value,
                    None => Value::Null
                }
            },
            Value::Array(ptr) => {
                let array = unsafe { GcHeader::unwrap::<Vec<Value>>(ptr.as_ptr()) };
                match attr {
                    Value::Int(int) => {
                        match array.get(int as usize) {
                            Some(value) => *value,
                            None => Value::Null
                        }
                    },
                    _ => Value::Null
                }
            },
            _ => Value::Null
        }
    }

    fn set_attr(&mut self, target: Value, attr: Value, value: Value) -> RuntimeResult<()> {
        match target {
            Value::Dict(ptr) => {
                let entries = unsafe { GcHeader::unwrap_mut::<Map>(ptr.as_ptr()) };
                if let Some((_, true)) = entries.insert(attr, (value, true)) {
                    return Err(RuntimeError::new_untraced(RuntimeErrorKind::CannotAssignToReadonlyProperty))
                }
            },
            Value::Array(ptr) => {
                let array = unsafe { GcHeader::unwrap_mut::<Vec<Value>>(ptr.as_ptr()) };
                match attr {
                    Value::Int(int) => {
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
            _ => ()
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

    pub(crate) fn allocate_non_null<O: ObjectTrait>(&mut self, object: O) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.allocate(object)) }
    }

    pub(crate) fn allocate_str_bytes(&mut self, bytes: &[u8]) -> NonNull<u8> {
        let string = TinyString::new(bytes);
        unsafe { NonNull::new_unchecked(self.allocate(string)) }
    }

    pub(crate) fn allocate_str(&mut self, string: TinyString) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.allocate(string)) }
    }

    pub(crate) fn allocate_static_str(&mut self, string: &str) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.allocate(TinyString::new(string.as_bytes()))) }
    }

    pub(crate) fn allocate_string(&mut self, string: String) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.allocate(TinyString::new(string.as_bytes()))) }
    }

    pub fn collect_garbage(&mut self) {
        unsafe fn mark_value(value: &Value) {
            match value {
                | Value::NativeFn(ptr)
                | Value::Dict(ptr)
                | Value::Function(ptr) => GcHeader::mark(ptr.as_ptr()),
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