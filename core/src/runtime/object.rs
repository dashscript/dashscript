use std::fmt::{self, Debug, Formatter};
use std::collections::HashMap;
use crate::{RuntimeResult, Value, TinyString, Vm, FunctionFlags, Upvalue, ValueIter};

#[derive(Debug, Clone, Copy)]
pub enum ObjectKind {
    NativeFunction,
    Function,
    Array,
    Map,
    Iterator,
    String
}

pub trait ObjectTrait {
    const DEFAULT_SIZE: usize; // The intial default size of the object
    const DEFAULT_ALIGN: usize;
    const KIND: ObjectKind;
}

macro_rules! impl_default_object_trait {
    ($($name:ty: $kind:ident)+) => {
        $(impl ObjectTrait for $name {
            const DEFAULT_SIZE: usize = std::mem::size_of::<Self>();
            const DEFAULT_ALIGN: usize = std::mem::align_of::<Self>();
            const KIND: ObjectKind = ObjectKind::$kind;
        })+
    };
}

pub type Map = HashMap<Value, (Value, bool)>;
pub type NativeFunctionHandler = fn (&mut Vm, &[Value]) -> RuntimeResult<Value>;

#[derive(Clone)]
pub struct NativeFunction {
    pub(crate) func: NativeFunctionHandler,
    pub(crate) is_instance: bool,
    pub(crate) name: TinyString
}

impl Default for NativeFunction {
    fn default() -> Self {
        Self {
            func: |_, _| Ok(Value::Null),
            is_instance: false,
            name: TinyString::new(b"anonymous")
        }
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("func", &String::from("[Function]"))
            .field("is_instance", &self.is_instance)
            .field("name", &self.name)
            .finish()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) name: TinyString,
    pub(crate) upvalues: Box<[Upvalue]>,
    pub(crate) start: usize,
    pub(crate) max_slots: u8,
    pub(crate) flags: u8
}

impl Function {
    pub fn is_instance(&self) -> bool {
        self.flags & FunctionFlags::INSTANCE == FunctionFlags::INSTANCE
    }

    pub fn to_instance(&mut self) {
        if self.flags & FunctionFlags::INSTANCE != FunctionFlags::INSTANCE {
            self.flags |= FunctionFlags::INSTANCE;
        }
    }
}

impl_default_object_trait! {
    NativeFunction: NativeFunction
    Vec<Value>: Array
    Map: Map
    Function: Function
    ValueIter: Iterator
    TinyString: String
}