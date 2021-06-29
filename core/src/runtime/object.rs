use std::fmt::{self, Debug, Formatter};
use std::collections::HashMap;
use crate::{RuntimeResult, Value, TinyString, Vm, Upvalue, ValueIter, ValuePtr};

#[derive(Debug, Clone, Copy)]
pub enum ObjectKind {
    NativeFunction,
    Function,
    Array,
    Map,
    Iterator,
    String,
    Instance
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
    pub(crate) name: TinyString
}

impl Default for NativeFunction {
    fn default() -> Self {
        Self { func: |_, _| Ok(Value::Null), name: TinyString::new(b"anonymous") }
    }
}

impl Debug for NativeFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "NativeFunction({})", self.name)
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) name: TinyString,
    pub(crate) upvalues: Box<[Upvalue]>,
    pub(crate) start: usize,
    pub(crate) max_slots: u8,
    pub(crate) is_async: bool
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub(crate) properties: Map,
    pub(crate) methods: ValuePtr<Map>
}

impl_default_object_trait! {
    NativeFunction: NativeFunction
    Vec<Value>: Array
    Map: Map
    Function: Function
    ValueIter: Iterator
    TinyString: String
    Instance: Instance
}