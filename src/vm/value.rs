use std::collections::HashMap;
use std::hash::{ Hash, Hasher };
use crate::common::{ fsize, MAX_BYTES };
use super::vm::VM;

#[derive(Debug, Clone)]
pub struct FloatLike(pub fsize);

impl FloatLike {
    fn to_bytes(&self) -> [u8; MAX_BYTES] {
        self.0.to_le_bytes()
    }
}

// TODO(Scientific-Guy): Make something better for comparison rather than matching bytes.
impl PartialEq for FloatLike {
    fn eq(&self, other: &Self) -> bool {
        self.to_bytes() == other.to_bytes()
    }
}

impl Eq for FloatLike {}

impl Hash for FloatLike {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.to_bytes().hash(state);
    }
}

pub type NativeFn = fn (this: Value, Vec<Value>, &mut VM) -> Value;

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum ValueIndex {
    Boolean(bool),
    Str(String),
    Num(FloatLike),
    Null
}

impl Eq for ValueIndex {}

#[derive(Clone)]
pub enum Value {
    Boolean(bool),
    Str(String),
    Num(fsize),
    Dict(HashMap<ValueIndex, (u32, bool)>),
    // TODO(Scientific-Guy): Think a better way for native functions.
    NativeFn(Box<Value>, NativeFn),
    // Array is used as a value type instead of an object because to prevent unwanted memory of attributes in value register.
    // TODO(Scientific-Guy): Find a way to make array as an object instead of a value type.
    Array(Vec<u32>),
    Func(u32, Vec<u32>, Vec<u8>),
    Null
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Func(_, _, a), Value::Func(_, _, b)) => a == b,
            (Value::NativeFn(_, a), Value::NativeFn(_, b)) => a as *const NativeFn == b as *const NativeFn,
            (Value::Dict(a), Value::Dict(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false
        }
    }
}

impl Eq for Value {}

impl Value {

    pub fn to_native_fn(func: NativeFn) -> Self {
        Self::NativeFn(Box::new(Value::Null), func)
    }

    pub fn type_as_str(&self) -> String {
        String::from(
            match self {
                Value::Boolean(_) => "boolean",
                Value::Null => "null",
                Value::Str(_) => "string",
                Value::Num(_) => "number",
                Value::NativeFn(_, _) | Value::Func(_, _, _) => "function",
                Value::Dict(_) => "object",
                Value::Array(_) => "array"
            }
        )
    }

    pub fn to_value_index(&self) -> ValueIndex {
        match self {
            Value::Boolean(bool) => ValueIndex::Boolean(*bool),
            Value::Num(num) => ValueIndex::Num(FloatLike(num.clone())),
            Value::Str(str) => ValueIndex::Str(str.clone()),
            _ => ValueIndex::Null
        }
    }

}

#[derive(Clone)]
pub struct ValueRegister {
    pub key: String,
    pub id: u32,
    pub depth: u16,
    pub mutable: bool
}