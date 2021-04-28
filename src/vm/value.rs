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

pub type NativeFn = fn (Vec<Value>, &mut VM) -> Value;

#[derive(Clone, Hash, PartialEq, Debug)]
pub enum ValueIndex {
    True,
    False,
    Null,
    Str(String),
    Num(FloatLike)
}

impl Eq for ValueIndex {}

#[derive(Clone)]
pub enum Value {
    True,
    False,
    Null,
    Str(String),
    Num(fsize),
    Dict(HashMap<ValueIndex, (u32, bool)>),
    NativeFn(NativeFn)
}

impl Value {

    pub fn type_as_str(&self) -> String {
        String::from(
            match self {
                Value::True | Value::False => "boolean",
                Value::Null => "null",
                Value::Str(_) => "string",
                Value::Num(_) => "number",
                Value::NativeFn(_) => "function",
                Value::Dict(_) => "object"
            }
        )
    }

    pub fn to_value_index(&self) -> ValueIndex {
        match self {
            Value::True => ValueIndex::True,
            Value::False => ValueIndex::False,
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