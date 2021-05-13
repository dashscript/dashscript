use std::fmt;
use std::sync::Arc;
use std::cmp::Ordering;
use dashscript_bytecode::fsize;
use crate::{Array, Vm, Dict, RuntimeResult};

pub trait ToValue {
    fn to_value(&self) -> Value;
}

pub type NativeFn = dyn Fn(&mut Vm, Vec<Value>) -> RuntimeResult<Value>;

#[derive(Clone)]
pub enum Value {
    Boolean(bool),
    Str(String),
    Num(fsize),
    Dict(Dict),
    // TODO(Scientific-Guy): Find a way to make array as an object instead of a value type.
    Array(Array),
    Fn {
        name: u32, // The constant register id
        parameters: Vec<(u32, bool)>, // [(The parameter constant name, Boolean stating is the parameter rest)]
        bytes: Vec<u8>, // Set of instructions to be execute
        is_async: bool // Boolean stating is the function asynchronous or not
    },
    // TODO(Scientific-Guy): Think a better way for native functions
    NativeFn {
        name: u32,
        func: Arc<NativeFn>
    },
    Null
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a.len().cmp(&b.len()),
            (Value::Num(a), Value::Num(b)) => a.partial_cmp(&b).unwrap_or(Ordering::Equal),
            _ => Ordering::Equal
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Fn { bytes: a, .. }, Value::Fn { bytes: b, .. }) => a == b,
            (Value::NativeFn { func: a, .. }, Value::NativeFn { func: b, .. }) => a as *const Arc<NativeFn> == b as *const Arc<NativeFn>,
            (Value::Dict(a), Value::Dict(b)) => a.pointer() == b.pointer(),
            (Value::Null, Value::Null) => true,
            _ => false
        }
    }
}

impl Eq for Value {}

impl fmt::Debug for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Value::Str(str) => format!("\"{}\"", str),
            Value::Num(num) => num.to_string(),
            Value::Boolean(bool) => bool.to_string(),
            Value::Null => "null".to_string(),
            Value::Dict(Dict::Map(_, Some(id))) => format!("[Object({})]", id),
            Value::Dict(Dict::Map(_, None)) => format!("[Object]"),
            Value::Dict(Dict::Ref(id)) => format!("[Object(Ref({}))]", id),
            Value::Array(Array::Vec(_, Some(id))) => format!("[Array({})]", id),
            Value::Array(Array::Vec(_, None)) => format!("[Array]"),
            Value::Array(Array::Ref(id)) => format!("[Array(Ref({}))]", id),
            Value::Fn { .. }  => "[Function]".to_string(),
            Value::NativeFn { .. }=> "[NativeFunction]".to_string()
        })
    }
}

impl Default for Value {
    fn default() -> Self { Value::Null }
}

impl Value {

    pub fn get_type(&self) -> String {
        String::from(
            match self {
                Value::Boolean(_) => "boolean",
                Value::Null => "null",
                Value::Str(_) => "string",
                Value::Num(_) => "number",
                Value::NativeFn { .. } | Value::Fn { .. } => "function",
                Value::Dict(_) => "object",
                Value::Array(_) => "array"
            }
        )
    }

    pub fn to_vec(&self, vm: &mut Vm) -> Vec<Self> {
        match self {
            Value::Array(arr) => {
                let mut res = vec![];
                for item in arr.vec(vm) { 
                    res.push(item) 
                }

                res
            },
            _ => vec![]
        }
    }
    
    pub fn from<V>(value: V) -> Self
    where
        V: ToValue
    {
        value.to_value()
    }

}

#[derive(Clone, Debug, Default)]
pub struct ValueRegister {
    pub mutable: bool,
    pub value: Value,
    pub id: u32
}