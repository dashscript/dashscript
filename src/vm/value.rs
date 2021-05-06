use std::fmt;
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
    Dict(Dict),
    // TODO(Scientific-Guy): Think a better way for native functions.
    NativeFn(Box<Value>, NativeFn),
    // Array is used as a value type instead of an object because to prevent unwanted memory of attributes in value register.
    // TODO(Scientific-Guy): Find a way to make array as an object instead of a value type.
    Array(Array),
    Func(u32, Vec<(u32, bool)>, Vec<u8>, bool),
    Null
}

impl From<bool> for Value { 
    fn from(bool: bool) -> Self { Self::Boolean(bool) } 
}

impl From<fsize> for Value { 
    fn from(num: fsize) -> Self { Self::Num(num) } 
}

impl From<&str> for Value {
    fn from(str: &str) -> Self { Self::Str(str.to_string()) }
}

impl From<String> for Value {
    fn from(str: String) -> Self { Self::Str(str) }
}

impl From<NativeFn> for Value {
    fn from(func: NativeFn) -> Self { Self::NativeFn(Box::new(Value::Null), func) }
}

impl From<(Value, NativeFn)> for Value {
    fn from(func: (Value, NativeFn)) -> Self { Self::NativeFn(Box::new(func.0), func.1) }
}

impl Default for Value {
    fn default() -> Self { Value::Null }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a == b,
            (Value::Num(a), Value::Num(b)) => a == b,
            (Value::Boolean(a), Value::Boolean(b)) => a == b,
            (Value::Func(_, _, a, _), Value::Func(_, _, b, _)) => a == b,
            (Value::NativeFn(_, a), Value::NativeFn(_, b)) => a as *const NativeFn == b as *const NativeFn,
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
            Value::Func(_, _, _, _)  => "[Function]".to_string(),
            Value::NativeFn(_, _) => "[NativeFunction]".to_string()
        })
    }
}

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
                Value::NativeFn(_, _) | Value::Func(_, _, _, _) => "function",
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

    pub fn to_vec(&self, vm: &mut VM) -> Vec<Self> {
        match self {
            Value::Array(arr) => {
                let mut res = vec![];
                for item in arr.vec(vm) { res.push(item) }
                res
            },
            _ => vec![]
        }
    }

    pub fn to_pointer_value(&self, pointer: u32) -> Value {
        match self {
            Value::Dict(dict) => Value::Dict(match dict.clone() {
                Dict::Map(entries, None) => Dict::Map(entries, Some(pointer)),
                Dict::Map(_, Some(id)) => Dict::Ref(id),
                dict => dict
            }),
            Value::Array(arr) => Value::Array(match arr.clone() {
                Array::Vec(vector, None) => Array::Vec(vector, Some(pointer)),
                Array::Vec(_, Some(id)) => Array::Ref(id),
                arr => arr
            }),
            value => value.clone()
        }
    }

    pub fn borrow(&self, vm: &mut VM) -> Value {
        match self {
            Value::Dict(dict) => Value::Dict(match dict.clone() {
                Dict::Map(entries, None) => {
                    let val = Value::Dict(Dict::Map(entries, Some(vm.value_stack.len() as u32 + 1)));
                    vm.value_stack.push(val.clone());
                    return val;
                },
                Dict::Map(_, Some(id)) => Dict::Ref(id),
                dict => dict
            }),
            Value::Array(arr) => Value::Array(match arr.clone() {
                Array::Vec(vector, None) => {
                    let val = Value::Array(Array::Vec(vector, Some(vm.value_stack.len() as u32 + 1)));
                    vm.value_stack.push(val.clone());
                    return val;
                }, 
                Array::Vec(_, Some(id)) => Array::Ref(id),
                arr => arr
            }),
            value => value.clone()
        }
    }

}

#[derive(Clone, Debug)]
pub struct ValueRegister {
    pub key: String,
    pub id: u32,
    pub mutable: bool,
    pub depth: u32
}

pub enum ControlFlow {
    Break,
    Return(Value),
    None
}

#[derive(Clone)]
pub enum Dict {
    Map(HashMap<ValueIndex, (Value, bool)>, Option<u32>),
    Ref(u32)
}

impl Dict {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Dict::Map(_, id) => *id,
            Dict::Ref(id) => Some(*id)
        }
    }

    pub fn entries(&self, vm: &mut VM) -> HashMap<ValueIndex, (Value, bool)> {
        match self {
            Dict::Map(entries, _) => entries.clone(),
            Dict::Ref(id) => match vm.value_stack[*id as usize].clone() {
                Value::Dict(dict) => dict.entries(vm),
                _ => HashMap::new()
            }
        }
    }

}

#[derive(Clone)]
pub enum Array {
    Vec(Vec<Value>, Option<u32>),
    Ref(u32)
}

impl Array {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Array::Vec(_, id) => *id,
            Array::Ref(id) => Some(*id)
        }
    }

    pub fn vec(&self, vm: &mut VM) -> Vec<Value> {
        match self {
            Array::Vec(vec, _) => vec.clone(),
            Array::Ref(id) => match vm.value_stack[*id as usize].clone() {
                Value::Array(arr) => arr.vec(vm),
                _ => vec![]
            }
        }
    }

}