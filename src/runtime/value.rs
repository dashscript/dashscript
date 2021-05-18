use std::{fmt, ops};
use std::sync::Arc;
use std::cmp::Ordering;
use std::string::ToString;
use crate::{Array, Vm, Dict, RuntimeResult, Number, NAN};

pub type NativeFn = dyn Fn(&mut Vm, Vec<Value>) -> RuntimeResult<Value> + Send + Sync;

#[derive(Clone)]
pub enum Value {
    Boolean(bool),
    Str(String),
    Num(Number),
    Dict(Dict),
    // TODO(Scientific-Guy): Find a way to make array as an object instead of a value type.
    Array(Array),
    Fn {
        name: String, // The function name
        parameters: Vec<(u32, bool)>, // [(The parameter constant name, Boolean stating is the parameter rest)]
        bytes: Vec<u8>, // Set of instructions to be execute
        is_async: bool, // Boolean stating is the function asynchronous or not
        is_instance: bool // Boolean stating is the function boolean or not
    },
    // TODO(Scientific-Guy): Think a better way for native functions
    NativeFn {
        name: String, // The function name
        func: Arc<NativeFn>, // The native function
        is_instance: bool // Boolean stating is the function boolean or not
    },
    Null
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a.len().cmp(&b.len()),
            (Value::Num(a), Value::Num(b)) => a.cmp(&b),
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

impl ToString for Value {
    fn to_string(&self) -> String {
        match self {
            Value::Str(string) => string.to_string(),
            Value::Num(number) => number.to_string(),
            Value::Null => "null".to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::Dict(_) => "[Object]".to_string(),
            Value::Array(_) => "[Array]".to_string(),
            Value::Fn { is_async: false, .. } | Value::NativeFn { .. } => "[Function]".to_string(),
            Value::Fn { is_async: true, .. } => "[AsyncFunction]".to_string(),
        }
    }
}

impl ops::Add for Value {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs + rhs),
            (Self::Str(lhs), Self::Str(rhs)) => Self::Str(lhs + &rhs),
            (Self::Null, Self::Num(rhs)) => Self::Num(rhs),
            (lhs, rhs) => Self::Str(lhs.to_string() + &rhs.to_string())
        }
    }
}

impl ops::Sub for Value {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs - rhs),
            (Self::Null, Self::Num(rhs)) => Self::Num(-rhs),
            _ => NAN
        }
    }
}

impl ops::Mul for Value {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs * rhs),
            _ => NAN
        }
    }
}

impl ops::Div for Value {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs / rhs),
            _ => NAN
        }
    }
}

impl ops::Rem for Value {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs % rhs),
            _ => NAN
        }
    }
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

    pub fn to_vec(&self, vm: &Vm) -> Vec<Self> {
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

    pub fn to_bool(&self) -> bool {
        match self {
            Value::Boolean(boolean) => *boolean,
            Value::Num(number) => *number != 0_isize,
            Value::Null => false,
            _ => true
        }
    }

    pub fn pow(&self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Num(lhs), Self::Num(rhs)) => Self::Num(lhs.pow(rhs)),
            _ => NAN
        }
    }

    pub fn borrow(&self, vm: &mut Vm) -> Self {
        match self {
            Self::Dict(dict) => {
                match dict {
                    Dict::Map(_, Some(id)) => Self::Dict(Dict::Ref(*id)),
                    Dict::Map(entries, None) => {
                        let pointer = vm.pointers.len() as u32;
                        vm.pointers.push(Self::Dict(Dict::Map(entries.clone(), Some(pointer))));
                        Self::Dict(Dict::Ref(pointer))
                    },
                    Dict::Ref(pointer) => Self::Dict(Dict::Ref(*pointer))
                }
            },
            Self::Array(array) => {
                match array {
                    Array::Vec(_, Some(id)) => Self::Array(Array::Ref(*id)),
                    Array::Vec(entries, None) => {
                        let pointer = vm.pointers.len() as u32;
                        vm.pointers.push(Self::Array(Array::Vec(entries.clone(), Some(pointer))));
                        Self::Array(Array::Ref(pointer))
                    },
                    Array::Ref(pointer) => Self::Array(Array::Ref(*pointer))
                }
            },
            value => value.clone()
        }
    }

}

impl Default for &Value {
    fn default() -> Self {
        &Value::Null
    }
}

impl From<&str> for Value {
    fn from(string: &str) -> Self { Self::Str(string.to_owned()) }
}

impl From<&String> for Value {
    fn from(string: &String) -> Self { Self::Str(string.to_owned()) }
}

impl From<String> for Value {
    fn from(string: String) -> Self { Self::Str(string) }
}

impl From<bool> for Value {
    fn from(boolean: bool) -> Self { Self::Boolean(boolean) }
}