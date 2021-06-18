// TODO(Scientific-Guy): Make a conditional support with a feature for Nan Tagging
// instead of just an enum which has the size of 16 bytes.
use std::ops;
use std::hash::{Hash, Hasher};
use std::ptr::NonNull;
use std::string::ToString;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use crate::{TinyString, GcHeader, Map, ValueIter, Vm};
use super::memory::{unwrap_str_bytes, unwrap_str, unwrap_tiny_string};

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Bool(bool), // The basic boolean value
    Int(isize), // The basic int value
    Float(f64), // The basic float value
    String(NonNull<u8>),
    Array(NonNull<u8>),
    Dict(NonNull<u8>),
    Function(NonNull<u8>),
    NativeFn(NonNull<u8>),
    Iterator(NonNull<u8>),
    Null // The basic null or empty value
}

impl Value {

    pub const NAN: Self = Self::Float(f64::NAN);
    pub const INFINITY: Self = Self::Float(f64::INFINITY);

    pub fn to_bool(&self) -> bool {
        match self {
            Self::Bool(boolean) => *boolean,
            Self::Int(0) => false,
            Self::Float(float) => *float == 0.0,
            _ => true
        }
    }

    pub fn to_tiny_string(&self) -> TinyString {
        match self {
            Self::Bool(boolean) => {
                if *boolean {
                    TinyString::new(b"true")
                } else { TinyString::new(b"false") }
            },
            Self::Int(int) => TinyString::new(format!("{}", int).as_bytes()),
            Self::Float(float) => TinyString::new(format!("{}", float).as_bytes()),
            Self::String(ptr) => unwrap_tiny_string(ptr.as_ptr()),
            Self::Dict(_) => TinyString::new(b"[Object]"),
            Self::Array(_) => TinyString::new(b"[Array]"),
            Self::Function(_) | Self::NativeFn(_) => TinyString::new(b"[Function]"),
            Self::Iterator(_) => TinyString::new(b"[Iterator]"),
            Self::Null => TinyString::new(b"null")
        }
    }

    pub fn pow(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a.pow(b as u32)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Int(a), Self::Float(b)) => Self::Float((a as f64).powf(b)),
            (Self::Float(a), Self::Int(b)) => Self::Float(a.powf(b as f64)),
            _ => Self::NAN
        }
    }

    pub fn is_nan(&self) -> bool {
        match self {
            Self::Float(float) => float.is_nan(),
            _ => false
        }
    }

    pub fn to_string(&self) -> String {
        // Safety: Those pointers are aligned and allocated perfectly
        match self {
            Self::Bool(boolean) => boolean.to_string(),
            Self::Int(int) => int.to_string(),
            Self::Float(float) => float.to_string(),
            Self::String(bytes) => format!("\"{}\"", unwrap_str(bytes.as_ptr())),
            Self::Dict(_) => "[Object]".to_owned(),
            Self::Array(_) => "[Array]".to_owned(),
            Self::Function(_) | Self::NativeFn(_) => "[Function]".to_owned(),
            Self::Iterator(_) => "[Iterator]".to_owned(),
            Self::Null => "null".to_string()
        }
    }

    pub fn to_f64(&self) -> f64 {
        match self {
            Value::Int(int) => *int as f64,
            Value::Float(float) => *float,
            _ => 0.0
        }
    }

    pub fn to_isize(&self) -> isize {
        match self {
            Value::Int(int) => *int,
            Value::Float(float) => *float as isize,
            _ => 0
        }
    }

    pub fn to_i32(&self) -> i32 {
        match self {
            Value::Int(int) => *int as i32,
            Value::Float(float) => *float as i32,
            _ => 0
        }
    }

    pub fn to_u32(&self) -> u32 {
        match self {
            Value::Int(int) => *int as u32,
            Value::Float(float) => *float as u32,
            _ => 0
        }
    }

    pub fn to_usize(&self) -> usize {
        match self {
            Value::Int(int) => *int as usize,
            Value::Float(float) => *float as usize,
            _ => 0
        }
    }

    pub fn get_type(&self) -> TinyString {
        TinyString::new(
            match self {
                Self::Bool(_) => b"boolean",
                Self::Null => b"null",
                Self::Int(_) | Self::Float(_) => b"number",
                Self::String(_) => b"string",
                Self::Array(_) => b"array",
                Self::Dict(_) => b"object",
                Self::Iterator(_) => b"iterator",
                Self::Function(_) | Self::NativeFn(_) => b"function"
            }
        )
    }

    pub fn into_iter(&self) -> ValueIter {
        match self {
            Self::Iterator(ptr) => unsafe { GcHeader::unwrap::<ValueIter>(ptr.as_ptr()) },
            Self::Array(ptr) => ValueIter::new(unsafe { GcHeader::unwrap::<Vec<Value>>(ptr.as_ptr()).as_slice() }),
            _ => ValueIter::default()
        }
    }

    pub fn iter_next(&self) -> Option<Value> {
        match self {
            Self::Iterator(ptr) => unsafe { GcHeader::unwrap_mut::<ValueIter>(ptr.as_ptr()).next() },
            _ => None
        }
    }

    pub fn add(self, vm: &mut Vm, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as f64 + b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a + b as f64),
            (Self::Null, value) => value,
            (lhs, rhs) => Self::String(vm.allocate_str(lhs.to_tiny_string() + rhs.to_tiny_string()))
        }
    }

    pub fn bit_not(self) -> Self {
        match self {
            Value::Int(int) => Value::Int(!int),
            _ => Value::Int(-1)
        }
    }

}

impl Default for Value {
    fn default() -> Self { Self::Null }
}

impl Default for &Value {
    fn default() -> &'static Value { &Value::Null }
}

impl ops::Not for Value {
    type Output = Self;

    fn not(self) -> Self {
        Self::Bool(!self.to_bool())
    }
}

impl ops::Neg for Value {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            Self::Int(int) => Self::Int(-int),
            Self::Float(float) => Self::Float(-float),
            _ => Self::Null
        }
    }
}

impl ops::Sub for Value {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as f64 - b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a - b as f64),
            (Self::Null, value) => value,
            _ => Self::NAN
        }
    }
}

impl ops::BitAnd for Value {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a & b),
            _ => Self::Int(-1)
        }
    }
}

impl ops::BitOr for Value {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a | b),
            _ => Self::Int(-1)
        }
    }
}

impl ops::BitXor for Value {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a ^ b),
            _ => Self::Int(-1)
        }
    }
}

impl ops::Shr for Value {
    type Output = Self;

    fn shr(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a >> b),
            _ => Self::Int(-1)
        }
    }
}

impl ops::Shl for Value {
    type Output = Self;

    fn shl(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a << b),
            _ => Self::NAN
        }
    }
}

impl ops::Mul for Value {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as f64 * b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a * b as f64),
            _ => Self::NAN
        }
    }
}

impl ops::Div for Value {
    type Output = Self;

    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Float(a as f64 / b as f64),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as f64 / b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a as f64 / b as f64),
            _ => Self::NAN
        }
    }
}

impl ops::Rem for Value {
    type Output = Self;

    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Float(a as f64 % b as f64),
            (Self::Float(a), Self::Float(b)) => Self::Float(a % b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as f64 % b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a as f64 % b as f64),
            _ => Self::NAN
        }
    }
}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            Self::Null => state.write_u8(0),
            Self::Bool(boolean) => state.write(&[1, *boolean as u8]),
            Self::Int(int) => {
                state.write_u8(3);
                int.hash(state);
            },
            Self::Float(float) => {
                state.write_u8(4);
                float.to_le_bytes().hash(state);
            },
            Self::String(ptr) => {
                state.write_u8(5);
                unwrap_str_bytes(ptr.as_ptr()).hash(state);
            },
            | Self::Array(ptr) 
            | Self::Dict(ptr) 
            | Self::Function(ptr) 
            | Self::NativeFn(ptr)
            | Self::Iterator(ptr) => {
                state.write_u8(6);
                ptr.hash(state);
            }
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Float(a), Value::Float(b)) => a == b,
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::String(a), Value::String(b)) => unwrap_str_bytes(a.as_ptr()) == unwrap_str_bytes(b.as_ptr()),
            (Value::Array(a), Value::Array(b)) => a == b,
            (Value::Dict(a), Value::Dict(b)) => a == b,
            (Value::Function(a), Value::Function(b)) => a == b,
            (Value::NativeFn(a), Value::NativeFn(b)) => a == b,
            (Value::Null, Value::Null) => true,
            _ => false
        }
    }
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a.partial_cmp(b),
            (Value::Float(a), Value::Float(b)) => a.partial_cmp(b),
            (Value::Int(a), Value::Float(b)) => (*a as f64).partial_cmp(b),
            (Value::Float(a), Value::Int(b)) => a.partial_cmp(&(*b as f64)),
            _ => None
        }
    }
}

impl Ord for Value {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.partial_cmp(other) {
            Some(ordering) => ordering,
            None => {
                match (self.is_nan(), other.is_nan()) {
                    (false, true) => Ordering::Greater,
                    (true, false) => Ordering::Equal,
                    _ => Ordering::Equal
                }
            }
        }
    }
}

impl Eq for Value {}

impl Display for Value {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(bytes) => write!(f, "{}", unwrap_str(bytes.as_ptr())),
            Value::Int(int) => write!(f, "{}", int),
            Value::Float(float) => write!(f, "{}", float),
            Value::Null => write!(f, "null"),
            Value::Bool(boolean) => write!(f, "{}", boolean),
            Value::Dict(entries) => {
                write!(f, "{{\n")?;
                for (key, value) in unsafe { GcHeader::unwrap::<Map>(entries.as_ptr()) } {
                    #[allow(unused_must_use)]
                    write!(f, "    {}: {},\n", key.to_string(), value.0.to_string());
                }

                write!(f, "}}")
            },
            Value::Array(vector) => {
                write!(f, "[\n")?;
                for item in unsafe { GcHeader::unwrap::<Vec<Value>>(vector.as_ptr()) }  {
                    #[allow(unused_must_use)]
                    write!(f, "    {},\n", item.to_string());
                }
                write!(f, "]")
            },
            Value::Function(_) | Value::NativeFn(_) => write!(f, "[Function]"),
            Value::Iterator(_) => write!(f, "[Iterator]")
        }
    }
}