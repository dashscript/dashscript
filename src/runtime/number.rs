use std::ops;
use std::cmp::Ordering;
use std::string::ToString;
use std::hash::{Hash, Hasher};
use std::f64::{NAN, INFINITY, NEG_INFINITY};
use dashscript_bytecode::fsize;
use crate::BYTE_COUNT;

#[derive(Debug, Clone)]
pub enum Number {
    Int(isize),
    Float(fsize)
}

impl Number {

    pub const NAN: Self = Self::Float(NAN);
    pub const INFINITY: Self = Self::Float(INFINITY);
    pub const NEG_INFINITY: Self = Self::Float(NEG_INFINITY);

    pub fn is_nan(&self) -> bool {
        match self {
            Self::Float(float) => float.is_nan(),
            Self::Int(_) => false
        }
    }

    pub fn pow(&self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a.pow(b as u32)),
            (Self::Float(a), Self::Float(b)) => Self::Float(a.powf(b)),
            (Self::Int(a), Self::Float(b)) => Self::Float((*a as fsize).powf(b)),
            (Self::Float(a), Self::Int(b)) => Self::Float(a.powf(b as fsize))
        }
    }

}

impl Default for Number {
    fn default() -> Self { Self::Int(0) }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a == b,
            (Self::Float(a), Self::Float(b)) => a == b,
            (Self::Int(a), Self::Float(b)) | (Self::Float(b), Self::Int(a)) => *a as fsize == *b
        }
    }
}

impl PartialEq<isize> for Number {
    fn eq(&self, other: &isize) -> bool {
        match self {
            Self::Int(int) => int == other,
            Self::Float(float) => *float == *other as fsize
        }
    }
}

impl Eq for Number {}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Self::Int(a), Self::Int(b)) => a.partial_cmp(&b),
            (Self::Float(a), Self::Float(b)) => a.partial_cmp(&b),
            (Self::Int(a), Self::Float(b)) | (Self::Float(b), Self::Int(a)) => (*a as fsize).partial_cmp(&b)
        }
    }
}

impl Ord for Number {
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

impl ToString for Number {
    fn to_string(&self) -> String {
        match self {
            Self::Int(int) => int.to_string(),
            Self::Float(float) => float.to_string()
        }
    }
}

impl ops::Add for Number {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a + b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a + b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as fsize + b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a + b as fsize)
        }
    }
}

impl ops::Sub for Number {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a - b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a - b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as fsize - b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a - b as fsize)
        }
    }
}

impl ops::Mul for Number {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a * b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a * b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as fsize * b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a * b as fsize)
        }
    }
}

impl ops::Div for Number {
    type Output = Self;
    fn div(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Float(a as fsize / b as fsize),
            (Self::Float(a), Self::Float(b)) => Self::Float(a / b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as fsize / b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a / b as fsize)
        }
    }
}

impl ops::Rem for Number {
    type Output = Self;
    fn rem(self, rhs: Self) -> Self {
        match (self, rhs) {
            (Self::Int(a), Self::Int(b)) => Self::Int(a % b),
            (Self::Float(a), Self::Float(b)) => Self::Float(a % b),
            (Self::Int(a), Self::Float(b)) => Self::Float(a as fsize % b),
            (Self::Float(a), Self::Int(b)) => Self::Float(a % b as fsize)
        }
    }
}

impl ops::Neg for Number {
    type Output = Self;
    fn neg(self) -> Self {
        match self {
            Self::Int(int) => Self::Int(-int),
            Self::Float(float) => Self::Float(-float)
        }
    }
}

// A float like struct to use it in hashmap's and etc
#[derive(Debug, Clone)]
pub struct FloatLike(pub fsize);

impl FloatLike {
    fn to_bytes(&self) -> [u8; BYTE_COUNT] {
        self.0.to_le_bytes()
    }
}

impl PartialEq for FloatLike {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl Eq for FloatLike {}

impl Hash for FloatLike {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.to_bytes().hash(state);
    }
}

impl ToString for FloatLike {
    fn to_string(&self) -> String {
        self.0.to_string()
    }
}