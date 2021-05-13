use std::string::ToString;
use std::hash::{Hash, Hasher};
use dashscript_bytecode::fsize;
use crate::BYTE_COUNT;

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