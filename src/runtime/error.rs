use dashscript_bytecode::Opcode;
use crate::{Value, Vm};

pub trait ErrorValue {
    fn to_error_value(vm: &mut Vm) -> Value;
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    DislocatedBytes,
    UnknownOpcode { opcode: Opcode },
    CalledAnUncallable,
    Panic
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String
}

pub type RuntimeResult<T> = Result<T, Error>;