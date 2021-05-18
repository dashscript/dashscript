use std::string::ToString;
use dashscript_bytecode::Opcode;
use crate::{Value, Vm};

pub trait ErrorValue {
    fn to_error_value(vm: &mut Vm) -> Value;
}

#[derive(Clone, Debug)]
pub enum ErrorKind {
    DislocatedBytes,
    UnknownOpcode { opcode: Opcode },
    UnexpectedByte { byte: u8 },
    CalledAnUncallable,
    AssignmentToConstant { name: String },
    AssignmentToReadonlyProperty { attribute_name: String },
    UnexpectedAttributeAccess { 
        value_type: String,
        attribute: Option<String>
    },
    UnexpectedAssignment { instruction_value_type: String },
    UpdatedNonExistingValue { name: String },
    Panic
}

#[derive(Clone, Debug)]
pub struct Error {
    pub kind: ErrorKind,
    pub message: String
}

impl Error {
    pub(crate) fn new_untraced<S: ToString>(kind: ErrorKind, message: S) -> Self {
        Self { kind, message: message.to_string() }
    }
}

pub type RuntimeResult<T> = Result<T, Error>;