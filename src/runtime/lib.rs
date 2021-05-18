pub mod vm;
pub mod value;
pub mod array;
pub mod number;
pub mod dict;
pub mod error;
pub mod core;
pub mod instruction;

pub use vm::{Vm, Frame};
pub use value::*;
pub use array::Array;
pub use dict::{Dict, DictKey, DictBuilder};
pub use number::{FloatLike, Number};
pub use error::*;
pub use instruction::{Instruction, InstructionValue};
pub use crate::core::IntoValue;

#[cfg(target_pointer_width = "64")]
pub(crate) const BYTE_COUNT: usize = 8;

#[cfg(not(target_pointer_width = "64"))]
pub(crate) const BYTE_COUNT: usize = 4;

pub const NULL: Value = Value::Null;
pub const NAN: Value = Value::Num(Number::NAN);
pub const INFINITY: Value = Value::Num(Number::INFINITY);
pub const NEG_INFINITY: Value = Value::Num(Number::NEG_INFINITY);