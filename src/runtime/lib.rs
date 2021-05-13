pub mod vm;
pub mod value;
pub mod array;
pub mod number;
pub mod dict;
pub mod error;
pub mod core;

pub use vm::{Vm, Frame};
pub use value::*;
pub use array::Array;
pub use dict::{Dict, DictKey};
pub use number::FloatLike;
pub use error::*;

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub(crate) const BYTE_COUNT: usize = 8;

#[cfg(not(target_pointer_width = "64"))]
pub(crate) const BYTE_COUNT: usize = 4;