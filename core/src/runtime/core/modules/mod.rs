use crate::{Vm, Value};

pub type ModuleLoader = fn (vm: &mut Vm) -> Value;