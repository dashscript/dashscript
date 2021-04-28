use std::collections::HashMap;
use crate::vm::value::{ Value, ValueIndex };
use crate::vm::vm::VM;

pub mod builtin;
pub mod window;
pub mod result;
pub mod memory;
pub mod math;
pub mod date;

pub fn into_value_dict(data_set: Vec<(&str, Value, bool)>, vm: &mut VM) -> Value {
    let mut map = HashMap::new();

    for data in data_set.iter() {
        vm.value_stack.push(data.1.clone());
        map.insert(ValueIndex::Str(data.0.to_string()), (vm.value_stack.len() as u32 - 1, data.2));
    }

    Value::Dict(map)
}