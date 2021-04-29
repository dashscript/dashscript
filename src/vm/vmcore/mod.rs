use std::collections::HashMap;
use crate::vm::value::{ Value, ValueIndex };
use crate::vm::vm::VM;

pub mod builtin;
pub mod window;
pub mod result;
pub mod memory;
pub mod math;
pub mod date;

// TODO(Scientific-Guy): Make more better conversion from dataset to dict
pub fn into_value_dict(data_set: Vec<(&str, Value, bool)>, vm: &mut VM) -> Value {
    let mut map = HashMap::new();

    for data in data_set {
        vm.value_stack.push(data.1.clone());
        map.insert(ValueIndex::Str(data.0.to_string()), (vm.value_stack.len() as u32 - 1, data.2));
    }

    Value::Dict(map)
}

pub fn into_value_array(data_set: Vec<Value>, vm: &mut VM) -> Value {
    let mut items = Vec::new();
    for data in data_set {
        vm.value_stack.push(data.clone());
        items.push(vm.value_stack.len() as u32 - 1);
    }

    Value::Array(items)
}

pub fn add_values(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Str(a), Value::Str(b)) => Value::Str(a + b.as_str()),
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
        (Value::Null, Value::Num(b)) => Value::Num(b),
        (a, b) => Value::Str(builtin::inspect_tiny(a) + builtin::inspect_tiny(b).as_str())
    }
}

pub fn sub_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
        (Value::Null, Value::Num(b)) => Value::Num(0.0 - b),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot subtract value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn mult_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
        (Value::Str(a), Value::Num(b)) => Value::Str(a.repeat(b as usize)),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot multiply value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn div_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot divide value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn pow_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot power value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}