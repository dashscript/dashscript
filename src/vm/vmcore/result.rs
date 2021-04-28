use crate::vm::{ value::Value, vm::VM };
use super::into_value_dict;

pub fn ok(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::True, false),
        ("isError", Value::False, false),
        ("content", val, false)
    ], vm)
}

pub fn err(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::False, false),
        ("isError", Value::True, false),
        ("content", val, false)
    ], vm)
}

pub fn ok_api(values: Vec<Value>, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::True, false),
        ("isError", Value::False, false),
        ("content", match values.get(0) {
            Some(val) => val.clone(),
            None => Value::Null
        }, false)
    ], vm)
}

pub fn err_api(values: Vec<Value>, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::False, false),
        ("isError", Value::True, false),
        ("content", match values.get(0) {
            Some(val) => val.clone(),
            None => Value::Null
        }, false)
    ], vm)
}