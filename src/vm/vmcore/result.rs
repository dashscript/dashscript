use crate::vm::{ value::Value, vm::VM };
use super::into_value_dict;
use crate::vm::value::NativeFn;

pub fn ok(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
    ], vm)
}

pub fn err(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
    ], vm)
}

pub fn ok_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    into_value_dict(vec![
    ], vm)
}

pub fn err_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    into_value_dict(vec![
    ], vm)
}