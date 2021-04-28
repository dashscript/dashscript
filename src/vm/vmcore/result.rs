use crate::vm::{ value::Value, vm::VM };
use super::builtin::panic;
use super::into_value_dict;

pub fn ok(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::Boolean(true), false),
        ("isErr", Value::Boolean(false), false),
        ("unwrap", Value::NativeFn(Box::new(val), |this, _, _| this), false),
        ("unwrapError", Value::to_native_fn(|_, _, vm| panic("UnwrapError: Unwrapping an error when the result is ok.".to_string(), vm)), false)
    ], vm)
}

pub fn err(val: Value, vm: &mut VM) -> Value {
    into_value_dict(vec![
        ("isOk", Value::Boolean(true), false),
        ("isErr", Value::Boolean(false), false),
        ("unwrapError", Value::NativeFn(Box::new(val), |this, _, _| this), false),
        ("unwrap", Value::to_native_fn(|_, _, vm| panic("UnwrapError: Unwrapping an result when the result is error.".to_string(), vm)), false)
    ], vm)
}

pub fn ok_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    ok(match args.get(0) {
        Some(val) => val.clone(),
        _ => Value::Null
    }, vm)
}

pub fn err_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    err(match args.get(0) {
        Some(val) => val.clone(),
        _ => Value::Null
    }, vm)
}