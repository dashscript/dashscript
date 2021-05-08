// TODO(Scientific-Guy): Add more basic functions to the date module
use std::time::{ SystemTime, UNIX_EPOCH, Duration };
use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::common::fsize;

pub fn get_current_time() -> Duration {
    SystemTime::now().duration_since(UNIX_EPOCH).unwrap()
}

pub fn get_current_time_ms_api(_this: Value, _args: Vec<Value>, _vm: &mut VM) -> Value {
    Value::Num(get_current_time().as_millis() as fsize)
}