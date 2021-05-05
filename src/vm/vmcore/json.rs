extern crate serde_json;
use crate::vm::value::Value;
use crate::vm::vm::VM;
use super::result::{ ok, err };
use super::builtin::

pub fn parse(str: String, vm: &mut VM) -> Value {
    let json_parsed: Value = match serde_json::from_str(str.as_str()) {
        Ok(json) => json,
        Err(e) => return err(, vm)
    };

    Value::Null
}