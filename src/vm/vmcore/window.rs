use std::env;
use std::env::VarError;
use std::collections::HashMap;
use std::time::Duration;
use std::thread;
use crate::vm::{ value::{ Value, ValueIndex }, vm::VM };
use super::builtin::{ inspect, panic, inspect_tiny };
use super::result::{ ok, err };
use super::into_value_dict;

pub fn inspect_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Str(match args.get(0) {
        Some(val) => inspect(val.clone(), vm),
        None => "null".to_string()
    })
}

pub fn inspect_tiny_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Str(match args.get(0) {
        Some(val) => inspect_tiny(val.clone(), vm),
        None => "null".to_string()
    })
}

pub fn exit_api(_this: Value, args: Vec<Value>, _vm: &mut VM) -> Value {
    std::process::exit(match args.get(0) {
        Some(Value::Num(num)) => *num as i32,
        _ => 0
    });
}

pub fn get_env_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match env::var(match args.get(0) {
        Some(key) => match key {
            Value::Str(str) => str,
            _ => panic("InvalidArgumentError: Expected a valid 1 string type argument.".to_string(), vm)
        },
        None => panic("InvalidArgumentError: Expected a valid 1 string type argument.".to_string(), vm)
    }) {
        Ok(val) => ok(Value::Str(val), vm),
        Err(e) => match e {
            VarError::NotPresent => err(into_value_dict(vec![("kind", Value::Str("Not Present".to_string()), false)], vm), vm),
            VarError::NotUnicode(_) => err(into_value_dict(vec![("kind", Value::Str("Not Unicode".to_string()), false)], vm), vm),
        }
    }
}

pub fn set_env_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    let key = match args.get(0) {
        Some(key) => match key {
            Value::Str(str) => str,
            _ => panic("InvalidArgumentError: Expected a valid 2 string type argument.".to_string(), vm)
        },
        None => panic("InvalidArgumentError: Expected a valid 2 string type argument.".to_string(), vm)
    };

    let value = match args.get(1) {
        Some(key) => match key {
            Value::Str(str) => str,
            _ => panic("InvalidArgumentError: Expected a valid 2 string type argument.".to_string(), vm)
        },
        None => panic("InvalidArgumentError: Expected a valid 2 string type argument.".to_string(), vm)
    };

    env::set_var(key, value);
    Value::Null
}

pub fn delete_env_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    env::remove_var(match args.get(0) {
        Some(Value::Str(str)) => str,
        _ => panic("InvalidArgumentError: Expected a valid 1 string type argument.".to_string(), vm)
    });
    
    Value::Null
}

pub fn all_env_api(_this: Value, _args: Vec<Value>, vm: &mut VM) -> Value {
    let mut entries = HashMap::new();

    for var in env::vars() {
        vm.value_stack.push(Value::Str(var.1));
        entries.insert(ValueIndex::Str(var.0), (vm.value_stack.len() as u32 - 1, true));
    }

    Value::Dict(entries)
}

pub fn sleep_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    let duration = match args.get(0) {
        Some(Value::Num(val)) => Duration::from_millis(*val as u64),
        _ => panic("InvalidArgumentError: Expected a valid 1 number type argument.".to_string(), vm)
    };

    thread::sleep(duration);
    Value::Null
}