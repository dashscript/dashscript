use std::env;
use std::collections::HashMap;
use std::time::Duration;
use std::thread;
use crate::vm::vm::VM;
use crate::vm::value::{ Value, ValueIndex, Dict };
use crate::common::get_line_col_by_line_data;
use super::builtin::{ inspect, panic, inspect_tiny };
use super::result::{ ok, ValueError };

pub fn inspect_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Str(match args.get(0) {
        Some(val) => inspect(val.clone(), vm),
        None => "null".to_string()
    })
}

pub fn inspect_tiny_api(_this: Value, args: Vec<Value>, _vm: &mut VM) -> Value {
    Value::Str(match args.get(0) {
        Some(val) => inspect_tiny(val.clone()),
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
        Ok(val) => ok(vm, Value::Str(val)),
        Err(e) => e.to_value_error(vm)
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

pub fn all_env_api(_this: Value, _args: Vec<Value>, _vm: &mut VM) -> Value {
    let mut entries = HashMap::new();

    for var in env::vars() {
        entries.insert(ValueIndex::Str(var.0), (Value::Str(var.1), true));
    }

    Value::Dict(Dict::Map(entries, None))
}

pub fn sleep_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    let duration = match args.get(0) {
        Some(Value::Num(val)) => Duration::from_millis(*val as u64),
        _ => panic("InvalidArgumentError: Expected a valid 1 number type argument.".to_string(), vm)
    };

    thread::sleep(duration);
    Value::Null
}

pub fn has_permission_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Boolean(vm.has_permission(match args.get(0) {
        Some(Value::Str(str)) => str.as_str(),
        _ => panic("InvalidArgumentError: Expected a valid 1 string type argument.".to_string(), vm)
    }))
}

pub fn trace_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    let mut address = String::new();

    for frame in vm.get_stack_trace().into_iter().rev() { 
        address += &format!("    at {}\n", frame).to_string();
    }

    let (line, col) = get_line_col_by_line_data(vm.body_line_data.clone(), vm.reader.ci);
    println!("{} ({}:{}:{})\n{}", match args.get(0) {
        Some(value) => inspect(value.clone(), vm),
        _ => String::new()
    }, vm.filename, line, col, address);

    Value::Null
}