use std::io::stdin;
use crate::vm::vm::VM;
use crate::vm::value::{ Value, ValueIndex };
use crate::common::get_line_col_by_line_data;
use super::result::{ ok, err };
use super::into_value_dict;

pub fn readline(vm: &mut VM) -> Value {
    let mut result = String::new();
    match stdin().read_line(&mut result) {
        Ok(_) => ok(Value::Str(result), vm),
        Err(e) => err(into_value_dict(vec![
            ("kind", Value::Str(format!("{:?}", e.kind())), false),
            ("message", Value::Str(format!("{:?}", e)), false)
        ], vm), vm)
    }
}

pub fn inspect(val: Value, vm: &mut VM) -> String {
    match val {
        Value::Str(str) => str.to_string(),
        Value::Num(num) => num.to_string(),
        Value::Boolean(bool) => bool.to_string(),
        Value::Null => "null".to_string(),
        Value::NativeFn(_, _) => "[NativeFunction]".to_string(),
        Value::Dict(dict) => {
            let mut content = "{\n".to_string();

            for entry in dict.iter() {
                content += &format!(
                    "    {}: {},\n", 
                    match entry.0 {
                        ValueIndex::Str(str) => str.to_string(),
                        ValueIndex::Boolean(bool) => format!("[{}]", *bool),
                        ValueIndex::Null => "[null]".to_string(),
                        ValueIndex::Num(num) => num.0.to_string()
                    }, 
                    inspect_tiny(vm.value_stack[entry.1.0 as usize].clone(), vm)
                ).to_owned()
            }

            content + "}"
        },
        //_ => "unknown".to_string()
    }
}

pub fn inspect_tiny(val: Value, _vm: &mut VM) -> String {
    match val {
        Value::Str(str) => format!("\"{}\"", str),
        Value::Num(num) => num.to_string(),
        Value::Boolean(bool) => bool.to_string(),
        Value::Null => "null".to_string(),
        Value::NativeFn(_, _) => "[NativeFunction]".to_string(),
        Value::Dict(_) => "[Object]".to_string(),
        //_ => "unknown".to_string()
    }
}

pub fn panic(message: String, vm: &mut VM) -> ! {
    let mut address = String::new();

    for frame in vm.call_stack.clone() {
        address += &format!("    at {}", frame).to_string();
    }

    let (line, col) = get_line_col_by_line_data(vm.body_line_data.clone(), vm.reader.ci);
    println!("{} ({}:{}:{})\n{}", message, vm.filename, line, col, address);
    std::process::exit(0)
}

pub fn print_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    for arg in args.iter() {
        println!("{}", inspect(arg.clone(), vm));
    }

    Value::Null
}

pub fn typeof_api(_this: Value, args: Vec<Value>, _vm: &mut VM) -> Value {
    match args.get(0) {
        Some(val) => Value::Str(val.type_as_str()),
        _ => Value::Null
    }
}

pub fn panic_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    panic(match args.get(0) {
        Some(val) => match val {
            Value::Str(str) => str.to_string(),
            _ => inspect(val.clone(), vm)
        },
        None => "RuntimePanic: Runtime unexpectedly panicked.".to_string()
    }, vm);
}

pub fn readline_api(_this: Value, _args: Vec<Value>, vm: &mut VM) -> Value {
    readline(vm)
}

pub fn prompt_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(val) => match val {
            Value::Str(str) => println!("{}", str),
            _ => print!("{}", inspect(val.clone(), vm))
        },
        _ => ()
    }

    readline(vm)
}

pub fn confirm_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(val) => match val {
            Value::Str(str) => println!("{}", str),
            _ => print!("{}", inspect(val.clone(), vm))
        },
        _ => ()
    }

    match readline(vm) {
        Value::Str(str) => match str.as_str() {
            "y" | "Y" => Value::Boolean(true),
            _ => Value::Boolean(false)
        },
        _ => Value::Boolean(false)
    }
}

pub fn bool_api(_this: Value, args: Vec<Value>, _vm: &mut VM) -> Value {
    match args.get(0) {
        Some(Value::Boolean(false)) | Some(Value::Null) => Value::Boolean(false),
        Some(Value::Num(num)) => Value::Boolean(num.clone() == 0.0),
        _ => Value::Boolean(true)
    }
}