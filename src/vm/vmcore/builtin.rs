use std::io::{ stdin, stdout, Write, Read };
use crate::vm::vm::VM;
use crate::vm::value::{ Value, ValueIndex };
use crate::common::get_line_col_by_line_data;
use super::result::{ ok, ValueError };

pub fn bool(val: Value) -> bool {
    match val {
        Value::Boolean(false) | Value::Null => false,
        Value::Num(num) => num == 0.0,
        _ => true
    }
}

pub fn inspect(val: Value, vm: &mut VM) -> String {
    match val {
        Value::Str(str) => str.to_string(),
        Value::Num(num) => num.to_string(),
        Value::Boolean(bool) => bool.to_string(),
        Value::Null => "null".to_string(),
        Value::Func(_, _, _, _) | Value::NativeFn(_, _) => "[Function]".to_string(),
        Value::Dict(dict) => {
            let entries = dict.entries(vm);
            let mut content = "{\n".to_string();

            for entry in entries.iter() {
                content += &format!(
                    "    {}: {},\n", 
                    match entry.0 {
                        ValueIndex::Str(str) => str.to_string(),
                        ValueIndex::Boolean(bool) => format!("[{}]", *bool),
                        ValueIndex::Null => "[null]".to_string(),
                        ValueIndex::Num(num) => num.0.to_string()
                    }, 
                    inspect_tiny(entry.1.0.clone())
                ).to_owned()
            }

            content + "}"
        },
        Value::Array(arr) => {
            let mut content = "[\n".to_string();
            for item in arr.vec(vm).iter().cloned() { content += format!("    {},\n", inspect_tiny(item)).as_str() }
            content + "]"
        }
    }
}

pub fn inspect_tiny(val: Value) -> String {
    match val {
        Value::Str(str) => format!("\"{}\"", str),
        Value::Num(num) => num.to_string(),
        Value::Boolean(bool) => bool.to_string(),
        Value::Null => "null".to_string(),
        Value::Dict(_) => "[Object]".to_string(),
        Value::Array(_) => "[Array]".to_string(),
        Value::Func(_, _, _, _) | Value::NativeFn(_, _)  => "[Function]".to_string()
    }
}

pub fn panic(message: String, vm: &mut VM) -> ! {
    let mut address = String::new();

    for frame in vm.get_stack_trace().into_iter().rev() { 
        address += &format!("    at {}\n", frame).to_string();
    }

    let (line, col) = get_line_col_by_line_data(vm.body_line_data.clone(), vm.reader.ci);
    println!("{} ({}:{}:{})\n{}", message, vm.filename, line, col, address);
    std::process::exit(1)
}

pub fn print_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    for arg in args.iter() {
        print!("{} ", inspect(arg.clone(), vm));
    }

    print!("\n");
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
        Some(val) => inspect(val.clone(), vm),
        None => "RuntimePanic: Runtime unexpectedly panicked.".to_string()
    }, vm);
}

pub fn readline_api(_this: Value, _args: Vec<Value>, vm: &mut VM) -> Value {
    let mut result = String::new();
    match stdin().read_line(&mut result) {
        Ok(_) => ok(vm, Value::Str(result)),
        Err(e) => e.to_value_error(vm)
    }
}

pub fn prompt_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(value) => print!("{}", inspect(value.clone(), vm)),
        _ => ()
    }

    if let Err(e) = stdout().flush() {
        return e.to_value_error(vm);
    };

    let mut result = String::new();
    match stdin().read_line(&mut result) {
        Ok(_) => ok(vm, Value::Str(result)),
        Err(e) => e.to_value_error(vm)
    }
}

pub fn confirm_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(value) => print!("{}", inspect(value.clone(), vm)),
        _ => ()
    }

    if let Err(_) = stdout().flush() {
        return Value::Boolean(false);
    };

    let mut result = String::new();
    match stdin().read_line(&mut result) {
        Ok(_) => Value::Boolean((result == "y") || (result == "Y")),
        Err(_) => Value::Boolean(false)
    }
}

pub fn alert_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(Value::Str(str)) => print!("{}", str),
        Some(value) => print!("{}", inspect(value.clone(), vm)),
        _ => ()
    }

    if let Err(_) = stdout().flush() {
        return Value::Null;
    };
    
    stdin().bytes().next();
    Value::Null
}

pub fn bool_api(_this: Value, args: Vec<Value>, _vm: &mut VM) -> Value {
    match args.get(0) {
        Some(Value::Boolean(false)) | Some(Value::Null) => Value::Boolean(false),
        Some(Value::Num(num)) => Value::Boolean(num.clone() == 0.0),
        _ => Value::Boolean(true)
    }
}

pub fn assert_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    if !match args.get(0..2) {
        Some([Value::Dict(a), Value::Dict(b)]) => a.entries(vm) == b.entries(vm),
        Some([Value::Array(a), Value::Array(b)]) => a.vec(vm) == b.vec(vm),
        Some([a, b]) => a == b,
        _ => false
    } {
        panic(format!(
            "AssertionFailure: Failed equality between {} and {}", 
            inspect(args.get(0).unwrap_or(&Value::Null).clone(), vm), 
            inspect(args.get(1).unwrap_or(&Value::Null).clone(), vm)
        ), vm)
    }

    Value::Null
}