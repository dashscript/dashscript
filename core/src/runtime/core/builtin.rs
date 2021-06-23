use std::time::{SystemTime, UNIX_EPOCH};
use crate::{Vm, Value, TinyString};
use super::map_builder::MapBuilder;

fn random_isize() -> isize {
    let mut random: isize = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as isize;

    let mut rand = || {
        random ^= random << 13;
        random ^= random >> 17;
        random ^= random << 5;
        random
    };
        
    if std::mem::size_of::<isize>() <= 4 {
        rand()
    } else {
        (((rand() as i64) << 32) | (rand() as i64)) as isize
    }
}

fn random_usize() -> usize {
    let mut random: usize = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as usize;

    let mut rand = || {
        random ^= random << 13;
        random ^= random >> 17;
        random ^= random << 5;
        random
    };
        
    if std::mem::size_of::<usize>() <= 4 {
        rand()
    } else {
        (((rand() as u64) << 32) | (rand() as u64)) as usize
    }
}

pub fn init_math(vm: &mut Vm) -> Value {
    let mut math = MapBuilder::new(vm);

    // All the basic and important constants
    math.constant("E", Value::Float(std::f64::consts::E));
    math.constant("LN10", Value::Float(std::f64::consts::LN_10));
    math.constant("LN2", Value::Float(std::f64::consts::LN_2));
    math.constant("PI", Value::Float(std::f64::consts::PI));
    math.constant("LOG10E", Value::Float(std::f64::consts::LOG10_E));
    math.constant("LOG2E", Value::Float(std::f64::consts::LOG2_E));
    math.constant("SQRT2", Value::Float(std::f64::consts::SQRT_2));

    // All the basic method based methods
    macro_rules! add_method_based_native_fn {
        ($name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0) {
                    Some(Value::Int(int)) => Value::Int(int.$method()),
                    Some(Value::Float(float)) => Value::Float(float.$method()),
                    _ => Value::NAN
                }
            )) 
        };

        (float $name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0) {
                    Some(Value::Float(float)) => Value::Float(float.$method()),
                    Some(Value::Int(int)) => Value::Float((*int as f64).$method()),
                    _ => Value::NAN
                }
            )) 
        };

        (float2 $name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0..1) {
                    Some([Value::Float(float), num]) => {
                        let num = match num {
                            Value::Int(int) => *int as f64,
                            Value::Float(float) => *float,
                            _ => return Ok(Value::NAN)
                        };

                        Value::Float(float.$method(num))
                    },
                    Some([Value::Int(int), num]) => {
                        let num = match num {
                            Value::Int(int) => *int as f64,
                            Value::Float(float) => *float,
                            _ => return Ok(Value::NAN)
                        };

                        Value::Float((*int as f64).$method(num))
                    },
                    _ => Value::NAN
                }
            )) 
        };
    }

    add_method_based_native_fn!("abs", abs);
    add_method_based_native_fn!(float "sqrt", sqrt);
    add_method_based_native_fn!(float "acosh", acosh);
    add_method_based_native_fn!(float "acos", acos);
    add_method_based_native_fn!(float "asin", asin);
    add_method_based_native_fn!(float "asinh", asinh);
    add_method_based_native_fn!(float "atan", atan);
    add_method_based_native_fn!(float "atanh", atanh);
    add_method_based_native_fn!(float "cbrt", cbrt);
    add_method_based_native_fn!(float "ceil", ceil);
    add_method_based_native_fn!(float "cos", cos);
    add_method_based_native_fn!(float "cosh", cosh);
    add_method_based_native_fn!(float "exp", exp);
    add_method_based_native_fn!(float "floor", floor);
    add_method_based_native_fn!(float "round", round);
    add_method_based_native_fn!(float "log10", log10);
    add_method_based_native_fn!(float "log2", log2);
    add_method_based_native_fn!(float "sin", sin);
    add_method_based_native_fn!(float "sinh", sinh);
    add_method_based_native_fn!(float "tan", tan);
    add_method_based_native_fn!(float "tamh", tanh);
    add_method_based_native_fn!(float "trunc", trunc);
    add_method_based_native_fn!(float2 "hypot", hypot);
    add_method_based_native_fn!(float2 "log", log);
    add_method_based_native_fn!(float2 "atan2", atan2);

    math.native_fn("max", |_, args| Ok(
        match args.iter().max() {
            Some(value) => value.clone(),
            None => Value::Null
        }
    ));

    math.native_fn("min", |_, args| Ok(
        match args.iter().min() {
            Some(value) => value.clone(),
            None => Value::Null
        }
    ));

    math.native_fn("random", |_, _| Ok(Value::Float(random_usize() as f64 / usize::MAX as f64)));
    math.native_fn("randomUint", |_, _| Ok(Value::Int(random_usize() as isize)));
    math.native_fn("randomInt", |_, _| Ok(Value::Int(random_isize())));

    Value::Dict(math.allocate_value_ptr())
}

pub fn init_date(vm: &mut Vm) -> Value {
    let mut date = MapBuilder::new(vm);

    date.native_fn("now", |_, _| Ok(Value::Int(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as isize
    )));

    Value::Dict(date.allocate_value_ptr())
}

pub fn init_memory(vm: &mut Vm) -> Value {
    let mut memory = MapBuilder::new(vm);

    memory.native_fn("stackSize", |vm, _| Ok(Value::Int(vm.stack.len() as isize)));
    memory.native_fn("bytesAllocated", |vm, _| Ok(Value::Int(vm.bytes_allocated as isize)));
    memory.native_fn("objectSize", |vm, _| Ok(Value::Int(vm.objects.len() as isize)));
    memory.native_fn("nextGC", |vm, _| Ok(Value::Int(vm.next_gc as isize)));

    memory.native_fn("setNextGC", |vm, args| {
        vm.next_gc = match args.get(0) {
            Some(Value::Int(int)) => *int as usize,
            Some(Value::Float(float)) => *float as usize,
            _ => return Ok(Value::Bool(false))
        };

        Ok(Value::Bool(true))
    });

    Value::Dict(memory.allocate_value_ptr())
}

pub fn init_json(vm: &mut Vm) -> Value {
    let mut json = MapBuilder::new(vm);

    json.native_fn("stringify", |vm, args| {
        let string = vm.allocate_value_ptr(
            TinyString::new(
                args.get(0)
                    .unwrap_or_default()
                    .json_stringify()
                    .as_bytes()
            )
        );

        Ok(Value::String(string))
    });

    Value::Dict(json.allocate_value_ptr())
}