use crate::vm::vm::VM;
use crate::vm::value::Value;

pub mod builtin;
pub mod window;
pub mod result;
pub mod math;
pub mod date;
pub mod json;

#[macro_export]
macro_rules! dict {
    ($vm:ident, {$($key:tt: $val:expr,)+}) => {{
        let mut map = HashMap::new();

        for data in vec![$(($key, Value::from($val))),*] {
            map.insert(
                ValueIndex::Str(data.0.to_string()), 
                (data.1.borrow($vm), false)
            );
        }
    
        Value::Dict(Dict::Map(map, None))
    }};

    ($vm:ident, extendable {$($key:tt: $val:expr,)+}) => {{
        let mut map = HashMap::new();

        for data in vec![$(($key, Value::from($val))),*] {
            map.insert(
                ValueIndex::Str(data.0.to_string()), 
                (data.1.borrow($vm), false)
            );
        }
    
        map
    }};

    ($vm:ident, extend $map:expr => {$($key:tt: $val:expr,)+}) => {{
        for data in vec![$(($key, Value::from($val))),*] {
            $map.insert(
                ValueIndex::Str(data.0.to_string()), 
                (data.1.borrow($vm), false)
            );
        }
    }};

    ($map:ident) => { Value::Dict(Dict::Map($map, None)) };
}

// Arithmetic operations
pub fn add_values(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Str(a), Value::Str(b)) => Value::Str(a + b.as_str()),
        (Value::Num(a), Value::Num(b)) => Value::Num(a + b),
        (Value::Null, Value::Num(b)) => Value::Num(b),
        (a, b) => Value::Str(builtin::inspect_tiny(a) + builtin::inspect_tiny(b).as_str())
    }
}

pub fn sub_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a - b),
        (Value::Null, Value::Num(b)) => Value::Num(0.0 - b),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot subtract value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn mult_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a * b),
        (Value::Str(a), Value::Num(b)) => Value::Str(a.repeat(b as usize)),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot multiply value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn div_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a / b),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot divide value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}

pub fn pow_values(a: Value, b: Value, vm: &mut VM) -> Value {
    match (a, b) {
        (Value::Num(a), Value::Num(b)) => Value::Num(a.powf(b)),
        (a, b) => builtin::panic(
            format!("ImproperArithmeticOperation: You cannot power value of type {} with type {}.", a.type_as_str(), b.type_as_str()),
            vm
        )
    }
}