use std::sync::Arc;
use crate::{Vm, Value};

pub mod builtin;

// Easy way to create objects instead of deep syntaxes
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

pub fn init(vm: &mut Vm) {
    macro_rules! add_native_function {
        ($name:expr, $value:expr) => {
            let name = String::from($name);
            let constant = vm.chunk.constants.add_string(&name);
            vm.add_value(name, Value::NativeFn {
                name: constant,
                func: Arc::new($value)
            }, false);
        };
    }

    add_native_function!("print", |vm, args| {
        for arg in args {
            println!("{}", builtin::inspect(vm, arg)?)
        }
        Ok(Value::Null)
    });
}