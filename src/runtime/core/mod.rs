use std::io::Error as IoError;
use std::env::VarError;
use std::sync::Arc;
use crate::{Vm, Value, NULL, DictKey, Dict};

pub mod builtin;
pub mod window;

// Easy way to create native functions
#[macro_export]
macro_rules! func {
    ($vm:expr, $name:expr, $func:expr) => {
        Value::NativeFn {
            name: $name.to_string(),
            func: std::sync::Arc::new($func),
            is_instance: false
        }
    };
}

// Easy way to create objects instead of deep syntaxes
#[macro_export]
macro_rules! dict {
    ($vm:ident, {$($key:tt: $val:expr,)+}) => {{
        let mut map = std::collections::HashMap::new();
        $(map.insert(DictKey::Str($key.to_string()), (Value::from($val).borrow($vm), false));)*
        Value::Dict(Dict::Map(map, None))
    }};

    ($vm:ident, extendable {$($key:tt: $val:expr,)+}) => {{
        let mut map = std::collections::HashMap::new();
        $(map.insert(DictKey::Str($key.to_string()), (Value::from($val).borrow($vm), false));)*
        map
    }};

    ($vm:ident, extend $map:expr => {$($key:tt: $val:expr,)+}) => {{
        $($map.insert(DictKey::Str($key.to_string()), (Value::from($val).borrow($vm), false));)*
    }};

    ($map:ident) => { Value::Dict(Dict::Map($map, None)) };
}

pub fn init(vm: &mut Vm) {
    macro_rules! add_native_function {
        ($name:expr, $value:expr) => {
            let name = $name.to_string();
            let constant = vm.chunk.constants.add_string(&name);
            vm.add_value(constant, Value::NativeFn { 
                name, 
                func: Arc::new($value),
                is_instance: false
            }, false);
        };
    }

    add_native_function!("print", |vm, args| {
        for arg in args {
            print!("{}", builtin::inspect(vm, arg)?)
        }
        
        Ok(Value::Null)
    });

    add_native_function!("println", |vm, args| {
        for arg in args {
            println!("{}", builtin::inspect(vm, arg)?)
        }

        Ok(Value::Null)
    });

    add_native_function!("typeof", |_, args| Ok(Value::Str(args.get(0).unwrap_or(&NULL).get_type())));

    window::init(vm);
}

pub trait IntoValue {
    fn into_value(&self, vm: &mut Vm) -> Value;
}

impl IntoValue for IoError {
    fn into_value(&self, _vm: &mut Vm) -> Value {
        Value::Str(format!("{:?}", self))
    }
}

impl IntoValue for VarError {
    fn into_value(&self, vm: &mut Vm) -> Value {
        match self {
            Self::NotPresent => dict!(vm, {
                "kind": "not found",
            }),
            Self::NotUnicode(os_str) => dict!(vm, {
                "kind": "not unicode",
                "osString": format!("{:?}", os_str),
            })
        }
    }
}