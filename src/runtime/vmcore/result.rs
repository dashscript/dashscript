use std::collections::HashMap;
use std::io::Error;
use std::env::VarError;
use serde_json::Error as JsonError;
use crate::vm::value::{ Value, ValueIndex, Dict, NativeFn };
use crate::vm::vm::VM;
use crate::dict;
use super::builtin::panic;

pub trait ValueError {
    fn to_value_error(&self, vm: &mut VM) -> Value;
}

impl ValueError for Error {
    fn to_value_error(&self, vm: &mut VM) -> Value {
        let content = dict!(vm, {
            "kind": format!("{:?}", self.kind()),
            "message": format!("{:?}", self),
        });

        return err(vm, content);
    }
}

impl ValueError for VarError {
    fn to_value_error(&self, vm: &mut VM) -> Value {
        match self {
            VarError::NotPresent => err(vm, Value::Str("NotFound".to_string())),
            VarError::NotUnicode(_) => err(vm, Value::Str("NotUnicode".to_string())),
        }
    }
}

impl ValueError for JsonError {
    fn to_value_error(&self, vm: &mut VM) -> Value {
        let content = dict!(vm, {
            "line": self.line(),
            "column": self.column(),
            "type": format!("{:?}", self.classify()),
        });

        err(vm, content)
    }
}

pub fn ok(vm: &mut VM, val: Value) -> Value {
    dict!(vm, {
        "isOk": true,
        "isErr": false,
        "unwrap": (val.clone(), (|this, _, _| this) as NativeFn),
        "unwrapError": (|_, _, vm| panic("UnwrapError: Unwrapping an error when the result is ok.".to_string(), vm)) as NativeFn,
        "unwrapOr": (val, (|this, _, _| this) as NativeFn),
    })
}

pub fn err(vm: &mut VM, val: Value) -> Value {
    dict!(vm, {
        "isOk": false,
        "isErr": true,
        "unwrapError": (val, (|this, _, _| this) as NativeFn),
        "unwrap": (|_, _, vm| panic("UnwrapError: Unwrapping result when the result is err.".to_string(), vm)) as NativeFn,
        "unwrapOr": (|_, args, _| match args.get(0) {
            Some(val) => val.clone(),
            None => Value::Null
        }) as NativeFn,
    })
}

pub fn ok_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    ok(vm, match args.get(0) {
        Some(val) => val.clone(),
        _ => Value::Null
    })
}

pub fn err_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    err(vm, match args.get(0) {
        Some(val) => val.clone(),
        _ => Value::Null
    })
}