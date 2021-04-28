// TODO(Scientific-Guy): Add more basic functions to the memory module and make this module stable
use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::common::fsize;
use super::builtin::panic;

pub fn push_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(arg) => {
            vm.value_stack.push(arg.clone());
            Value::Num(vm.value_stack.len() as fsize - 1.0)
        },
        None => panic("InvalidArgumentError: Expected a valid 1 any type argument.".to_string(), vm)
    }
}

pub fn get_by_pointer_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match args.get(0) {
        Some(Value::Num(num)) => {
            match vm.value_stack.get(*num as usize) {
                Some(val) => val.clone(),
                None => Value::Null
            }
        },
        _ => panic("InvalidArgumentError: Expected a valid 1 number(any) type argument.".to_string(), vm)
    }
}

pub fn len_api(_this: Value, _args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(vm.value_stack.len() as fsize - 1.0)
}