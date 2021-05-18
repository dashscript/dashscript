use std::env::{self, consts};
use crate::{Vm, Dict, DictKey, Value, IntoValue, dict, func};

pub fn init(vm: &mut Vm) {
    let mut window = dict!(vm, extendable {
        "version": "1.0.0",
        "arch": consts::ARCH,
        "platform": consts::OS,
        "platformFamily": consts::FAMILY,
    });

    if vm.has_permission("read") {
        dict!(vm, extend window => {
            "filename": vm.flags.get("filename").unwrap(),
        })
    }

    if vm.has_permission("env") {
        dict!(vm, extend window => {
            "env": init_env(vm),
        })
    }

    let constant_id = vm.chunk.constants.add_string(&"window".to_string());
    vm.add_value(constant_id, dict!(window), false);
}

fn init_env(vm: &mut Vm) -> Value {
    dict!(vm, {
        "get": func!(vm, "get", |vm, args| {
            match env::var(args.get(0).unwrap_or_default().to_string()) {
                Ok(string) => Ok(Value::Str(string)),
                Err(e) => Ok(e.into_value(vm))
            }
        }),
    })
}