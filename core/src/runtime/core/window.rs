use std::{env, thread, process};
use std::time::Duration;
use std::ptr::NonNull;
use crate::{Value, Vm, Map, TinyString};
use crate::runtime::memory::{unwrap_str};
use super::map_builder::MapBuilder;

pub fn init(vm: &mut Vm) -> Value {
    let env = init_env(vm);
    let permissions = init_permissions(vm);
    let mut window = MapBuilder::new(vm);

    window.string_constant("version", "1.0.0-dev");
    window.string_constant("platform", env::consts::OS);
    window.string_constant("platformFamily", env::consts::FAMILY);
    window.string_constant("arch", env::consts::ARCH);
    window.constant("env", env);
    window.constant("permissions", permissions);

    window.native_fn("sleep", |_, args| {
        if let Some(value) = args.get(0) {
            let duration = value.to_usize();
            thread::sleep(Duration::from_millis(duration as u64));
        }

        Ok(Value::Null)
    });

    window.native_fn("exit", |_, args| {
        if let Some(value) = args.get(0) {
            process::exit(value.to_i32())
        }

        Ok(Value::Null)
    });

    window.native_fn("cwd", |vm, _| {
        let cwd = match vm.path.parent() {
            Some(path) => {
                match path.to_str() {
                    Some(path) => TinyString::new(path.as_bytes()),
                    None => return Ok(Value::Null)
                }
            },
            None => TinyString::new(&[])
        };

        Ok(Value::String(vm.allocate_str(cwd)))
    });

    window.native_fn("filename", |vm, _| Ok(
        match vm.path.to_str() {
            Some(path) => {
                let filename = TinyString::new(path.as_bytes());
                Value::String(vm.allocate_str(filename))
            },
            None => return Ok(Value::Null)
        }
    ));

    Value::Dict(unsafe { NonNull::new_unchecked(window.allocate()) })
}

pub fn init_env(vm: &mut Vm) -> Value {
    let mut env = MapBuilder::new(vm);

    env.native_fn("get", |vm, args| Ok(
        match args.get(0) {
            Some(Value::String(bytes)) => {
                match env::var(unwrap_str(bytes.as_ptr())) {
                    Ok(var) => Value::String(vm.allocate_str_bytes(var.as_bytes())),
                    Err(_) => Value::Null
                }
            },
            _ => Value::Null
        }
    ));

    env.native_fn("set", |_, args| Ok(Value::Bool(
        match args.get(0..2) {
            Some(&[Value::String(key_ptr), Value::String(value_ptr)]) => {
                env::set_var(unwrap_str(key_ptr.as_ptr()), unwrap_str(value_ptr.as_ptr()));
                true
            },
            _ => false
        }
    )));

    env.native_fn("delete", |_, args| Ok(Value::Bool(
        match args.get(0) {
            Some(Value::String(ptr)) => {
                env::remove_var(unwrap_str(ptr.as_ptr()));
                true
            },
            _ => false
        }
    )));

    env.native_fn("all", |vm, _| {
        let mut map = Map::new();

        for (key, value) in env::vars() {
            map.insert(Value::String(vm.allocate_string(key)), (Value::String(vm.allocate_string(value)), true));
        }

        Ok(Value::Dict(unsafe { NonNull::new_unchecked(vm.allocate(map)) }))
    });

    Value::Dict(unsafe { NonNull::new_unchecked(env.allocate()) })
}

pub fn init_permissions(vm: &mut Vm) -> Value {
    let vm_permissions = vm.permissions;
    let mut permissions = MapBuilder::new(vm);

    permissions.constant("read", Value::Bool(vm_permissions.read));
    permissions.constant("write", Value::Bool(vm_permissions.write));
    permissions.constant("memory", Value::Bool(vm_permissions.memory));
    permissions.constant("childProcess", Value::Bool(vm_permissions.child_process));
    permissions.constant("unsafe", Value::Bool(vm_permissions.unsafe_libs));

    Value::Dict(unsafe { NonNull::new_unchecked(permissions.allocate()) })
}