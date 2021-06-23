use std::{env, thread, process};
use std::fs::{self, File};
use std::time::Duration;
use crate::{Value, Vm, Map, TinyString, RuntimeError};
use super::map_builder::MapBuilder;

pub fn init(vm: &mut Vm) -> Value {
    let env = init_env(vm);
    let permissions = init_permissions(vm);
    let mut window = MapBuilder::new(vm);

    init_fs(&mut window);

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

    window.native_fn("inspect", |vm, args| {
        match args.get(0) {
            Some(value) => Ok(Value::String(vm.allocate_string(format!("{}", value)))),
            None => Err(RuntimeError::new_str(vm, "Expected (any) parameters."))
        }
    });

    Value::Dict(window.allocate_value_ptr())
}

pub fn init_env(vm: &mut Vm) -> Value {
    let mut env = MapBuilder::new(vm);

    env.native_fn("get", |vm, args| Ok(
        match args.get(0) {
            Some(Value::String(bytes)) => {
                match env::var(bytes.unwrap_ref() as &str) {
                    Ok(var) => Value::String(vm.allocate_str_bytes(var.as_bytes())),
                    Err(_) => Value::Null
                }
            },
            _ => Value::Null
        }
    ));

    env.native_fn("set", |_, args| Ok(Value::Bool(
        match args.get(0..2) {
            Some(&[Value::String(key), Value::String(value)]) => {
                env::set_var(key.unwrap_ref() as &str, value.unwrap_ref() as &str);
                true
            },
            _ => false
        }
    )));

    env.native_fn("delete", |_, args| Ok(Value::Bool(
        match args.get(0) {
            Some(Value::String(ptr)) => {
                env::remove_var(ptr.unwrap_ref() as &str);
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

        Ok(Value::Dict(vm.allocate_value_ptr(map)))
    });

    Value::Dict(env.allocate_value_ptr())
}

pub fn init_permissions(vm: &mut Vm) -> Value {
    let vm_permissions = vm.permissions;
    let mut permissions = MapBuilder::new(vm);

    permissions.constant("read", Value::Bool(vm_permissions.read));
    permissions.constant("write", Value::Bool(vm_permissions.write));
    permissions.constant("memory", Value::Bool(vm_permissions.memory));
    permissions.constant("childProcess", Value::Bool(vm_permissions.child_process));
    permissions.constant("unsafe", Value::Bool(vm_permissions.unsafe_libs));

    Value::Dict(permissions.allocate_value_ptr())
}

pub fn init_fs<'a>(window: &mut MapBuilder<'a>) {
    if window.vm.permissions.read {
        window.native_fn("cwd", |vm, _| {
            let cwd = match vm.path.parent() {
                Some(path) => {
                    match path.to_str() {
                        Some(path) => TinyString::new(path.as_bytes()),
                        None => TinyString::new(&[])
                    }
                },
                None => TinyString::new(&[])
            };
    
            Ok(Value::String(vm.allocate_value_ptr(cwd)))
        });

        window.native_fn("execPath", |vm, _| {
            let exec_path = match env::current_dir() {
                Ok(path) => {
                    match path.to_str() {
                        Some(path) => TinyString::new(path.as_bytes()),
                        None => TinyString::new(&[])
                    }
                },
                Err(error) => return Err(RuntimeError::new_io(vm, error))
            };
    
            Ok(Value::String(vm.allocate_value_ptr(exec_path)))
        });

        window.native_fn("readTextFile", |vm, args| {
            match args.get(0) {
                Some(Value::String(file_path)) => {
                    match fs::read_to_string(file_path.unwrap_ref() as &str) {
                        Ok(string) => Ok(Value::String(vm.allocate_string(string))),
                        Err(error) => Err(RuntimeError::new_io(vm, error))
                    }
                },
                _ => Err(RuntimeError::new_str(vm, "Expected (string) parameters."))
            }
        });
    }

    if window.vm.permissions.write {
        window.native_fn("chdir", |vm, args| {
            match args.get(0) {
                Some(Value::String(new_dir)) => {
                    match env::set_current_dir(new_dir.unwrap_ref() as &str) {
                        Ok(_) => Ok(Value::Null),
                        Err(error) => Err(RuntimeError::new_io(vm, error))
                    }
                },
                _ => Err(RuntimeError::new_str(vm, "Expected (string) parameters."))
            }
        });

        window.native_fn("copyFile", |vm, args| {
            match args.get(0..2) {
                Some([Value::String(from), Value::String(to)]) => {
                    match std::fs::copy(from.unwrap_ref() as &str, to.unwrap_ref() as &str) {
                        Ok(_) => Ok(Value::Null),
                        Err(error) => Err(RuntimeError::new_io(vm, error))
                    }
                },
                _ => Err(RuntimeError::new_str(vm, "Expected (string, string) parameters."))
            }
        });

        window.native_fn("createFile", |vm, args| {
            match args.get(0) {
                Some(Value::String(file_path)) => {
                    match File::create(file_path.unwrap_ref() as &str) {
                        Ok(_) => Ok(Value::Null),
                        Err(error) => Err(RuntimeError::new_io(vm, error))
                    }
                },
                _ => Err(RuntimeError::new_str(vm, "Expected (string) parameters."))
            }
        });
    }
}