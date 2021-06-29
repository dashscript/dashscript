use std::{env, thread, process};
use std::fs::{self, File};
use std::time::Duration;
use std::process::{Command};
use crate::{Value, Vm, Map, TinyString, RuntimeError, RuntimeResult};
use crate::runtime::resources::{ChildResource, ChildStdinResource, ChildStdoutResource, ChildStderrResource};
use super::builtin::{initiate_process_instance};
use super::map_builder::MapBuilder;

pub fn init(vm: &mut Vm) -> Value {
    let env = init_env(vm);
    let permissions = init_permissions(vm);
    let mut window = MapBuilder::new(vm);

    init_fs(&mut window);
    init_process(&mut window);

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
            None => Err(RuntimeError::new(vm, "Expected (any) parameters."))
        }
    });

    window.native_fn("close", |vm, args| {
        match args.get(0) {
            Some(Value::Int(rid)) => {
                if let Some(resource) = vm.resource_table.remove(&(*rid as u32)) {
                    if let Err(kind) = resource.close() {
                        return Err(RuntimeError::new(vm, kind))
                    }
                }

                Ok(Value::Null)
            },
            _ => return Err(RuntimeError::new(vm, "[window.close]: Expected (rid) arguments."))
        }
    });

    window.native_fn("flush", |vm, args| {
        match args.get(0) {
            Some(Value::Int(rid)) => {
                if let Some(resource) = vm.get_io_resource(*rid as u32) {
                    if let Err(kind) = resource.flush() {
                        return Err(RuntimeError::new(vm, kind))
                    }
                }

                Ok(Value::Null)
            },
            _ => return Err(RuntimeError::new(vm, "[window.flush]: Expected (rid) arguments."))
        }
    });

    window.native_fn("write", |vm, args| {
        match args.get(0..2) {
            Some([Value::Int(rid), Value::Array(bytes)]) => {
                if let Some(resource) = vm.get_io_resource(*rid as u32) {
                    match resource.write(&*bytes.unwrap_bytes()) {
                        Ok(n) => Ok(Value::Int(n as _)),
                        Err(kind) => Err(RuntimeError::new(vm, kind))
                    }
                } else { 
                    Ok(Value::Null) 
                }
            },
            _ => return Err(RuntimeError::new(vm, "[window.write]: Expected (rid, array[u8]) arguments."))
        }
    });

    window.native_fn("read", |vm, args| {
        match args.get(0..2) {
            Some([Value::Int(rid), Value::Array(bytes)]) => {
                if let Some(resource) = vm.get_io_resource(*rid as u32) {
                    let mut buf = bytes.unwrap_bytes();
                    match resource.read(&mut buf) {
                        Ok(n) => {
                            bytes.write_bytes(&*buf);
                            Ok(Value::Int(n as _))
                        },
                        Err(kind) => Err(RuntimeError::new(vm, kind))
                    }
                } else { 
                    Ok(Value::Null) 
                }
            },
            _ => return Err(RuntimeError::new(vm, "[window.read]: Expected (rid, array[u8]) arguments."))
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
                _ => Err(RuntimeError::new(vm, "[window.readTextFile]: Expected (string) parameters."))
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
                _ => Err(RuntimeError::new(vm, "[window.chdir]: Expected (string) parameters."))
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
                _ => Err(RuntimeError::new(vm, "[window.copyFile]: Expected (string, string) parameters."))
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
                _ => Err(RuntimeError::new(vm, "[window.createFile]: Expected (string) parameters."))
            }
        });
    }
}

pub fn init_process<'a>(window: &mut MapBuilder<'a>) {
    if !window.vm.permissions.child_process {
        return;
    }

    window.native_fn("run", |vm, args| {
        let mut command = resolve_run_args(vm, args)?;
        let mut child = match command.spawn() {
            Ok(child) => child,
            Err(error) => return Err(RuntimeError::new_io(vm, error))
        };

        let pid = child.id();

        macro_rules! define_stdio_rid {
            ($($var:ident $key:ident => $type:tt)+) => {
                $(let $var = match child.$key.take() {
                    Some($var) => Some(vm.add_resource($type(Box::new($var)))),
                    None => None
                };)+
            };
        }

        define_stdio_rid! {
            stdout_rid stdout => ChildStdoutResource
            stdin_rid stdin => ChildStdinResource
            stderr_rid stderr => ChildStderrResource
        }

        let rid = vm.add_resource(ChildResource(Box::new(child)));
        Ok(initiate_process_instance(vm, rid, pid, stdout_rid, stdin_rid, stderr_rid))
    });
}

fn stdio_map(string: &str) -> Option<std::process::Stdio> {
    use std::process::Stdio;

    match string {
        "inherit" => Some(Stdio::inherit()),
        "piped" => Some(Stdio::piped()),
        "null" => Some(Stdio::null()),
        _ => None
    }
}

fn resolve_run_args(vm: &mut Vm, args: &[Value]) -> RuntimeResult<Command> {
    match args.get(0) {
        Some(Value::Dict(options_ptr)) => {
            let map = options_ptr.unwrap_ref();
            match map.get(&vm.constants.cmd) {
                Some((Value::Array(ptr), _)) => {
                    let args = ptr.unwrap_ref();
                    let mut command = Command::new(args.get(0).unwrap_or_default());

                    for arg in &args[1..] {
                        command.arg(arg);
                    }

                    if let Some(cwd) = map.get(&vm.constants.cwd) {
                        command.current_dir(cwd.0);
                    }

                    if let Some((Value::Dict(env_ptr), _)) = map.get(&vm.constants.env) {
                        for (key, value) in env_ptr.unwrap_ref() {
                            command.env(key, value.0);
                        }
                    }

                    macro_rules! if_let_stdio {
                        ($($key:ident)+) => {
                            $(if let Some((value, _)) = map.get(&vm.constants.$key) {
                                if let Value::String(string) = value {
                                    if let Some(stdio) = stdio_map(string.unwrap_ref()) {
                                        command.$key(stdio);
                                    }
                                }
                            })+
                        };
                    }

                    if_let_stdio! { stdin stdout stderr }

                    Ok(command)
                }
                _ => Err(RuntimeError::new(vm, "[window.run]: Expected `cmd` field as array in the options."))
            }
        },
        _ => Err(RuntimeError::new(vm, "[window.run]: Expected (object[options]) arguments."))
    }
}