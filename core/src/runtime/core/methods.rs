macro_rules! methods {
    ($self:expr, {$($key:expr => $value:expr,)+}) => {{
        $($self.insert(TinyString::new($key.as_bytes()), $value);)+
    }};
}

pub mod iterator {

    use std::ptr::NonNull;
    use crate::{Vm, Value, TinyString, ValueIter};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        methods!(vm.iterator_methods, {
            "clone" => |vm, iterator, _| {
                let ptr = vm.allocate(iterator.clone());
                Ok(Value::Iterator(unsafe { NonNull::new_unchecked(ptr) }))
            },
            "next" => |_, iterator, _| Ok(
                match iterator.next() {
                    Some(value) => value,
                    None => Value::Null
                }
            ),
            "current" => |_, iterator, _| Ok(iterator.current()),
        });

        let mut iterator_object = MapBuilder::new(vm);

        iterator_object.native_fn("empty", |vm, _| {
            let ptr = unsafe { NonNull::new_unchecked(vm.allocate(ValueIter::default())) };
            Ok(Value::Iterator(ptr))
        });

        iterator_object.native_fn("from", |vm, args| {
            let iter = match args.get(0) {
                Some(value) => value.into_iter(),
                None => ValueIter::default()
            };

            Ok(Value::Dict(unsafe { NonNull::new_unchecked(vm.allocate(iter)) }))
        });

        let iterator = Value::Dict(iterator_object.allocate_non_null());
        vm.add_global("Iterator", iterator);
    }

}

pub mod string {

    use std::ptr::NonNull;
    use std::ops::Deref;
    use crate::{Vm, Value, TinyString};
    use crate::runtime::memory::{unwrap_str};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        methods!(vm.string_methods, {
            "len" => |_, string, _| Ok(Value::Int(string.len() as isize)),
            "isEmpty" => |_, string, _| Ok(Value::Bool(string.len() == 0)),
            "toLowerCase" => |vm, string, _| Ok(Value::String(vm.allocate_string(string.deref().to_lowercase()))),
            "toUpperCase" => |vm, string, _| Ok(Value::String(vm.allocate_string(string.deref().to_uppercase()))),
            "trim" => |vm, string, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim()))),
            "trimStart" => |vm, string, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim_start()))),
            "trimEnd" => |vm, string, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim_end()))),
            "charCodeAt" => |_, string, args| Ok(
                match args.get(0) {
                    Some(&Value::Int(index)) if index >= 0 => {
                        match string.char_code_at(index as usize) {
                            Some(code) => Value::Int(code as isize),
                            None => Value::Null
                        }
                    },
                    _ => Value::Null
                }
            ),
            "charCount" => |_, string, _| {
                let mut count = 0;
                for _ in string.chars() {
                    count += 1;
                }

                Ok(Value::Int(count))
            },
            "endsWith" => |_, string, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(&Value::String(a)) => string.deref().ends_with(unwrap_str(a.as_ptr())),
                    _ => false
                }
            )),
            "startsWith" => |_, string, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(&Value::String(a)) => string.deref().starts_with(unwrap_str(a.as_ptr())),
                    _ => false
                }
            )),
            "split" => |vm, string, args| Ok(
                match args.get(0) {
                    Some(&Value::String(a)) => {
                        let mut result = Vec::new();
                        for item in string.deref().split(unwrap_str(a.as_ptr())) {
                            result.push(Value::String(vm.allocate_static_str(item)));
                        }

                        Value::Array(unsafe { NonNull::new_unchecked(vm.allocate(result)) })
                    },
                    _ => Value::Null
                }
            ),
            "includes" => |_, string, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(&Value::String(a)) => string.deref().contains(unwrap_str(a.as_ptr())),
                    _ => false
                }
            )),
            "repeat" => |vm, string, args| Ok(
                match args.get(0) {
                    Some(times) => Value::String(vm.allocate_string(string.deref().repeat(times.to_usize()))),
                    _ => Value::Null
                }
            ),
            "slice" => |vm, string, args| Ok(
                match args.get(0..2) {
                    Some(&[start, end]) => {
                        match string.deref().get(start.to_usize()..end.to_usize()) {
                            Some(string) => Value::String(vm.allocate_str_bytes(string.as_bytes())),
                            None => Value::Null
                        }
                    },
                    _ => Value::Null
                }
            ),
            "toBytes" => |vm, string, _| {
                let mut bytes = Vec::new();

                for byte in string.to_bytes() {
                    bytes.push(Value::Int(*byte as isize));
                }

                Ok(Value::Array(unsafe { NonNull::new_unchecked(vm.allocate(bytes)) }))
            },
        });

        let mut string_object = MapBuilder::new(vm);
        let replacement_str = Value::String(string_object.vm.allocate_static_str(std::char::REPLACEMENT_CHARACTER.encode_utf8(&mut [0; 4])));

        string_object.constant("REPLACEMENT", replacement_str);

        string_object.native_fn("from", |vm, args| {
            let string = format!("{}", match args.get(0) {
                Some(value) => *value,
                None => Value::Null
            });

            Ok(Value::String(vm.allocate_string(string)))
        });

        string_object.native_fn("fromCharCode", |vm, args| Ok(
            match args.get(0) {
                Some(value) => {
                    match std::char::from_u32(value.to_u32()) {
                        Some(char_) => Value::String(vm.allocate_str(TinyString::new(char_.encode_utf8(&mut [0; 4]).as_bytes()))),
                        None => Value::Null
                    }
                },
                None => Value::Null
            }
        ));

        let string = Value::Dict(string_object.allocate_non_null());
        vm.add_global("String", string)
    }

}

pub mod boolean {

    use crate::{Vm, Value};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        let mut boolean_object = MapBuilder::new(vm);

        boolean_object.native_fn("from", |_, args| Ok(Value::Bool(
            match args.get(0) {
                Some(value) => value.to_bool(),
                None => false
            }
        )));

        let boolean = Value::Dict(boolean_object.allocate_non_null());
        vm.add_global("Boolean", boolean);
    }

}

pub mod object {

    use crate::{Vm, Value, GcHeader, Map, RuntimeError, RuntimeErrorKind};
    use crate::runtime::core::map_builder::MapBuilder; 

    pub fn init(vm: &mut Vm) {
        let mut object_ = MapBuilder::new(vm);

        object_.native_fn("defineProperty", |_, args| {
            match args.get(0..3) {
                Some(&[Value::Dict(ptr), key, value]) => unsafe {
                    if let Some((_, true)) = GcHeader::unwrap_mut::<Map>(ptr.as_ptr()).insert(key, (value, false)) {
                        return Err(RuntimeError::new_untraced(RuntimeErrorKind::CannotAssignToReadonlyProperty))
                    }
                },
                _ => ()
            }

            Ok(Value::Null)
        });

        object_.native_fn("defineReadonlyProperty", |_, args| {
            match args.get(0..3) {
                Some(&[Value::Dict(ptr), key, value]) => unsafe {
                    if let Some((_, true)) = GcHeader::unwrap_mut::<Map>(ptr.as_ptr()).insert(key, (value, true)) {
                        return Err(RuntimeError::new_untraced(RuntimeErrorKind::CannotAssignToReadonlyProperty))
                    }
                },
                _ => ()
            }

            Ok(Value::Null)
        });

        object_.native_fn("convertIntoReadonlyProperties", |_, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => unsafe {
                    for (_, value) in GcHeader::unwrap_mut::<Map>(ptr.as_ptr()).iter_mut() {
                        *value = (value.0, true);
                    }

                    Value::Dict(*ptr)
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("entries", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => unsafe {
                    let mut entries = Vec::new();
                    for (key, value) in GcHeader::unwrap_ref::<Map>(ptr.as_ptr()) {
                        entries.push(Value::Array(vm.allocate_non_null(vec![*key, value.0])))
                    }

                    Value::Array(vm.allocate_non_null(entries))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("keys", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => unsafe {
                    let mut keys = Vec::new();
                    for (key, _) in GcHeader::unwrap_ref::<Map>(ptr.as_ptr()) {
                        keys.push(*key);
                    }

                    Value::Array(vm.allocate_non_null(keys))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("values", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => unsafe {
                    let mut values = Vec::new();
                    for (_, (value, _)) in GcHeader::unwrap_ref::<Map>(ptr.as_ptr()) {
                        values.push(*value);
                    }

                    Value::Array(vm.allocate_non_null(values))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("clone", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => unsafe {
                    Value::Dict(vm.allocate_non_null(GcHeader::unwrap::<Map>(ptr.as_ptr())))
                },
                _ => Value::Null
            }
        ));

        let object = Value::Dict(object_.allocate_non_null());
        vm.add_global("Object", object);
    }

}

pub mod function {

    use crate::{Vm, Value, NativeFunction, TinyString, GcHeader, Function};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        let mut function_object = MapBuilder::new(vm);
        let noop = function_object.vm.allocate_non_null(NativeFunction {
            func: |_, _| Ok(Value::Null),
            is_instance: false,
            name: TinyString::new(b"noop")
        });

        function_object.constant("noop", Value::NativeFn(noop));

        function_object.native_fn("getName", |vm, args| unsafe {
            let name = match args.get(0) {
                Some(Value::Function(ptr)) => GcHeader::unwrap_ref::<Function>(ptr.as_ptr()).name.to_bytes(),
                Some(Value::NativeFn(ptr)) => GcHeader::unwrap_ref::<NativeFunction>(ptr.as_ptr()).name.to_bytes(),
                _ => return Ok(Value::Null)
            };

            Ok(Value::String(vm.allocate_str_bytes(name)))
        });

        function_object.native_fn("isNative", |_, args| Ok(Value::Bool(
            match args.get(0) {
                Some(Value::NativeFn(_)) => true,
                _ => false
            }
        )));

        let function = Value::Dict(function_object.allocate_non_null());
        vm.add_global("Function", function);
    }

}

pub mod array {

    use crate::{Vm, Value, GcHeader, TinyString, ValueIter};
    use crate::runtime::memory::{unwrap_tiny_string_ref, unwrap_str_bytes};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        methods!(vm.array_methods, {
            "len" => |_, array, _| Ok(Value::Int(array.len() as isize)),
            "clone" => |vm, array, _| Ok(Value::Array(vm.allocate_non_null(array.clone()))),
            "concat" => |vm, array, args| Ok(
                match args.get(0) {
                    Some(Value::Array(ptr)) => unsafe {
                        let mut array = array.clone();
                        array.extend(GcHeader::unwrap_ref::<Vec<Value>>(ptr.as_ptr()));
                        Value::Array(vm.allocate_non_null(array))
                    },
                    _ => Value::Array(vm.allocate_non_null(array.clone()))
                }
            ),
            "extend" => |_, array, args| {
                match args.get(0) {
                    Some(Value::Array(ptr)) => unsafe { array.extend(GcHeader::unwrap_ref::<Vec<Value>>(ptr.as_ptr())) },
                    _ => ()
                }

                Ok(Value::Null)
            },
            "forEach" => |vm, array, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for item in array.iter() {
                    vm.stack.extend_from_slice(&[*item, Value::Int(index)]);
                    if let Err(error) = vm.call_function(function, 2) {
                        return Err(error);
                    }

                    vm.stack.pop();
                    index += 1;
                }

                Ok(Value::Null)
            },
            "filter" => |vm, array, args| {
                let mut index = 0;
                let mut values = Vec::new();
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };

                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    if let Err(error) = vm.call_function(function, 2) {
                        return Err(error);
                    }

                    if vm.stack.pop().unwrap_or(Value::Null).to_bool() {
                        values.push(item);
                    }

                    index += 1;
                }

                Ok(Value::Array(vm.allocate_non_null(values)))
            },
            "find" => |vm, array, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    if let Err(error) = vm.call_function(function, 2) {
                        return Err(error);
                    }

                    if vm.stack.pop().unwrap_or(Value::Null).to_bool() {
                        return Ok(item);
                    }

                    index += 1;
                }

                Ok(Value::Null)
            },
            "findIndex" => |vm, array, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    if let Err(error) = vm.call_function(function, 2) {
                        return Err(error);
                    }

                    if vm.stack.pop().unwrap_or(Value::Null).to_bool() {
                        return Ok(Value::Int(index));
                    }

                    index += 1;
                }

                Ok(Value::Int(-1))
            },
            "includes" => |_, array, args| {
                let value = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for item in array.iter() {
                    if *item == value {
                        return Ok(Value::Bool(true));
                    }
                }

                Ok(Value::Bool(false))
            },
            "indexOf" => |_, array, args| {
                let mut index = 0;
                let value = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for item in array.iter() {
                    if *item == value {
                        return Ok(Value::Int(index));
                    }

                    index += 1;
                }

                Ok(Value::Int(-1))
            },
            "lastIndexOf" => |_, array, args| {
                let mut index = 0;
                let mut result = -1;
                let value = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for item in array.iter() {
                    if *item == value {
                        result = index;
                    }

                    index += 1;
                }

                Ok(Value::Int(result))
            },
            "join" => |vm, array, args| {
                let seperator = match args.get(0) {
                    Some(Value::String(ptr)) if array.len() != 0 => unwrap_str_bytes(ptr.as_ptr()),
                    _ => return Ok(Value::String(vm.allocate_str_bytes(&[])))
                };

                let mut bytes = Vec::new();
                let mut index = 0;
                let li = array.len() - 1;
                
                for item in array.iter() {
                    bytes.extend_from_slice(format!("{}", item).as_bytes());
                    bytes.extend_from_slice(seperator);

                    if index == li {
                        break
                    }

                    index += 1;
                }

                Ok(Value::String(vm.allocate_str_bytes(bytes.as_slice())))
            },
            "map" => |vm, array, args| {
                let mut index = 0;
                let mut result = Vec::new();
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    if let Err(error) = vm.call_function(function, 2) {
                        return Err(error);
                    }

                    result.push(vm.stack.pop().unwrap_or(Value::Null));
                    index += 1;
                }

                Ok(Value::Array(vm.allocate_non_null(result)))
            },
            "pop" => |_, array, _| {
                array.pop();
                Ok(Value::Null)
            },
            "push" => |_, array, args| {
                let value = match args.get(0) {
                    Some(value) => *value,
                    None => Value::Null
                };

                array.push(value);
                Ok(value)
            },
            "reverse" => |vm, array, _| {
                let mut array = array.clone();
                array.reverse();
                Ok(Value::Array(vm.allocate_non_null(array)))
            },
            "resize" => |_, array, args| {
                array.resize_with(match args.get(0) {
                    Some(&Value::Int(int)) if int >= 0 => int as usize,
                    Some(&Value::Float(float)) if float >= 0.0 => float as usize,
                    _ => 0
                }, || Value::Null);

                Ok(Value::Null)
            },
            "remove" => |_, array, args| {
                array.remove(match args.get(0) {
                    Some(&Value::Int(int)) if int >= 0 => int as usize,
                    Some(&Value::Float(float)) if float >= 0.0 => float as usize,
                    _ => 0
                });

                Ok(Value::Null)
            },
        });

        let mut array_object = MapBuilder::new(vm);

        array_object.native_fn("from", |vm, args| Ok(Value::Array(
            match args.get(0) {
                Some(Value::Array(ptr)) => *ptr,
                Some(Value::String(ptr)) => {
                    let mut chars = Vec::new();

                    for u32_ in unwrap_tiny_string_ref(ptr.as_ptr()).chars() {
                        let char_ = match std::char::from_u32(u32_) {
                            Some(character) => character,
                            None => std::char::REPLACEMENT_CHARACTER 
                        };

                        chars.push(Value::String(vm.allocate_static_str(char_.encode_utf8(&mut [0; 4]))));
                    }

                    vm.allocate_non_null(chars)
                },
                Some(Value::Iterator(ptr)) => unsafe {
                    let mut items = Vec::new();
                    let iter = GcHeader::unwrap_mut::<ValueIter>(ptr.as_ptr());

                    while let Some(item) = iter.next()  {
                        items.push(item);
                    }

                    vm.allocate_non_null(items)
                },
                _ => vm.allocate_non_null(Vec::new())
            }
        )));

        array_object.native_fn("of", |vm, args| {
            let cap = match args.get(0) {
                Some(&Value::Int(int)) if int >= 0 => int as usize,
                Some(&Value::Float(float)) if float >= 0.0 => float as usize,
                _ => 0
            };

            let mut array = Vec::with_capacity(cap);
            array.resize_with(cap, || Value::Null);
            Ok(Value::Array(vm.allocate_non_null(array)))
        });

        let array = Value::Dict(array_object.allocate_non_null());
        vm.add_global("Array", array);
    }

}