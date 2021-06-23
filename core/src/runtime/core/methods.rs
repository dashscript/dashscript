macro_rules! methods {
    ($self:expr, {$($key:expr => $value:expr,)+}) => {{
        $($self.insert(TinyString::new($key.as_bytes()), $value);)+
    }};
}

pub mod iterator {

    use crate::{Vm, Value, TinyString, ValueIter};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        methods!(vm.iterator_methods, {
            "clone" => |vm, iterator, _, _| Ok(Value::Iterator(vm.allocate_value_ptr(iterator.clone()))),
            "current" => |_, iterator, _, _| Ok(iterator.current()),
            "next" => |_, iterator, _, _| Ok(
                match iterator.next() {
                    Some(value) => value,
                    None => Value::Null
                }
            ),
        });

        let mut iterator_object = MapBuilder::new(vm);

        iterator_object.native_fn("empty", |vm, _| {
            Ok(Value::Iterator(vm.allocate_value_ptr(ValueIter::default())))
        });

        iterator_object.native_fn("from", |vm, args| {
            let iter = match args.get(0) {
                Some(value) => value.into_iter(),
                None => ValueIter::default()
            };

            Ok(Value::Iterator(vm.allocate_value_ptr(iter)))
        });

        let iterator = Value::Dict(iterator_object.allocate_value_ptr());
        vm.add_global("Iterator", iterator);
    }

}

pub mod string {

    use std::ops::Deref;
    use crate::{Vm, Value, TinyString};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        methods!(vm.string_methods, {
            "len" => |_, string, _, _| Ok(Value::Int(string.len() as isize)),
            "isEmpty" => |_, string, _, _| Ok(Value::Bool(string.len() == 0)),
            "toLowerCase" => |vm, string, _, _| Ok(Value::String(vm.allocate_string(string.deref().to_lowercase()))),
            "toUpperCase" => |vm, string, _, _| Ok(Value::String(vm.allocate_string(string.deref().to_uppercase()))),
            "trim" => |vm, string, _, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim()))),
            "trimStart" => |vm, string, _, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim_start()))),
            "trimEnd" => |vm, string, _, _| Ok(Value::String(vm.allocate_static_str(string.deref().trim_end()))),
            "charCodeAt" => |_, string, _, args| Ok(
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
            "charCount" => |_, string, _, _| {
                let mut count = 0;
                for _ in string.chars() {
                    count += 1;
                }

                Ok(Value::Int(count))
            },
            "endsWith" => |_, string, _, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(Value::String(a)) => string.deref().ends_with(a.unwrap_ref() as &str),
                    _ => false
                }
            )),
            "startsWith" => |_, string, _, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(&Value::String(a)) => string.deref().starts_with(a.unwrap_ref() as &str),
                    _ => false
                }
            )),
            "split" => |vm, string, _, args| Ok(
                match args.get(0) {
                    Some(&Value::String(a)) => {
                        let mut result = Vec::new();
                        for item in string.deref().split(a.unwrap_ref() as &str) {
                            result.push(Value::String(vm.allocate_static_str(item)));
                        }

                        Value::Array(vm.allocate_value_ptr(result))
                    },
                    _ => Value::Null
                }
            ),
            "includes" => |_, string, _, args| Ok(Value::Bool(
                match args.get(0) {
                    Some(&Value::String(a)) => string.deref().contains(a.unwrap_ref() as &str),
                    _ => false
                }
            )),
            "repeat" => |vm, string, _, args| Ok(
                match args.get(0) {
                    Some(times) => Value::String(vm.allocate_string(string.deref().repeat(times.to_usize()))),
                    _ => Value::Null
                }
            ),
            "slice" => |vm, string, _, args| Ok(
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
            "toBytes" => |vm, string, _, _| {
                let mut bytes = Vec::new();

                for byte in string.to_bytes() {
                    bytes.push(Value::Int(*byte as isize));
                }

                Ok(Value::Array(vm.allocate_value_ptr(bytes)))
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
                        Some(char_) => Value::String(vm.allocate_value_ptr(TinyString::new(char_.encode_utf8(&mut [0; 4]).as_bytes()))),
                        None => Value::Null
                    }
                },
                None => Value::Null
            }
        ));

        let string = Value::Dict(string_object.allocate_value_ptr());
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

        let boolean = Value::Dict(boolean_object.allocate_value_ptr());
        vm.add_global("Boolean", boolean);
    }

}

pub mod object {

    use crate::{Vm, Value, RuntimeError, RuntimeErrorKind};
    use crate::runtime::core::map_builder::MapBuilder; 

    pub fn init(vm: &mut Vm) {
        let mut object_ = MapBuilder::new(vm);

        object_.native_fn("defineProperty", |vm, args| {
            match args.get(0..3) {
                Some(&[Value::Dict(ptr), key, value]) => {
                    if let Some((_, true)) = ptr.unwrap_mut().insert(key, (value, false)) {
                        return Err(RuntimeError::new(vm, RuntimeErrorKind::CannotAssignToReadonlyProperty))
                    }
                },
                _ => ()
            }

            Ok(Value::Null)
        });

        object_.native_fn("defineReadonlyProperty", |_, args| {
            match args.get(0..3) {
                Some(&[Value::Dict(ptr), key, value]) => {
                    if let Some((_, true)) = ptr.unwrap_mut().insert(key, (value, true)) {
                        return Err(RuntimeError::new_untraced(RuntimeErrorKind::CannotAssignToReadonlyProperty))
                    }
                },
                _ => ()
            }

            Ok(Value::Null)
        });

        object_.native_fn("defineMethod", |vm, args| {
            match args.get(0..3) {
                Some(&[Value::Dict(ptr), key, value]) => {
                    match value {
                        Value::Function(ptr) => ptr.unwrap_mut().to_instance(),
                        _ => return Err(RuntimeError::new_str(vm, "Expected (object, string, function[non-native]) arguments."))
                    };

                    if let Some((_, true)) = ptr.unwrap_mut().insert(key, (value, true)) {
                        return Err(RuntimeError::new_kind(vm, RuntimeErrorKind::CannotAssignToReadonlyProperty))
                    }
                },
                _ => ()
            }

            Ok(Value::Null)
        });

        object_.native_fn("convertIntoReadonlyObject", |_, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => {
                    for (_, value) in ptr.unwrap_mut().iter_mut() {
                        *value = (value.0, true);
                    }

                    Value::Dict(*ptr)
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("entries", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => {
                    let mut entries = Vec::new();
                    for (key, value) in ptr.unwrap_ref() {
                        entries.push(Value::Array(vm.allocate_value_ptr(vec![*key, value.0])))
                    }

                    Value::Array(vm.allocate_value_ptr(entries))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("keys", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => {
                    let mut keys = Vec::new();
                    for (key, _) in ptr.unwrap_ref() {
                        keys.push(*key);
                    }

                    Value::Array(vm.allocate_value_ptr(keys))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("values", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => {
                    let mut values = Vec::new();
                    for (_, (value, _)) in ptr.unwrap_ref() {
                        values.push(*value);
                    }

                    Value::Array(vm.allocate_value_ptr(values))
                },
                _ => Value::Null
            }
        ));

        object_.native_fn("remove", |vm, args| {
            match args.get(0) {
                Some(Value::Dict(ptr)) => {
                    let map = ptr.unwrap_mut();
                    for key in args.get(1..).unwrap() {
                        map.remove(key);
                    }

                    Ok(Value::Null)
                },
                _ => return Err(RuntimeError::new_str(vm, "Wanted (object, ..keys) arguments."))
            }
        });

        object_.native_fn("clone", |vm, args| Ok(
            match args.get(0) {
                Some(Value::Dict(ptr)) => Value::Dict(vm.allocate_value_ptr(ptr.unwrap())),
                _ => Value::Null
            }
        ));

        let object = Value::Dict(object_.allocate_value_ptr());
        vm.add_global("Object", object);
    }

}

pub mod function {

    use crate::{Vm, Value, NativeFunction, TinyString};
    use crate::runtime::core::map_builder::MapBuilder;

    pub fn init(vm: &mut Vm) {
        let mut function_object = MapBuilder::new(vm);
        let noop = function_object.vm.allocate_value_ptr(NativeFunction {
            func: |_, _| Ok(Value::Null),
            is_instance: false,
            name: TinyString::new(b"noop")
        });

        function_object.constant("noop", Value::NativeFn(noop));

        function_object.native_fn("getName", |vm, args| {
            let name = match args.get(0) {
                Some(Value::Function(ptr)) => ptr.unwrap_ref().name.to_bytes(),
                Some(Value::NativeFn(ptr)) => ptr.unwrap_ref().name.to_bytes(),
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

        let function = Value::Dict(function_object.allocate_value_ptr());
        vm.add_global("Function", function);
    }

}

pub mod array {

    use crate::{Vm, Value, TinyString, ValuePtr};
    use crate::runtime::core::map_builder::MapBuilder;
    
    fn ptr_as_value_array(ptr: *const u8) -> Value {
        Value::Array(ValuePtr::new_unchecked(ptr))
    }

    pub fn init(vm: &mut Vm) {
        methods!(vm.array_methods, {
            "len" => |_, array, _, _| Ok(Value::Int(array.len() as isize)),
            "clone" => |vm, array, _,  _| Ok(Value::Array(vm.allocate_value_ptr(array.clone()))),
            "isEmpty" => |_, array, _, _| Ok(Value::Bool(array.len() == 0)),
            "concat" => |vm, array, ptr, args| Ok(
                match args.get(0) {
                    Some(Value::Array(ptr)) => {
                        let mut array = array.clone();
                        array.extend(ptr.unwrap_ref());

                        Value::Array(vm.allocate_value_ptr(array))
                    },
                    _ => ptr_as_value_array(ptr)
                }
            ),
            "extend" => |_, array, ptr, args| {
                array.extend_from_slice(args);
                Ok(ptr_as_value_array(ptr))
            },
            "extendFromArray" => |_, array, ptr, args| {
                match args.get(0) {
                    Some(Value::Array(ptr)) => array.extend(ptr.unwrap_ref()),
                    _ => ()
                }

                Ok(ptr_as_value_array(ptr))
            },
            "forEach" => |vm, array, _, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for item in array.iter() {
                    vm.stack.extend_from_slice(&[*item, Value::Int(index)]);
                    if let Err(error) = vm.call_function_with_returned_value(function, 2) {
                        return Err(error);
                    }

                    index += 1;
                }

                Ok(Value::Null)
            },
            "filter" => |vm, array, _, args| {
                let mut index = 0;
                let mut values = Vec::new();
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };

                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    match vm.call_function_with_returned_value(function, 2) {
                        Ok(value) => values.push(value),
                        Err(error) => return Err(error)
                    }

                    index += 1;
                }

                Ok(Value::Array(vm.allocate_value_ptr(values)))
            },
            "find" => |vm, array, _, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    match vm.call_function_with_returned_value(function, 2) {
                        Ok(value) => {
                            if value.to_bool() {
                                return Ok(item);
                            }
                        },
                        Err(error) => return Err(error)
                    }

                    index += 1;
                }

                Ok(Value::Null)
            },
            "findIndex" => |vm, array, _, args| {
                let mut index = 0;
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    match vm.call_function_with_returned_value(function, 2) {
                        Ok(value) => {
                            if value.to_bool() {
                                return Ok(Value::Int(index))
                            }
                        },
                        Err(error) => return Err(error)
                    }

                    index += 1;
                }

                Ok(Value::Int(-1))
            },
            "includes" => |_, array, _, args| {
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
            "indexOf" => |_, array, _, args| {
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
            "lastIndexOf" => |_, array, _, args| {
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
            "join" => |vm, array, _, args| {
                let seperator = match args.get(0) {
                    Some(Value::String(ptr)) if array.len() != 0 => ptr.unwrap_bytes(),
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
            "map" => |vm, array, _, args| {
                let mut index = 0;
                let mut result = Vec::new();
                let function = match args.get(0) {
                    Some(value) => *value,
                    None => return Ok(Value::Null)
                };
                
                for &item in array.iter() {
                    vm.stack.extend_from_slice(&[item, Value::Int(index)]);
                    match vm.call_function_with_returned_value(function, 2) {
                        Ok(value) => result.push(value),
                        Err(error) => return Err(error)
                    }

                    index += 1;
                }

                Ok(Value::Array(vm.allocate_value_ptr(result)))
            },
            "pop" => |_, array, _, _| {
                Ok(array.pop().unwrap_or(Value::Null))
            },
            "push" => |_, array, _, args| {
                let value = match args.get(0) {
                    Some(value) => *value,
                    None => Value::Null
                };

                array.push(value);
                Ok(value)
            },
            "reverse" => |_, array, ptr, _| {
                array.reverse();
                Ok(ptr_as_value_array(ptr))
            },
            "resize" => |_, array, ptr, args| {
                array.resize_with(match args.get(0) {
                    Some(&Value::Int(int)) if int >= 0 => int as usize,
                    Some(&Value::Float(float)) if float >= 0.0 => float as usize,
                    _ => 0
                }, || Value::Null);

                Ok(ptr_as_value_array(ptr))
            },
            "remove" => |_, array, ptr, args| {
                array.remove(match args.get(0) {
                    Some(&Value::Int(int)) if int >= 0 => int as usize,
                    Some(&Value::Float(float)) if float >= 0.0 => float as usize,
                    _ => 0
                });

                Ok(ptr_as_value_array(ptr))
            },
        });

        let mut array_object = MapBuilder::new(vm);

        array_object.native_fn("from", |vm, args| Ok(Value::Array(
            match args.get(0) {
                Some(Value::Array(ptr)) => *ptr,
                Some(Value::String(ptr)) => {
                    let mut chars = Vec::new();

                    for u32_ in ptr.unwrap_ref().chars() {
                        let char_ = match std::char::from_u32(u32_) {
                            Some(character) => character,
                            None => std::char::REPLACEMENT_CHARACTER 
                        };

                        chars.push(Value::String(vm.allocate_static_str(char_.encode_utf8(&mut [0; 4]))));
                    }

                    vm.allocate_value_ptr(chars)
                },
                Some(Value::Iterator(ptr)) => {
                    let mut items = Vec::new();
                    let iter = ptr.unwrap_mut();

                    while let Some(item) = iter.next()  {
                        items.push(item);
                    }

                    vm.allocate_value_ptr(items)
                },
                _ => vm.allocate_value_ptr(Vec::new())
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
            Ok(Value::Array(vm.allocate_value_ptr(array)))
        });

        let array = Value::Dict(array_object.allocate_value_ptr());
        vm.add_global("Array", array);
    }

}