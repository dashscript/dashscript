use super::value::Value;
use super::vm::VM;
use super::tools;
use super::vmcore::builtin::inspect_tiny;
use crate::common::fsize;

#[derive(Clone)]
pub enum Array {
    Vec(Vec<Value>, Option<u32>),
    Ref(u32)
}

impl Array {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Array::Vec(_, id) => *id,
            Array::Ref(id) => Some(*id)
        }
    }

    pub fn vec(&self, vm: &mut VM) -> Vec<Value> {
        match self {
            Array::Vec(vec, _) => vec.clone(),
            Array::Ref(id) => match vm.value_stack[*id as usize].clone() {
                Value::Array(arr) => arr.vec(vm),
                _ => vec![]
            }
        }
    }

    pub fn from_str_iter<'a, I>(iter: I) -> Vec<Value> 
    where
        I: IntoIterator<Item = &'a str>
    {
        let mut vector = vec![];
        for item in iter {
            vector.push(Value::Str(item.to_owned()));
        }

        vector
    }

    pub fn get_prototype(attr: &str, this: Self, vm: &mut VM) -> Value {
        macro_rules! func {
            ($function:expr) => {
                Value::NativeFn(Box::new(Value::Array(this)), $function) 
            };
        }

        match attr {
            "length" => Value::Num(this.vec(vm).len() as fsize),
            "pointer" => match this.pointer() {
                Some(pointer) => Value::Num(pointer as fsize),
                None => Value::Null
            },
            "slice" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    match args.get(0..2) {
                        Some([Value::Num(start), Value::Num(end)]) => {
                            match arr.vec(vm).get(*start as usize..*end as usize) {
                                Some(val) => Value::Array(Array::Vec(val.to_vec(), None)),
                                None => Value::Array(Array::Vec(vec![], None))
                            }
                        },
                        _ => Value::Array(Array::Vec(vec![], None))
                    }
                } else { Value::Null }
            }),
            "push" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    if let Some(pointer) = arr.pointer() {
                        let mut vector = arr.vec(vm);
                        vector.extend(args);
                        vm.value_stack[pointer as usize] = vector.into()
                    }
                }
                
                Value::Null
            }),
            "insert" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    if let Some(pointer) = arr.pointer() {
                        let mut vector = arr.vec(vm);
                        let index = tools::unwrap_usize(args.get(0));
                        if vector.len() < index {
                            vector.resize_with(index + 1, || Value::Null)
                        }

                        vector[index] = tools::unwrap_value(args.get(1));
                        vm.value_stack[pointer as usize] = vector.into()
                    }
                }
                
                Value::Null
            }),
            "extend" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    if let Some(pointer) = arr.pointer() {
                        let mut vector = arr.vec(vm);
                        vector.extend(tools::unwrap_array(args.get(0)).vec(vm));
                        vm.value_stack[pointer as usize] = vector.into()
                    }
                }
                
                Value::Null
            }),
            "includes" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    return Value::Boolean(
                        arr.vec(vm).contains(args.get(0).unwrap_or(&Value::Null))
                    );
                }
                
                Value::Null
            }),
            "join" => func!(|this, args, vm| {
                if let Value::Array(arr) = this {
                    let join = tools::unwrap_string(args.get(0));
                    let arr = arr.vec(vm);
                    let mut content = String::new();

                    return Value::Str(
                        if arr.len() == 0 {
                            String::new()
                        } else {
                            for item in arr {
                                content += &join;
                                content += &inspect_tiny(item);
                            }

                            content[join.len()..].to_owned()
                        }
                    );
                }
                
                Value::Null
            }),
            "pop" => func!(|this, _, vm| {
                if let Value::Array(arr) = this {
                    if let Some(pointer) = arr.pointer() {
                        let mut vector = arr.vec(vm);
                        vector.pop();
                        vm.value_stack[pointer as usize] = vector.into()
                    }
                }
                
                Value::Null
            }),
            "reverse" => func!(|this, _, vm| {
                if let Value::Array(arr) = this {
                    let mut vector = arr.vec(vm);
                    vector.reverse();
                    return Value::Array(Array::Vec(vector, None));
                }
                
                Value::Null
            }),
            "clone" => func!(|this, _, vm| {
                if let Value::Array(arr) = this {
                    return Value::Array(Array::Vec(arr.vec(vm), None));
                }
                
                Value::Null
            }),
            "sort" => func!(|this, _, vm| {
                if let Value::Array(arr) = this {
                    let mut vector = arr.vec(vm);
                    vector.sort();
                    return Value::Array(Array::Vec(vector, None));
                }
                
                Value::Null
            }),
            _ => Value::Null
        }
    }

}