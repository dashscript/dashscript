use super::value::Value;
use super::array::Array;
use super::vmcore::result::{ ok, err };
use crate::common::fsize;

pub fn get_prototype(attr: &str, this: String) -> Value {
    match attr {
        "length" => Value::Num(this.chars().count() as fsize),
        "toLowerCase" => Value::NativeFn(Box::new(Value::Str(this)), |this, _, _| {
            if let Value::Str(str) = this { 
                Value::Str(str.to_lowercase()) 
            } else { Value::Null }
        }),
        "toUpperCase" => Value::NativeFn(Box::new(Value::Str(this)), |this, _, _| {
            if let Value::Str(str) = this { 
                Value::Str(str.to_uppercase()) 
            } else { Value::Null }
        }),
        "toNumber" => Value::NativeFn(Box::new(Value::Str(this)), |this, _, vm| {
            if let Value::Str(str) = this {
                match str.parse::<fsize>() {
                    Ok(num) => ok(vm, Value::Num(num)),
                    Err(_) => err(vm, Value::Str("Improper number.".to_string()))
                }
            } else {
                err(vm, Value::Str("Improper number.".to_string()))
            }
        }),
        "startsWith" => Value::NativeFn(Box::new(Value::Str(this)), |this, args, _| {
            if let Value::Str(str) = this {
                Value::Boolean(
                    str.starts_with(
                        match args.get(0) {
                            Some(Value::Str(str)) => str,
                            _ => return Value::Boolean(false)
                        }        
                    )
                )
            } else {
                Value::Boolean(false)
            }
        }),
        "endsWith" => Value::NativeFn(Box::new(Value::Str(this)), |this, args, _| {
            if let Value::Str(str) = this {
                Value::Boolean(
                    str.ends_with(
                        match args.get(0) {
                            Some(Value::Str(str)) => str,
                            _ => return Value::Boolean(false)
                        }        
                    )
                )
            } else {
                Value::Boolean(false)
            }
        }),
        "includes" => Value::NativeFn(Box::new(Value::Str(this)), |this, args, _| {
            if let Value::Str(str) = this {
                Value::Boolean(
                    str.contains(
                        match args.get(0) {
                            Some(Value::Str(str)) => str,
                            _ => return Value::Boolean(false)
                        } 
                    )
                )
            } else {
                Value::Boolean(false)
            }
        }),
        "escapeDebug" => Value::NativeFn(Box::new(Value::Str(this)), |this, _, _| {
            if let Value::Str(str) = this {
                Value::Str(str.escape_debug().to_string()) 
            } else { Value::Null }
        }),
        "trim" => Value::NativeFn(Box::new(Value::Str(this)), |this, _, _| {
            if let Value::Str(str) = this { 
                Value::Str(str.trim().to_string()) 
            } else { Value::Null }
        }),
        "slice" => Value::NativeFn(Box::new(Value::Str(this)), |this, args, _| {
            if let Value::Str(str) = this {
                Value::Str(
                    match args.get(0..2) {
                        Some([Value::Num(start), Value::Num(end)]) => {
                            let start = *start as usize;
                            let end = *end as usize;
                            let len = str.len();
                            if (start < len) && (len > end) {
                                str[start..end].to_string()
                            } else { str }
                        },
                        _ => str
                    }
                )
            } else {
                Value::Str(String::new())
            }
        }),
        // TODO(Scientific-Guy): Make a better slice function
        "split" =>  Value::NativeFn(Box::new(Value::Str(this)), |this, args, _| {
            if let Value::Str(str) = this {
                Value::Array(
                    Array::Vec(
                        match args.get(0) {
                            Some(Value::Str(separator)) => Array::from_str_iter(str.split(separator)),
                            _ => vec![]
                        },
                        None
                    )
                )    
            } else {
                Value::Null
            }
        }),
        _ => Value::Null
    }
}