use super::value::Value;
use super::array::Array;

pub fn unwrap_usize(value: Option<&Value>) -> usize {
    match value {
        Some(Value::Num(num)) => *num as usize,
        _ => 0
    }
}

pub fn unwrap_value(value: Option<&Value>) -> Value {
    match value {
        Some(val) => val.clone(),
        _ => Value::Null
    }
}

pub fn unwrap_array(value: Option<&Value>) -> Array {
    match value {
        Some(Value::Array(array)) => array.clone(),
        _ => Array::Vec(vec![], None)
    }
}

pub fn unwrap_string(value: Option<&Value>) -> String {
    match value {
        Some(Value::Str(string)) => string.to_owned(),
        _ => String::new()
    }
}