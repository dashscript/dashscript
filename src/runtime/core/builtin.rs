use crate::{Value, RuntimeResult, Vm};

pub fn inspect(vm: &mut Vm, value: Value) -> RuntimeResult<String> {
    Ok(
        match value {
            Value::Str(string) => string,
            Value::Num(number) => number.to_string(),
            Value::Null => "null".to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::Dict(dict) => {
                let mut content = String::from("{");
                for (key, value) in dict.entries(vm) {
                    content += &format!("    {}: {},", key.to_string(), tiny_inspect(value.0))
                }
                content + "}"
            },
            Value::Array(array) => {
                let mut content = String::from("[");
                for item in array.vec(vm) {
                    content += &format!("    {},", tiny_inspect(item))
                }
                content + "]"
            },
            Value::Fn { is_async: false, .. } | Value::NativeFn { .. } => "[Function]".to_string(),
            Value::Fn { is_async: true, .. } => "[AsyncFunction]".to_string(),
        }
    )
}

pub fn tiny_inspect(value: Value) -> String {
    match value {
        Value::Str(string) => string,
        Value::Num(number) => number.to_string(),
        Value::Null => "null".to_string(),
        Value::Boolean(boolean) => boolean.to_string(),
        Value::Dict(_) => "[Object]".to_string(),
        Value::Array(_) => "[Array]".to_string(),
        Value::Fn { is_async: false, .. } | Value::NativeFn { .. } => "[Function]".to_string(),
        Value::Fn { is_async: true, .. } => "[AsyncFunction]".to_string(),
    }
}