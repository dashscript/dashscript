use crate::{Value, RuntimeResult, Vm};

pub fn inspect(vm: &mut Vm, value: Value) -> RuntimeResult<String> {
    Ok(
        match value {
            Value::Str(string) => string,
            Value::Num(number) => number.to_string(),
            Value::Null => "null".to_string(),
            Value::Boolean(boolean) => boolean.to_string(),
            Value::Dict(dict) => {
                let mut content = String::from("{\n");
                for (key, value) in dict.entries(vm) {
                    content += &format!("    {}: {},\n", key.to_string(), value.0.to_string())
                }
                content + "}"
            },
            Value::Array(array) => {
                let mut content = String::from("[\n");
                for item in array.vec(vm) {
                    content += &format!("    {},\n", item.to_string())
                }
                content + "]"
            },
            Value::Fn { is_async: false, .. } | Value::NativeFn { .. } => "[Function]".to_string(),
            Value::Fn { is_async: true, .. } => "[AsyncFunction]".to_string(),
        }
    )
}