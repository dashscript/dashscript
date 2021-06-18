use crate::{Value, GcHeader, Map};
use crate::runtime::memory::{unwrap_str};

impl Value {

    pub fn json_stringify(&self) -> String {
        match self {
            Value::Int(int) => int.to_string(),
            Value::Float(float) => float.to_string(),
            Value::String(string) => format!("\"{}\"", unwrap_str(string.as_ptr())),
            Value::Bool(boolean) => boolean.to_string(),
            Value::Null => "null".to_string(),
            Value::Array(array) => {
                let mut string = String::from('[');

                for item in unsafe { GcHeader::unwrap_ref::<Vec<Value>>(array.as_ptr()) } {
                    string.push_str(item.json_stringify().as_str());
                    string.push(',');
                }

                string.pop();
                string.push(']');
                string
            },
            Value::Dict(map) => {
                let mut string = String::from('{');
                
                for (key, value) in unsafe { GcHeader::unwrap_ref::<Map>(map.as_ptr()) } {
                    string.push_str(key.json_stringify().as_str());
                    string.push(':');
                    string.push_str(value.0.json_stringify().as_str());
                    string.push(',');
                }

                string.pop();
                string.push('}');
                string
            },
            Value::Function(_) | Value::NativeFn(_) => "\"[Function]\"".to_string(),
            Value::Iterator(_) => "\"[Iterator]\"".to_string()
        }
    }

}