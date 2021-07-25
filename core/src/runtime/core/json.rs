use crate::Value;

impl Value {

    pub fn json_stringify(&self) -> String {
        match self {
            Value::Int(int) => int.to_string(),
            Value::Float(float) => float.to_string(),
            Value::String(string) => format!("\"{}\"", string.unwrap_ref()),
            Value::Bool(boolean) => boolean.to_string(),
            Value::Null => "null".to_string(),
            Value::Array(array) => {
                let mut string = String::from('[');

                for item in array.unwrap_ref() {
                    string.push_str(item.json_stringify().as_str());
                    string.push(',');
                }

                string.pop();
                string.push(']');
                string
            },
            Value::Dict(map) => {
                let mut string = String::from('{');
                
                for (key, value) in map.unwrap_ref() {
                    string.push_str(key.json_stringify().as_str());
                    string.push(':');
                    string.push_str(value.0.json_stringify().as_str());
                    string.push(',');
                }

                string.pop();
                string.push('}');
                string
            },
            Value::Instance(instance) => {
                let mut string = String::from('{');
                
                for (key, value) in instance.unwrap_ref().properties.iter() {
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
            Value::Iterator(_) => "\"[Iterator]\"".to_string(),
            Value::Promise(promise) => format!("\"[Promise<{}>]\"", promise.unwrap_str())
        }
    }

}