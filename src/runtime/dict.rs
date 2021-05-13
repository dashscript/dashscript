use std::string::ToString;
use std::collections::HashMap;
use crate::{FloatLike, Value, Vm};

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum DictKey {
    Boolean(bool),
    Str(String),
    Num(FloatLike),
    Null
}

impl ToString for DictKey {
    fn to_string(&self) -> String {
        match self {
            Self::Boolean(boolean) => boolean.to_string(),
            Self::Str(string) => string.to_owned(),
            Self::Num(number) => number.to_string(),
            Self::Null => "null".to_string()
        }
    }
}

impl From<Value> for DictKey {
    fn from(value: Value) -> Self { 
        match value {
            Value::Boolean(bool) => Self::Boolean(bool),
            Value::Num(num) => Self::Num(FloatLike(num)),
            Value::Str(str) => Self::Str(str),
            _ => Self::Null
        }
    }
}

#[derive(Clone)]
pub enum Dict {
    Map(HashMap<DictKey, (Value, bool)>, Option<u32>), // Map({ key: (value, is_readonly) }, Option(pointer))
    Ref(u32) // ReferenceMap(pointer)
}

impl Dict {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Dict::Map(_, id) => *id,
            Dict::Ref(id) => Some(*id)
        }
    }

    pub fn entries(&self, vm: &mut Vm) -> HashMap<DictKey, (Value, bool)> {
        match self {
            Dict::Map(entries, _) => entries.clone(),
            Dict::Ref(id) => match vm.get_pointer(*id) {
                Value::Dict(dict) => dict.entries(vm),
                _ => HashMap::new()
            }
        }
    }

}