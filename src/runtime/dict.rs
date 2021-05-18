use std::sync::Arc;
use std::string::ToString;
use std::collections::HashMap;
use crate::{FloatLike, Value, Vm, Number, RuntimeResult};

#[derive(Clone, Hash, Debug, Eq, PartialEq)]
pub enum DictKey {
    Boolean(bool),
    Str(String),
    Float(FloatLike),
    Int(isize),
    Null
}

impl From<Number> for DictKey {
    fn from(number: Number) -> Self {
        match number {
            Number::Int(int) => Self::Int(int),
            Number::Float(float) => Self::Float(FloatLike(float))
        }
    }
}

impl ToString for DictKey {
    fn to_string(&self) -> String {
        match self {
            Self::Boolean(boolean) => boolean.to_string(),
            Self::Str(string) => string.to_owned(),
            Self::Int(int) => int.to_string(),
            Self::Float(float) => float.to_string(),
            Self::Null => "null".to_string()
        }
    }
}

impl From<Value> for DictKey {
    fn from(value: Value) -> Self { 
        match value {
            Value::Boolean(bool) => Self::Boolean(bool),
            Value::Num(number) => number.into(),
            Value::Str(str) => Self::Str(str),
            _ => Self::Null
        }
    }
}

pub type DictMap = HashMap<DictKey, (Value, bool)>;

#[derive(Clone)]
pub enum Dict {
    Map(DictMap, Option<u32>), // Map({ key: (value, is_readonly) }, Option(pointer))
    Ref(u32) // ReferenceMap(pointer)
}

impl Dict {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Dict::Map(_, id) => *id,
            Dict::Ref(id) => Some(*id)
        }
    }

    pub fn entries(&self, vm: &Vm) -> HashMap<DictKey, (Value, bool)> {
        match self {
            Dict::Map(entries, _) => entries.clone(),
            Dict::Ref(id) => match vm.get_pointer(*id) {
                Value::Dict(dict) => dict.entries(vm),
                _ => HashMap::new()
            }
        }
    }

}

#[derive(Clone, Default)]
pub struct DictBuilder {
    pub map: DictMap
}

impl DictBuilder {

    pub fn add_constant<S, V>(&mut self, name: S, value: V) 
    where
        S: ToString,
        V: Into<Value>
    {
        self.map.insert(DictKey::Str(name.to_string()), (value.into(), false));
    }

    pub fn add_function<S, F>(&mut self, name: S, func: F)
    where
        S: ToString,
        F: Fn(&mut Vm, Vec<Value>) -> RuntimeResult<Value> + Send + Sync + 'static
    {
        let name = name.to_string();
        self.map.insert(DictKey::Str(name.clone()), (Value::NativeFn { 
            name, 
            func: Arc::new(func) ,
            is_instance: false
        }, false));
    }

}

impl Into<Value> for DictBuilder {
    fn into(self) -> Value { Value::Dict(Dict::Map(self.map, None)) }
}