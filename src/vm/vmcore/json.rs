pub extern crate serde_json;
extern crate serde;

use std::fmt;
use std::collections::HashMap;
use serde::de::{ Deserialize, Deserializer, Visitor, SeqAccess, MapAccess, DeserializeSeed };
use crate::vm::value::{ Value, Array, Dict, ValueIndex };
use crate::vm::vm::VM;
use crate::common::fsize;
use super::result::{ ok, ValueError };
use super::builtin::panic;

struct MapKey;

impl<'de> DeserializeSeed<'de> for MapKey {
    type Value = String;

    fn deserialize<D>(self, deserializer: D) -> Result<Self::Value, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        deserializer.deserialize_str(self)
    }
}

impl<'de> Visitor<'de> for MapKey {

    type Value = String;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("JSON Map Key")
    }

    fn visit_str<E>(self, s: &str) -> Result<String, E> {
        Ok(s.to_owned())
    }

    fn visit_string<E>(self, s: String) -> Result<String, E> {
        Ok(s)
    }

}

impl<'de> Deserialize<'de> for Value {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: Deserializer<'de>,
    {
        struct ValueVisitor;

        impl<'de> Visitor<'de> for ValueVisitor {

            type Value = Value;

            fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
                formatter.write_str("JSON Value")
            }

            fn visit_bool<E>(self, val: bool) -> Result<Value, E> {
                Ok(Value::Boolean(val))
            }

            fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
                Ok(Value::Num(value as fsize))
            }

            fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
                Ok(Value::Num(value as fsize))
            }

            fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
                Ok(Value::Num(value as fsize))
            }

            fn visit_str<E>(self, value: &str) -> Result<Value, E> {
                Ok(Value::Str(value.to_string()))
            }

            fn visit_string<E>(self, value: String) -> Result<Value, E> {
                Ok(Value::Str(value))
            }

            fn visit_none<E>(self) -> Result<Value, E> {
                Ok(Value::Null)
            }

            fn visit_unit<E>(self) -> Result<Value, E> {
                Ok(Value::Null)
            }

            fn visit_some<D>(self, deserializer: D) -> Result<Value, D::Error>
            where
                D: Deserializer<'de>,
            {
                Deserialize::deserialize(deserializer)
            }

            fn visit_seq<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: SeqAccess<'de>,
            {
                let mut vec = Vec::new();
                while let Ok(Some(elem)) = visitor.next_element() {
                    vec.push(elem);
                }

                Ok(Value::Array(Array::Vec(vec, None)))
            }

            fn visit_map<V>(self, mut visitor: V) -> Result<Value, V::Error>
            where
                V: MapAccess<'de>,
            {
                match visitor.next_key_seed(MapKey)? {
                    Some(initial_key) => {
                        let mut entries = HashMap::new();
                        entries.insert(ValueIndex::Str(initial_key), (visitor.next_value().unwrap_or(Value::Null), true));

                        while let Ok(Some((key, value))) = visitor.next_entry() {
                            entries.insert(ValueIndex::Str(key), (value, true));
                        }

                        Ok(Value::Dict(Dict::Map(entries, None)))
                    },
                    None => Ok(Value::Dict(Dict::Map(HashMap::new(), None))),
                }
            }

        }

        deserializer.deserialize_any(ValueVisitor)
    }
}

pub fn stringify(value: Value, vm: &mut VM) -> String {
    match value {
        Value::Null => "null".to_string(),
        Value::Boolean(bool) => bool.to_string(),
        Value::Str(string) => format!("\"{}\"", string),
        Value::Num(number) => number.to_string(),
        Value::Array(arr) => {
            let mut content = String::new();

            for value in arr.vec(vm) {
                content += &format!(",{}", stringify(value, vm));
            }

            if content.len() == 0 { 
                "[]".to_string()
            } else {
                "[".to_string() + &content[1..] + "]"
            }
        },
        Value::Dict(dict) => {
            let mut content = String::new();

            for (key, value) in dict.entries(vm) {
                content += &format!(",{}:{}", match key {
                    ValueIndex::Str(str) => format!("\"{}\"", str),
                    ValueIndex::Num(num) => num.0.to_string(),
                    ValueIndex::Boolean(bool) => format!("\"{}\"", bool),
                    ValueIndex::Null => "\"null\"".to_string()
                }, stringify(value.0, vm));
            }

            if content.len() == 0 { 
                "{}".to_string()
            } else {
                "{".to_string() + &content[1..] + "}"
            } 
        },
        Value::Func(_, _, _, _) | Value::NativeFn(_, _) => "\"[Function]\"".to_string()
    }
}

pub fn parse_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    match serde_json::from_str(
        match args.get(0) {
            Some(Value::Str(str)) => str,
            _ => panic("InvalidArgumentError: Expected a valid 1 string type argument.".to_string(), vm)
        }
    ) {
        Ok(json) => ok(vm, json),
        Err(e) => e.to_value_error(vm)
    }
}

pub fn stringify_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Str(
        stringify(
            match args.get(0) {
                Some(val) => val.clone(),
                None => Value::Null
            }, 
            vm
        )
    )
}