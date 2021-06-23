use std::ptr::NonNull;
use std::collections::HashMap;
use std::fmt::{self, Formatter};
use serde::de::{Deserialize, Deserializer, Visitor, SeqAccess, MapAccess, DeserializeSeed};
use crate::{TinyString, Vm, Value};

struct MapKey<'a>(&'a mut Vm);

impl<'a, 'de> DeserializeSeed<'de> for MapKey<'a> {
    type Value = TinyString;
    fn deserialize<D: Deserializer<'de>>(self, deserializer: D) -> Result<Self::Value, D::Error> {
        deserializer.deserialize_str(self)
    }
}

impl<'a, 'de> Visitor<'de> for MapKey<'a> {
    type Value = TinyString;

    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("JSON Map Key")
    }

    fn visit_str<E>(self, s: &str) -> Result<TinyString, E> {
        Ok(TinyString::from(s))
    }

    fn visit_string<E>(self, s: String) -> Result<TinyString, E> {
        Ok(TinyString::new(s.as_bytes()))
    }
}

struct ValueVisitor<'a>(&'a mut Vm);

impl<'a, 'de> Visitor<'de> for ValueVisitor<'a> {
    type Value = Value;

    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("JSON Value")
    }

    fn visit_bool<E>(self, val: bool) -> Result<Value, E> {
        Ok(Value::Bool(val))
    }

    fn visit_i64<E>(self, value: i64) -> Result<Value, E> {
        Ok(Value::Int(value as isize))
    }

    fn visit_u64<E>(self, value: u64) -> Result<Value, E> {
        Ok(Value::Int(value as isize))
    }

    fn visit_f64<E>(self, value: f64) -> Result<Value, E> {
        Ok(Value::Float(value))
    }

    fn visit_str<E>(self, value: &str) -> Result<Value, E> {
        Ok(Value::String(self.0.allocate_str_bytes(value.as_bytes())))
    }

    fn visit_string<E>(self, value: String) -> Result<Value, E> {
        Ok(Value::String(self.0.allocate_str_bytes(value.as_bytes())))
    }

    fn visit_none<E>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_unit<E>(self) -> Result<Value, E> {
        Ok(Value::Null)
    }

    fn visit_some<D: Deserializer<'de>>(self, deserializer: D) -> Result<Value, D::Error> {
        Deserialize::deserialize(deserializer)
    }

    fn visit_seq<V: SeqAccess<'de>>(self, mut visitor: V) -> Result<Value, V::Error> {
        let mut vec = Vec::new();
        while let Ok(Some(item)) = visitor.next_element() {
            vec.push(item);
        }

        Ok(Value::Array(unsafe { NonNull::new_unchecked(self.0.allocate(vec)) }))
    }

    fn visit_map<V: MapAccess<'de>>(self, mut visitor: V) -> Result<Value, V::Error> {
        match visitor.next_key_seed(MapKey(self.0))? {
            Some(initial_key) => {
                let mut entries = HashMap::new();

                macro_rules! insert {
                    ($key:expr, $value:expr) => {{
                        let ptr = unsafe { NonNull::new_unchecked(self.0.allocate($key)) };
                        entries.insert(Value::String(ptr), ($value, true));
                    }};
                }

                insert!(initial_key, visitor.next_value().unwrap_or_default());

                while let Ok(Some((key, value))) = visitor.next_entry() {
                    insert!(key, value);
                }

                Ok(Value::Dict(unsafe { NonNull::new_unchecked(self.0.allocate(entries)) }))
            },
            None => Ok(Value::Dict(self.0.allocate_empty_map())),
        }
    }

}

struct TinyStringVisitor;

impl<'de> Visitor<'de> for TinyStringVisitor {
    type Value = TinyString;

    fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
        formatter.write_str("JSON String")
    }

    fn visit_str<E>(self, value: &str) -> Result<TinyString, E> {
        Ok(TinyString::from(value))
    }

    fn visit_string<E>(self, value: String) -> Result<TinyString, E> {
        Ok(TinyString::new(value.as_bytes()))
    }
}

impl<'de> Deserialize<'de> for TinyString {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        deserializer.deserialize_any(TinyStringVisitor)
    }
}