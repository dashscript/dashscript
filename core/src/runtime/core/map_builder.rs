use std::ptr::NonNull;
use crate::{Vm, Value, Map, NativeFunction, NativeFunctionHandler, TinyString};

pub struct MapBuilder<'a> {
    pub(super) map: Map,
    pub(super) vm: &'a mut Vm
}

impl MapBuilder<'_> {

    pub fn new<'a>(vm: &'a mut Vm) -> MapBuilder<'a> {
        MapBuilder { map: Map::new(), vm }
    }

    pub fn native_fn(&mut self, name: &str, func: NativeFunctionHandler) {
        let bytes = name.as_bytes();
        let name = self.vm.allocate_str_bytes(bytes);
        let nf = NativeFunction {
            name: TinyString::new(bytes), 
            func,
            is_instance: false
        };

        let ptr = unsafe { NonNull::new_unchecked(self.vm.allocate(nf)) };
        self.map.insert(Value::String(name), (Value::NativeFn(ptr), true));
    }

    pub fn constant(&mut self, name: &str, value: Value) {
        let allocated = self.vm.allocate_str_bytes(name.as_bytes());
        self.map.insert(Value::String(allocated), (value, true));
    }

    pub fn string_constant(&mut self, name: &str, value: &str) {
        let allocated = self.vm.allocate_str_bytes(name.as_bytes());
        let allocated_constant = self.vm.allocate_str_bytes(value.as_bytes());
        self.map.insert(Value::String(allocated), (Value::String(allocated_constant), true));
    }

    pub fn char_constant(&mut self, name: &str, value: char) {
        let allocated = self.vm.allocate_str_bytes(name.as_bytes());
        let allocated_constant = self.vm.allocate_str_bytes(&[value as u8]);
        self.map.insert(Value::String(allocated), (Value::String(allocated_constant), true));
    }

    pub fn allocate(self) -> *mut u8 {
        self.vm.allocate(self.map)
    }

    pub fn allocate_non_null(self) -> NonNull<u8> {
        unsafe { NonNull::new_unchecked(self.vm.allocate(self.map)) }
    }

}