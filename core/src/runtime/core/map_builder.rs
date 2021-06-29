use crate::{Vm, Value, Map, NativeFunction, NativeFunctionHandler, TinyString, ValuePtr};

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
        let nf = NativeFunction { name: TinyString::new(bytes), func };
        let ptr = self.vm.allocate_value_ptr(nf);
        self.map.insert(Value::String(name), (Value::NativeFn(ptr), true));
    }

    pub fn constant(&mut self, name: &str, value: Value) {
        let allocated = self.vm.allocate_static_str(name);
        self.map.insert(Value::String(allocated), (value, true));
    }

    pub fn string_constant(&mut self, name: &str, value: &str) {
        let allocated = self.vm.allocate_static_str(name);
        let allocated_constant = self.vm.allocate_str_bytes(value.as_bytes());
        self.map.insert(Value::String(allocated), (Value::String(allocated_constant), true));
    }

    pub fn allocate(self) -> *mut u8 {
        self.vm.allocate(self.map)
    }

    pub fn allocate_value_ptr(self) -> ValuePtr<Map> {
        self.vm.allocate_value_ptr(self.map)
    }

}

pub struct ClassBuilder<'a> {
    pub(super) map: Map,
    pub(super) prototype: Map,
    pub(super) vm: &'a mut Vm
}

impl ClassBuilder<'_> {

    pub fn new<'a>(vm: &'a mut Vm) -> ClassBuilder<'a> {
        ClassBuilder { 
            map: Map::new(), 
            prototype: Map::new(),
            vm 
        }
    }

    pub fn native_fn(&mut self, name: &str, func: NativeFunctionHandler) {
        let bytes = name.as_bytes();
        let name = self.vm.allocate_str_bytes(bytes);
        let nf = NativeFunction { name: TinyString::new(bytes), func };
        let ptr = self.vm.allocate_value_ptr(nf);
        self.map.insert(Value::String(name), (Value::NativeFn(ptr), true));
    }

    pub fn prototype_fn(&mut self, name: &str, func: NativeFunctionHandler) {
        let bytes = name.as_bytes();
        let name = self.vm.allocate_str_bytes(bytes);
        let nf = NativeFunction { name: TinyString::new(bytes), func };
        let ptr = self.vm.allocate_value_ptr(nf);
        self.prototype.insert(Value::String(name), (Value::NativeFn(ptr), true));
    }

    pub fn init(&mut self, func: NativeFunctionHandler) {
        self.native_fn("init", func);
    }

    pub fn constant(&mut self, name: &str, value: Value) {
        let allocated = self.vm.allocate_static_str(name);
        self.map.insert(Value::String(allocated), (value, true));
    }

    pub fn string_constant(&mut self, name: &str, value: &str) {
        let allocated = self.vm.allocate_static_str(name);
        let allocated_constant = self.vm.allocate_str_bytes(value.as_bytes());
        self.map.insert(Value::String(allocated), (Value::String(allocated_constant), true));
    }

    pub fn allocate_value_ptr(mut self) -> ValuePtr<Map> {
        let prototype = Value::Dict(self.vm.allocate_value_ptr(self.prototype));
        self.map.insert(self.vm.constants.prototype, (prototype, true));
        self.vm.allocate_value_ptr(self.map)
    }

    pub fn allocate_value_ptr_with_prototype(mut self) -> (ValuePtr<Map>, ValuePtr<Map>) {
        let prototype = self.vm.allocate_value_ptr(self.prototype);
        self.map.insert(self.vm.constants.prototype, (Value::Dict(prototype), true));
        (self.vm.allocate_value_ptr(self.map), prototype)
    }

}