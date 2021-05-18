use crate::{Value, Vm};

#[derive(Clone)]
pub enum Array {
    Vec(Vec<Value>, Option<u32>),
    Ref(u32)
}

impl From<Vec<Value>> for Array {
    fn from(array: Vec<Value>) -> Self {
        Self::Vec(array, None)
    }
}

impl Array {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Array::Vec(_, id) => *id,
            Array::Ref(id) => Some(*id)
        }
    }

    pub fn vec(&self, vm: &Vm) -> Vec<Value> {
        match self {
            Array::Vec(vec, _) => vec.clone(),
            Array::Ref(id) => match vm.get_pointer(*id) {
                Value::Array(array) => array.vec(vm),
                _ => vec![]
            }
        }
    }

}