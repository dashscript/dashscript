use crate::{Value, Vm, ToValue};

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

    pub fn vec(&self, vm: &mut Vm) -> Vec<Value> {
        match self {
            Array::Vec(vec, _) => vec.clone(),
            Array::Ref(id) => match vm.get_pointer(*id) {
                Value::Array(array) => array.vec(vm),
                _ => vec![]
            }
        }
    }

    pub fn from_iter<I, V>(iter: I) -> Vec<Value> 
    where
        I: IntoIterator<Item = V>,
        V: ToValue
    {
        let mut vector = vec![];
        for item in iter {
            vector.push(Value::from(item));
        }

        vector
    }

}