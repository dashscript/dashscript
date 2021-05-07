use super::value::Value;
use super::vm::VM;

#[derive(Clone)]
pub enum Array {
    Vec(Vec<Value>, Option<u32>),
    Ref(u32)
}

impl Array {

    pub fn pointer(&self) -> Option<u32> {
        match self {
            Array::Vec(_, id) => *id,
            Array::Ref(id) => Some(*id)
        }
    }

    pub fn vec(&self, vm: &mut VM) -> Vec<Value> {
        match self {
            Array::Vec(vec, _) => vec.clone(),
            Array::Ref(id) => match vm.value_stack[*id as usize].clone() {
                Value::Array(arr) => arr.vec(vm),
                _ => vec![]
            }
        }
    }

    pub fn from_str_iter<'a, I>(iter: I) -> Vec<Value> 
    where
        I: IntoIterator<Item = &'a str>
    {
        let mut vector = vec![];
        for item in iter {
            vector.push(Value::Str(item.to_owned()));
        }

        vector
    }

}