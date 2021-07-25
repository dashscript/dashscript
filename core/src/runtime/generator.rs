use crate::{Vm, Value};

pub struct Generator<'a> {
    stack: Vec<Value>,
    vm: &'a mut Vm
}