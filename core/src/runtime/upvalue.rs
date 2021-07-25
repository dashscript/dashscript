extern crate alloc;

use std::ptr::{self, NonNull};
use std::alloc::Layout;
use crate::{Value};

#[derive(Debug, Clone, Copy)]
pub enum UpvalueState {
    Open(usize),
    Closed(Value)
}

// This is something similar to Rc<Refcell<UpvalueState>>.
// But this is a custom made one.
#[derive(Debug, Clone, Copy)]
pub struct Upvalue(NonNull<u8>);

impl Upvalue {

    pub fn new(state: UpvalueState) -> Self {
        unsafe {
            let layout = Layout::new::<UpvalueState>();
            let buf = alloc::alloc::alloc(layout);
            if buf.is_null() {
                alloc::alloc::handle_alloc_error(layout);
            }

            ptr::write(buf as *mut UpvalueState, state);
            Self(NonNull::new_unchecked(buf))
        }
    }

    pub fn new_open(local: usize) -> Self {
        Self::new(UpvalueState::Open(local))
    }

    pub fn close(&self, value: Value) {
        unsafe {
            ptr::write(self.0.as_ptr() as *mut UpvalueState, UpvalueState::Closed(value))
        };
    }

    pub fn state(&self) -> UpvalueState {
        unsafe { *(self.0.as_ptr() as *const UpvalueState) }
    }

    pub unsafe fn dealloc(self) {
        alloc::alloc::dealloc(self.0.as_ptr(), Layout::new::<UpvalueState>());
    }

}