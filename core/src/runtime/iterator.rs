extern crate alloc;

use alloc::alloc::Layout;
use std::ptr;
use crate::Value;

#[derive(Debug, Clone, Copy)]
pub struct ValueIter {
    ptr: *const Value,
    len: usize,
    index: usize
}

impl Default for ValueIter {
    fn default() -> Self {
        Self::new(&[])
    }
}

impl ValueIter {

    pub fn new(slice: &[Value]) -> Self {
        unsafe {
            let count = slice.len();
            let layout = Layout::array::<Value>(count).expect("Could not create a layout for a ValueIter.");
            let buf = alloc::alloc::alloc(layout);

            if buf.is_null() {
                alloc::alloc::handle_alloc_error(layout);
            }

            ptr::copy_nonoverlapping(slice.as_ptr(), buf as _, count);

            Self {
                ptr: buf as _,
                index: 0,
                len: count
            }
        }
    }

    pub fn len(&self) -> usize {
        self.len
    }

    pub fn index(&self) -> usize {
        self.index
    }

    pub fn current(&self) -> Value {
        if self.index >= self.len {
            Value::Null
        } else {
            unsafe { (*self.ptr.offset(self.index as isize)).clone() }
        }
    }

    pub fn prev(&mut self) -> Option<Value> {
        if self.index == 0 {
            return None;
        }

        self.index -= 1;
        unsafe { Some((*self.ptr.offset(self.index as isize)).clone()) }
    }

    pub unsafe fn dealloc(self) {
        if self.ptr.is_null() {
            panic!("Expected ptr to deallocate ValueIter. Maybe it was deallocated already");
        }

        ptr::drop_in_place(self.ptr as *mut &[Value]);
        alloc::alloc::dealloc(self.ptr as _, Layout::array::<Value>(self.len).unwrap());
    }

}

impl Iterator for ValueIter {
    type Item = Value;
    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.len {
            return None;
        }

        let value = unsafe { (*self.ptr.offset(self.index as isize)).clone() };
        self.index += 1;
        Some(value)
    }
}