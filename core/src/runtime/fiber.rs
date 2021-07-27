extern crate alloc;

use std::alloc::Layout;
use std::ops::{Deref, DerefMut};
use std::fmt::{self, Debug, Formatter};
use crate::{Value, TinyString, Upvalue, UpvalueState};

// The CallFrame is where the details of a paticular call state exists.
#[derive(Debug, Clone)]
pub struct CallFrame {
    name: TinyString,
    pub(crate) upvalues: Vec<Upvalue>,
    pub(crate) stack_start: usize,
    max_slots: u8,
    pub(crate) ip: usize // This would be 0 if the call frame belongs to a native function
}

// Fiber contains the main runtime context for the VM. It is like a thread to the VM.
#[derive(Debug, Clone)]
pub struct Fiber {
    pub(crate) stack: Vec<Value>,
    pub(crate) ip: usize,
    pub(crate) frame: *mut CallFrame,
    frames: Vec<CallFrame>,
    upper: Option<FiberHandle>,
    borrow_count: u32,
    pub(crate) open_upvalues: Vec<Upvalue>
}

impl Fiber {

    pub fn new<N>(name: N, max_slots: u8, ip: usize) -> Self
    where 
        N: Into<TinyString>
    {
        let stack = vec![Value::Null; max_slots as usize];
        let mut frames = Vec::with_capacity(1);
        let mut fiber = Fiber {
            frame: frames.as_mut_ptr(),
            stack,
            frames,
            ip,
            borrow_count: 0,
            upper: None,
            open_upvalues: Vec::new()
        };

        fiber.new_frame(name, max_slots, &[]); // The initial default frame
        fiber
    }

    pub fn frame_mut(&self) -> &mut CallFrame {
        unsafe { &mut *self.frame }
    }

    pub fn frame(&self) -> &CallFrame {
        unsafe { &*self.frame }
    }

    pub fn new_frame<N>(&mut self, name: N, max_slots: u8, upvalues: &[Upvalue])
    where
        N: Into<TinyString>
    {
        self.new_frame_with_offset_and_ip(name, max_slots, upvalues, 0, self.ip)
    }

    pub fn new_frame_with_offset<N>(&mut self, name: N, max_slots: u8, upvalues: &[Upvalue], offset: usize)
    where
        N: Into<TinyString>
    {
        self.new_frame_with_offset_and_ip(name, max_slots, upvalues, offset, self.ip)
    }

    pub fn new_frame_with_offset_and_ip<N>(&mut self, name: N, max_slots: u8, upvalues: &[Upvalue], offset: usize, ip: usize)
    where
        N: Into<TinyString>
    {
        let stack_start = self.stack.len() - offset;

        self.stack.resize(stack_start + max_slots as usize, Value::Null);
        self.frames.push(CallFrame {
            name: name.into(),
            upvalues: upvalues.to_vec(),
            stack_start,
            max_slots,
            ip
        });

        self.frame = unsafe { self.frames.as_mut_ptr().add(self.frames.len() - 1) };
    }

    pub fn slot(&self, slot: u8) -> Value {
        self.stack[self.frame().stack_start + slot as usize]
    }

    pub fn set_slot(&mut self, slot: u8, value: Value) {
        let index = self.frame().stack_start + slot as usize;
        if self.stack.len() >= index {
            self.stack.resize(index + 1, Value::Null);
        } 
        
        self.stack[index] = value;
    }

    pub fn set_upslot(&mut self, index: usize, value: Value) {
        if let Some(fiber) = &mut self.upper {
            fiber.stack[index] = value
        }
    }

    pub fn upvalue(&self, index: u8) -> Value {
        let frame = self.frame();

        match frame.upvalues.get(index as usize) {
            Some(upvalue) => {
                match upvalue.state() {
                    UpvalueState::Open(index) => {
                        match &self.upper {
                            Some(fiber) => Value::from(fiber.stack.get(index)),
                            _ => Value::Null
                        }
                    },
                    UpvalueState::Closed(value) => value
                }
            },
            None => Value::Null
        }
    }

    pub fn set_upvalue(&mut self, index: u8, value: Value) {
        let frame = self.frame();

        if let Some(upvalue) = frame.upvalues.get(index as usize) {
            match upvalue.state() {
                UpvalueState::Open(index) => {
                    if let Some(fiber) = &mut self.upper {
                        fiber.stack[index] = value;
                    }
                },
                UpvalueState::Closed(value) => upvalue.close(value)
            }
        }
    }

    pub fn child<N>(&mut self, name: N, max_slots: u8, ip: usize) -> Self
    where 
        N: Into<TinyString>
    {
        let mut fiber = Self::new(name, max_slots, ip);
        fiber.upper = Some(FiberHandle::from_ptr(self as *mut Self));
        fiber
    }

    pub fn complete_trace(&self) -> Vec<TinyString> {
        let mut frames = Vec::new();
        
        for frame in &self.frames {
            frames.push(frame.name.clone());
        }

        let mut upper = &self.upper;

        while let Some(fiber) = upper {
            for frame in &fiber.frames {
                frames.push(frame.name.clone());
            }

            upper = &fiber.upper;
        }

        frames.reverse();
        frames
    }

    pub fn collect_upvalues<F>(&mut self, func: F)
    where
        F: Fn(&Value)
    {
        let mut upper = Some(self as *mut Self);

        unsafe {
            while let Some(ptr) = upper {
                let fiber = &*ptr;

                for frame in &fiber.frames {
                    for upvalue in &frame.upvalues {
                        if let UpvalueState::Closed(value) = upvalue.state() {
                            func(&value)
                        }
                    }
                }

                upper = match &fiber.upper {
                    Some(fiber) => Some(fiber.0),
                    None => None
                }
            }
        }
    }

    pub fn pop_frame(&mut self) -> CallFrame {
        let frame = self.frames.pop()
            .expect("[Fiber]: Fiber has no more call frames to pop.");

        match self.frames.last_mut() {
            Some(frame) => self.frame = frame as *mut CallFrame,
            None => panic!("[Fiber]: Fiber has no more call frames to pop.")
        }

        frame
    }

    pub fn pop(&mut self) -> Value {
        match self.stack.pop() {
            Some(value) => value,
            None => Value::Null
        }
    }

    pub fn pop_twice(&mut self) -> (Value, Value) {
        unsafe {
            let length = self.stack.len();

            if length < 2 {
                panic!("[Fiber]: Stack went 2 values in short.")
            } else {
                self.stack.set_len(length - 2);
                let ptr = self.stack.as_ptr().add(length - 1);
                (*ptr.sub(1), *ptr) // (lhs, rhs)
            }
        }
    }

    pub fn pop_thrice(&mut self) -> (Value, Value, Value) {
        unsafe {
            let length = self.stack.len();

            if length < 3 {
                panic!("[Fiber]: Stack went 3 values in short.")
            } else {
                self.stack.set_len(length - 3);
                let ptr = self.stack.as_ptr().add(length - 1);
                (*ptr.sub(2), *ptr.sub(1), *ptr)
            }
        }
    }

    pub fn last(&mut self) -> Option<Value> {
        let length = self.stack.len();

        if length == 0 {
            None
        } else {
            unsafe { Some(*self.stack.as_ptr().add(length - 1)) }
        }
    }

    pub fn last_or_null(&mut self) -> Value {
        let length = self.stack.len();

        if length == 0 {
            Value::Null
        } else {
            unsafe { *self.stack.as_ptr().add(length - 1) }
        }
    }

    pub fn drain(&mut self, count: usize) -> Vec<Value> {
        let new_len = self.stack.len() - count;
        let result = match self.stack.get(new_len..) {
            Some(x) => Vec::from(x),
            None => {
                let cloned = self.stack.clone();
                self.stack.clear();
                cloned
            }
        };

        self.stack.truncate(new_len);
        result
    }

    pub fn drain_strict(&mut self, count: usize) -> Vec<Value> {
        let new_len = self.stack.len() - count;
        let result = match self.stack.get(new_len..) {
            Some(x) => Vec::from(x),
            None => panic!("[Fiber]: Stack went {} values short in count.", count)
        };

        self.stack.truncate(new_len);
        result
    }

    pub fn close_upvalue(&mut self) {
        let closed_index = self.stack.len() - 1;
        let closed = self.pop();
        let mut retain = self.open_upvalues.len();

        for upvalue in self.open_upvalues.iter().rev() {
            match upvalue.state() {
                UpvalueState::Open(index) if index == closed_index => upvalue.close(closed),
                UpvalueState::Closed(_) => (),
                _ => break
            }

            retain -= 1;
        }

        self.open_upvalues.truncate(retain);
    }

    pub fn close_frame(&mut self) {
        let frame = self.pop_frame();
        let mut retain = self.open_upvalues.len();

        for upvalue in self.open_upvalues.iter().rev() {
            if let UpvalueState::Open(index) = upvalue.state() {
                if frame.stack_start <= index {
                    upvalue.close(self.stack[index])
                } else { 
                    break 
                }
            }

            retain -= 1;
        }

        self.stack.drain(frame.stack_start..);
        self.open_upvalues.truncate(retain);
        self.ip = frame.ip;
    }

    pub fn upper(&self) -> FiberHandle {
        self.upper.as_ref()
            .unwrap()
            .clone()
    }

}

// The handle for the fiber but with a pointer.
pub struct FiberHandle(*mut Fiber);

impl FiberHandle {

    pub fn new(mut fiber: Fiber) -> Self {
        unsafe {
            fiber.borrow_count += 1;
            let layout = Layout::new::<Fiber>();
            let buf = alloc::alloc::alloc(layout) as *mut Fiber;

            if buf.is_null() {
                alloc::alloc::handle_alloc_error(layout)
            }

            std::ptr::write(buf, fiber);
            Self(buf)
        }
    }

    pub fn from_ptr(ptr: *mut Fiber) -> Self {
        // Increase the borrow count which is similar to cloning.
        unsafe { (*ptr).borrow_count += 1; }
        Self(ptr)
    }

}

impl Debug for FiberHandle {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Drop for FiberHandle {
    fn drop(&mut self) {
        self.borrow_count -= 1;

        // Incase if the Fiber is no more borrowed by any FiberHandle, that means
        // that the last FiberHandle has been dropped and the Fiber can be deallocated.
        if self.borrow_count == 0 {
            unsafe { alloc::alloc::dealloc(self.0 as _, Layout::new::<Fiber>()) }
        }
    }
}

impl Clone for FiberHandle {
    fn clone(&self) -> Self {
        // Increase the borrow count as it is been borrowed by another cloned FiberHandle
        unsafe { (*self.0).borrow_count += 1; }
        Self(self.0)
    }
}

impl Deref for FiberHandle {
    type Target = Fiber;
    fn deref(&self) -> &Fiber {
        unsafe { &*self.0 }
    }
}

impl DerefMut for FiberHandle {
    fn deref_mut(&mut self) -> &mut Fiber {
        unsafe { &mut *self.0 }
    }
}