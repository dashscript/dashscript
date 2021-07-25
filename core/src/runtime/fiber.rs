use crate::{Value, TinyString, Upvalue, UpvalueState};

#[derive(Debug, Clone)]
pub struct CallFrame {
    name: TinyString,
    pub(crate) upvalues: Vec<Upvalue>,
    pub(crate) stack_start: usize,
    max_slots: u8,
    pub(crate) ip: usize // This would be 0 if the call frame belongs to a native function
}

pub struct Fiber {
    pub(crate) stack: Vec<Value>,
    pub(crate) ip: usize,
    frame: *mut CallFrame,
    frames: Vec<CallFrame>,
    upper: Option<*mut Fiber>
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
            upper: None
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
        let stack_start = self.stack.len();

        self.stack.resize(stack_start + max_slots as usize, Value::Null);
        self.frames.push(CallFrame {
            name: name.into(),
            upvalues: upvalues.to_vec(),
            stack_start,
            max_slots,
            ip: self.ip
        });

        self.frame = unsafe { self.frames.as_mut_ptr().add(self.frames.len() - 1) };
    }

    pub fn new_frame_with_offset<N>(&mut self, name: N, max_slots: u8, upvalues: &[Upvalue], offset: usize)
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
            ip: self.ip
        });

        self.frame = unsafe { self.frames.as_mut_ptr().add(self.frames.len() - 1) };
    }

    pub fn slot(&self, slot: u8) -> Value {
        self.stack[self.frame().stack_start + slot as usize]
    }

    pub fn set_slot(&mut self, slot: u8, value: Value) {
        let index = self.frame().stack_start + slot as usize;
        self.stack[index] = value;
    }

    pub fn set_upslot(&self, index: usize, value: Value) {
        match self.upper {
            Some(ptr) if !ptr.is_null() => unsafe { (*ptr).stack[index] = value; },
            _ => ()
        }
    }

    pub fn upvalue(&self, index: u8) -> Value {
        let frame = self.frame();

        match frame.upvalues.get(index as usize) {
            Some(upvalue) => {
                match upvalue.state() {
                    UpvalueState::Open(index) => {
                        match self.upper {
                            Some(ptr) if !ptr.is_null() => unsafe { Value::from((*ptr).stack.get(index)) },
                            _ => Value::Null
                        }
                    },
                    UpvalueState::Closed(value) => value
                }
            },
            None => Value::Null
        }
    }

    pub fn set_upvalue(&self, index: u8, value: Value) {
        let frame = self.frame();

        if let Some(upvalue) = frame.upvalues.get(index as usize) {
            match upvalue.state() {
                UpvalueState::Open(index) => unsafe {
                    match self.upper {
                        Some(ptr) => (*ptr).stack[index] = value,
                        _ => ()
                    }
                },
                UpvalueState::Closed(value) => upvalue.close(value)
            }
        }
    }

    pub fn child<N>(&self, name: N, max_slots: u8, ip: usize) -> Self
    where 
        N: Into<TinyString>
    {
        let mut fiber = Self::new(name, max_slots, ip);
        fiber.upper = Some(self as *const Self as *mut Self);
        fiber
    }

    pub fn complete_trace(&self) -> Vec<TinyString> {
        let mut frames = Vec::new();
        
        for frame in &self.frames {
            frames.push(frame.name.clone());
        }

        let mut upper = self.upper;

        unsafe {
            while let Some(ptr) = upper {
                let fiber = &*ptr;

                for frame in &fiber.frames {
                    frames.push(frame.name.clone());
                }

                upper = fiber.upper;
            }
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

                upper = fiber.upper;
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

    pub fn pop_two(&mut self) -> Option<(Value, Value)> {
        unsafe {
            let length = self.stack.len();

            if length < 2 {
                None
            } else {
                self.stack.set_len(length - 2);
                let ptr = self.stack.as_ptr().add(length - 1);
                Some((*ptr.sub(1), *ptr)) // (lhs, rhs)
            }
        }
    }

    pub fn pop_three(&mut self) -> Option<(Value, Value, Value)> {
        unsafe {
            let length = self.stack.len();

            if length < 3 {
                None
            } else {
                self.stack.set_len(length - 3);
                let ptr = self.stack.as_ptr().add(length - 1);
                Some((*ptr.sub(2), *ptr.sub(1), *ptr)) 
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

}