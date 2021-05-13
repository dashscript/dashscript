use std::convert::TryInto;
use std::collections::HashMap;
use dashscript_bytecode::{Chunk, Opcode};
use crate::{
    Value, ValueRegister, RuntimeResult, ErrorKind, Error, RegisterId, core
};

#[derive(Default)]
pub struct Frame {
    pub name: String,
    pub id: u16
}

#[derive(Default)]
pub struct Vm {
    pub chunk: Chunk,
    pub(crate) ip: usize,
    pub(crate) error: Option<Error>,
    pub(crate) current_scope_id: u16,
    pub frames: Vec<Frame>,
    pub value_register: HashMap<RegisterId, ValueRegister>,
}

impl Vm {

    pub fn new(chunk: Chunk) -> RuntimeResult<Self> {
        let mut vm = Self {
            chunk,
            ..Default::default()
        };

        core::init(&mut vm);
        vm.new_frame("@runtime".to_string());
        vm.execute()?;

        // The vm sometimes ends with untraced errors. This is used to detect the untraced error.
        if let Some(error) = vm.error {
            return Err(error);
        }

        Ok(vm)
    }

    pub fn execute(&mut self) -> RuntimeResult<()> {
        let mut next_byte = self.chunk.bytes.get(self.ip);

        while let Some(instruction) = next_byte {
            #[allow(mutable_borrow_reservation_conflict)]
            self.execute_instruction(*instruction)?;
            self.ip += 1;
            next_byte = self.chunk.bytes.get(self.ip);
        }

        Ok(())
    }

    pub fn execute_instruction(&mut self, instruction: u8) -> RuntimeResult<()> {
        
            match Opcode::from(instruction) {
                opcode => {
                    return match self.execute_instruction_value(opcode) {
                        Ok(_) => Ok(()),
                        Err(error) => Err(error)
                    };
                }
            }
        
    }

    pub fn execute_instruction_value(&mut self, opcode: Opcode) -> RuntimeResult<Value> {
        Ok(
            match opcode {
                Opcode::True => Value::Boolean(true),
                Opcode::False => Value::Boolean(false),
                Opcode::Null => Value::Null,
                Opcode::Str => {
                    let index = self.get_u8();
                    Value::Str(self.chunk.constants.get_string(index as u32))
                },
                Opcode::StrLong => {
                    let index = self.get_u32();
                    Value::Str(self.chunk.constants.get_string(index))
                },
                Opcode::Word => {
                    let index = self.get_u8();
                    let name = self.chunk.constants.get_string(index as u32);
                    self.value_register
                        .get(&RegisterId(name, self.current_scope_id))
                        .cloned()
                        .unwrap_or_else(|| ValueRegister::default())
                        .value
                },
                Opcode::WordLong => {
                    let index = self.get_u32();
                    let name = self.chunk.constants.get_string(index);
                    self.value_register
                        .get(&RegisterId(name, self.current_scope_id))
                        .cloned()
                        .unwrap_or_else(|| ValueRegister::default())
                        .value
                },
                Opcode::Call => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    let target = self.execute_instruction_value(Opcode::from(byte))?;
                    println!("{:?}", target);
                    let len = self.get_u8();
                    let mut params = Vec::new();

                    for _ in 0..len {
                        match Opcode::from(self.chunk.bytes[self.ip]) {
                            Opcode::RestParam => {
                                let byte = self.get_u8();
                                params.extend(self.execute_instruction_value(Opcode::from(byte))?.to_vec(self));
                            },
                            opcode => {
                                let param = self.execute_instruction_value(opcode)?;
                                params.push(param);
                            }
                        }

                        self.ip += 1;
                    }

                    self.call_function(target, params).unwrap_or_else(|error| self.terminate_execution(error))
                },
                opcode => return Err(self.create_error(ErrorKind::UnknownOpcode { opcode }, "Detected an unexpected opcode."))
            }
        )
    }

    pub fn call_function(
        &mut self,
        target: Value,
        params: Vec<Value>
    ) -> RuntimeResult<Value> {
        match target {
            Value::NativeFn {
                func,
                ..
            } => {
                func.as_ref()(self, params)
            },
            _ => Err(self.create_error(
                ErrorKind::CalledAnUncallable,
                format!("Cannot call of type {}.", target.get_type())
            ))
        }
    }

    pub(crate) fn get_u8(&mut self) -> u8 {
        self.ip += 1;
        match self.chunk.bytes.get(self.ip) {
            Some(byte) => *byte,
            None => {
                self.error = Some(self.create_error(ErrorKind::DislocatedBytes, "Expected one byte to form a u8 but the chunk has an insufficient bytes."));
                0
            }
        }
    }

    pub(crate) fn get_u32(&mut self) -> u32 {
        self.ip += 1;
        match self.chunk.bytes.get(self.ip..self.ip + 4) {
            Some(byte) => {
                self.ip += 4;
                u32::from_le_bytes(byte.try_into().unwrap())
            },
            None => {
                self.error = Some(self.create_error(ErrorKind::DislocatedBytes, "Expected 4 bytes to form a u32 but the chunk has an insufficient bytes."));
                0
            }
        }
    }
    
    pub fn create_error<S>(&self, kind: ErrorKind, message: S) -> Error 
    where
        S: std::string::ToString
    {
        Error {
            kind,
            message: message.to_string()
        }
    }

    pub fn new_frame(&mut self, name: String) {
        self.current_scope_id += 1;
        self.frames.push(Frame {
            name,
            id: self.current_scope_id
        });
    }

    pub fn get_pointer(&self, id: u32) -> Value {
        for (_, value) in self.value_register.iter() {
            if value.id == id {
                return value.value.clone();
            }
        }

        Value::Null
    }

    pub fn add_value(&mut self, name: String, value: Value, mutable: bool) -> Option<ValueRegister> {
        let register = ValueRegister {
            mutable,
            value,
            id: self.value_register.len() as u32
        };

        self.value_register.insert(RegisterId(name, self.current_scope_id), register)
    }

    pub fn terminate_execution(&mut self, error: Error) -> Value {
        // Terminating the execution by overflowing the ip from the actual instruction array length
        self.ip = self.chunk.bytes.len();
        self.error = Some(error);
        Value::Null
    }

}