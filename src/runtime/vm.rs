use std::convert::TryInto;
use std::collections::HashMap;
use dashscript_bytecode::{Chunk, Opcode, BinaryOperator, AssignmentOperator};
use crate::{
    Value, RuntimeResult, ErrorKind, Error, Number, Instruction, InstructionValue, DictKey,
    Array, Dict, NULL, core
};

pub type LocalMapRegister = (u32, bool);
// A hashmap type in which you insert the constant variable type and get the pointer
pub type LocalMap = HashMap<u32, LocalMapRegister>;

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
    pub(crate) flags: HashMap<String, String>,
    pub frames: Vec<Frame>,
    // A map consisting the locals details referencing pointers to the names
    pub local_map: Vec<LocalMap>,
    // This is actually used to to store values as pointers in the local map
    pub(crate) pointers: Vec<Value>,
}

impl Vm {

    pub const NULL_POINTER: u32 = 0;

    pub fn new(chunk: Chunk, flags: HashMap<String, String>) -> RuntimeResult<Self> {
        let mut vm = Self {
            chunk,
            local_map: vec![LocalMap::default()],
            flags,
            pointers: vec![NULL],
            ..Default::default()
        };

        core::init(&mut vm);
        vm.execute()?;

        // The vm sometimes ends with untraced errors. This is used to detect the untraced error.
        if let Some(error) = vm.error {
            return Err(error);
        }

        Ok(vm)
    }

    pub fn execute(&mut self) -> RuntimeResult<()> {
        let mut next_byte = self.chunk.bytes.get(self.ip);

        while let Some(byte) = next_byte {
            #[allow(mutable_borrow_reservation_conflict)]
            let instruction = self.parse_instruction(*byte)?;
            self.execute_instruction(instruction)?;
            self.ip += 1;
            next_byte = self.chunk.bytes.get(self.ip);
        }

        Ok(())
    }

    pub fn execute_instruction(&mut self, instruction: Instruction) -> RuntimeResult<()> {
        Ok(
            match instruction {
                Instruction::Value(instruction_value) => {
                    return match self.execute_instruction_value(instruction_value) {
                        Ok(_) => Ok(()),
                        Err(error) => Err(error)
                    };
                },
                Instruction::Store { name, value, is_const } => {
                    let value = self.execute_instruction_value(value)?;
                    self.add_value(name, value, !is_const);
                },
                Instruction::Update { target, op, value } => return self.execute_assignment(target, op, value),
                Instruction::Return(target) => {
                    println!("{}", self.execute_instruction_value(target)?.to_string());
                    std::process::exit(0)
                },
                Instruction::Break => std::process::exit(0)
            }
        )    
    }

    pub fn execute_instruction_value(&mut self, value: InstructionValue) -> RuntimeResult<Value> {
        Ok(
            match value {
                InstructionValue::Boolean(boolean) => Value::Boolean(boolean),
                InstructionValue::Null => Value::Null,
                InstructionValue::String(string_constant) => Value::Str(self.chunk.constants.get_string(string_constant)),
                InstructionValue::Word(word_constant) => self.get_value(&word_constant).clone(),
                InstructionValue::Int(int_constant) => Value::Num(Number::Int(
                    self.chunk.constants.ints
                        .get(int_constant as usize)
                        .unwrap_or(&0)
                        .clone()
                )),
                InstructionValue::Float(float_constant) => Value::Num(Number::Float(
                    self.chunk.constants.floats
                        .get(float_constant as usize)
                        .unwrap_or(&0.0)
                        .clone()
                )),
                InstructionValue::BinaryOperation(lhs, operator, rhs) => {
                    match operator {
                        BinaryOperator::Add => self.execute_instruction_value(*lhs)? + self.execute_instruction_value(*rhs)?,
                        BinaryOperator::Subtract => self.execute_instruction_value(*lhs)? - self.execute_instruction_value(*rhs)?,
                        BinaryOperator::Multiply => self.execute_instruction_value(*lhs)? * self.execute_instruction_value(*rhs)?,
                        BinaryOperator::Divide => self.execute_instruction_value(*lhs)? / self.execute_instruction_value(*rhs)?,
                        BinaryOperator::Power => self.execute_instruction_value(*lhs)?.pow(self.execute_instruction_value(*rhs)?),
                        BinaryOperator::Rem => self.execute_instruction_value(*lhs)? % self.execute_instruction_value(*rhs)?,
                        BinaryOperator::Or => {
                            let lhs = self.execute_instruction_value(*lhs)?;
                            if lhs.to_bool() { lhs } else {
                                self.execute_instruction_value(*rhs)?
                            }
                        },
                        BinaryOperator::And => Value::Boolean(self.execute_instruction_value(*lhs)?.to_bool() && self.execute_instruction_value(*rhs)?.to_bool()),
                        BinaryOperator::Equal => Value::Boolean(self.execute_instruction_value(*lhs)? == self.execute_instruction_value(*rhs)?),
                        BinaryOperator::NotEqual => Value::Boolean(self.execute_instruction_value(*lhs)? != self.execute_instruction_value(*rhs)?),
                        BinaryOperator::GreaterThan => Value::Boolean(self.execute_instruction_value(*lhs)? > self.execute_instruction_value(*rhs)?),
                        BinaryOperator::GreaterThanOrEqual => Value::Boolean(self.execute_instruction_value(*lhs)? >= self.execute_instruction_value(*rhs)?),
                        BinaryOperator::LessThan => Value::Boolean(self.execute_instruction_value(*lhs)? < self.execute_instruction_value(*rhs)?),
                        BinaryOperator::LessThanOrEqual => Value::Boolean(self.execute_instruction_value(*lhs)? <= self.execute_instruction_value(*rhs)?)
                    }
                },
                InstructionValue::Attribute(target, attribute) => {
                    let target = self.execute_instruction_value(*target)?;
                    match target {
                        Value::Dict(dict) => {
                            let dict_key = DictKey::from(self.execute_instruction_value(*attribute)?);
                            match dict.entries(self).get(&dict_key) {
                                Some((value, _)) => value.clone(),
                                _ => NULL
                            }
                        },
                        Value::Array(array) => {
                            let key = DictKey::from(self.execute_instruction_value(*attribute)?);
                            match key {
                                DictKey::Int(index) => array.vec(self)
                                                            .get(index as usize)
                                                            .unwrap_or(&NULL)
                                                            .clone(),
                                _ => NULL
                            }
                        },
                        target => return Err(Error::new_untraced(
                            ErrorKind::UnexpectedAttributeAccess { 
                                value_type: target.get_type(),
                                attribute: Some(self.execute_instruction_value(*attribute)?.to_string())
                            },
                            "Unexpected attribute access."
                        ))
                    }
                },
                InstructionValue::Call(target, params) => {
                    let target = self.execute_instruction_value(*target)?;
                    let mut executed_params = Vec::new();

                    for (value, is_rest) in params {
                        let value = self.execute_instruction_value(value)?;
                        if is_rest {
                            executed_params.extend(value.to_vec(self));
                        } else {
                            executed_params.push(value);
                        }
                    }

                    return self.call_function(target, executed_params);
                },
                InstructionValue::Invert(target) => Value::Boolean(!self.execute_instruction_value(*target)?.to_bool()),
                InstructionValue::Group(inner_target) => return self.execute_instruction_value(*inner_target),
                InstructionValue::Array(array) => {
                    let mut executed_array = Vec::new();
                    for item in array {
                        executed_array.push(self.execute_instruction_value(item)?);
                    }

                    Value::Array(Array::Vec(executed_array, None))
                },
                InstructionValue::Dict(dict) => {
                    let mut executed_dict = HashMap::new();
                    for (constant_id, inst_value) in dict {
                        executed_dict.insert(
                            DictKey::Str(self.chunk.constants.get_string(constant_id)),
                            (self.execute_instruction_value(inst_value)?, false)
                        );
                    }

                    Value::Dict(Dict::Map(executed_dict, None))
                },
                InstructionValue::Ternary(target, truthy_value, falsy_value) => {
                    return if self.execute_instruction_value(*target)?.to_bool() {
                        self.execute_instruction_value(*truthy_value)
                    } else {
                        self.execute_instruction_value(*falsy_value)
                    }
                }
            }
        )
    }

    pub fn execute_assignment(
        &mut self, 
        target: InstructionValue, 
        op: AssignmentOperator,
        value: InstructionValue
    ) -> RuntimeResult<()> {
        // For borrowing purposes
        macro_rules! get_pointer {
            ($pointer:expr) => {
                self.pointers
                    .get(*$pointer as usize)
                    .unwrap_or(&NULL)
                    .clone()
            };
        }

        match target {
            InstructionValue::Word(constant_id) => {
                let value = self.execute_instruction_value(value)?;
                for local_map in self.local_map.iter_mut().rev() {
                    if let Some((old_pointer, is_mutable)) = local_map.get(&constant_id) {
                        if !is_mutable {
                            return Err(Error::new_untraced(
                                ErrorKind::AssignmentToConstant {
                                    name: self.chunk.constants.get_string(constant_id)
                                },
                                "Assignment to a constant."
                            ))
                        }

                        let pointer = self.pointers.len() as u32;
                        let value = match op {
                            AssignmentOperator::Assign => value,
                            AssignmentOperator::Add => get_pointer!(old_pointer) + value,
                            AssignmentOperator::Sub => get_pointer!(old_pointer) - value
                        };

                        self.pointers.push(value);
                        local_map.insert(constant_id, (pointer, true));
                        return Ok(());
                    }
                }

                return Err(Error::new_untraced(
                    ErrorKind::UpdatedNonExistingValue {
                        name: self.chunk.constants.get_string(constant_id)
                    },
                    "Assignment to a unassigned value."
                ))
            },
            InstructionValue::Attribute(target, attribute) => {
                let value = self.execute_instruction_value(value)?;
                self.execute_attribute_target(*target, *attribute, op, value)
            },
            _ => return Err(Error::new_untraced(
                ErrorKind::UnexpectedAssignment {
                    instruction_value_type: target.get_type()
                }, "Assignment to an unassignable."
            ))
        }
    }

    fn execute_attribute_target(
        &mut self, 
        target: InstructionValue,
        attribute: InstructionValue,
        op: AssignmentOperator,
        value: Value
    ) -> RuntimeResult<()> {
        let dict_key: DictKey = self.execute_instruction_value(attribute)?.into();
        let pointer = self.execute_attribute_target_child(target)?;
        let target = self.pointers[pointer as usize].clone();

        match target {
            Value::Dict(dict) => {
                let mut entries = dict.entries(self);
                let attribute_value = entries.get(&dict_key);

                match attribute_value {
                    Some((old_value, mutable)) => {
                        if !mutable {
                            return Err(Error::new_untraced(
                                ErrorKind::AssignmentToReadonlyProperty { attribute_name: dict_key.to_string() }, 
                                "Assignment to a readonly property."
                            ))
                        }

                        let value = match op {
                            AssignmentOperator::Assign => value.borrow(self),
                            AssignmentOperator::Add => old_value.clone() + value.borrow(self),
                            AssignmentOperator::Sub => old_value.clone() - value.borrow(self)
                        };

                        entries.insert(dict_key, (value, true));
                        self.pointers[pointer as usize] = Value::Dict(Dict::Map(entries, Some(pointer)));
                    },
                    None => {
                        entries.insert(dict_key, (value.borrow(self), true));
                        self.pointers[pointer as usize] = Value::Dict(Dict::Map(entries, Some(pointer)));
                    }
                }

                Ok(())
            },
            _ => return Err(Error::new_untraced(
                ErrorKind::UnexpectedAttributeAccess { 
                    value_type: target.get_type(),
                    attribute: Some(dict_key.to_string())
                }, "Unexpected attribute access."
            ))
        }
    }

    fn execute_attribute_target_child(&mut self, target: InstructionValue) -> RuntimeResult<u32> {
        match target {
            InstructionValue::Word(constant_id) => Ok(*self.get_value_pointer(&constant_id)),
            InstructionValue::Attribute(target, attribute) => {
                let dict_key: DictKey = self.execute_instruction_value(*attribute)?.into();
                let pointer = self.execute_attribute_target_child(*target)?;
                let target = self.pointers[pointer as usize].clone();

                match target {
                    Value::Dict(dict) => {
                        let entries = dict.entries(self);
                        let attribute_value = entries.get(&dict_key);
        
                        match attribute_value {
                            Some(_) => Ok(pointer),
                            None => Err(Error::new_untraced(
                                ErrorKind::UnexpectedAttributeAccess {
                                    value_type: self.pointers[pointer as usize].get_type(),
                                    attribute: Some(dict_key.to_string())
                                },
                                "You cannot access attribute of null."
                            ))
                        }
                    },
                    _ => Err(Error::new_untraced(
                        ErrorKind::UnexpectedAttributeAccess { 
                            value_type: target.get_type(),
                            attribute: Some(dict_key.to_string())
                        }, "Unexpected attribute access."
                    ))
                }
            },
            _ => return Err(Error::new_untraced(
                ErrorKind::UnexpectedAttributeAccess { 
                    value_type: target.get_type(),
                    attribute: None
                }, "Unexpected attribute access."
            ))
        }
    }

    pub fn call_function(&mut self, target: Value, params: Vec<Value>) -> RuntimeResult<Value> {
        match target {
            Value::NativeFn { func, .. } => {
                func(self, params)
            },
            _ => Err(Error::new_untraced(
                ErrorKind::CalledAnUncallable,
                format!("Cannot call of type {}.", target.get_type())
            ))
        }
    }

    pub fn parse_instruction(&mut self, byte: u8) -> RuntimeResult<Instruction> {
        Ok(
            match Opcode::from(byte) {
                Opcode::Var => {
                    let name = self.get_auto()?;
                    let byte = self.get_u8()?;
                    Instruction::Store {
                        name,
                        value: self.parse_instruction_value(Opcode::from(byte))?,
                        is_const: false
                    }
                },
                Opcode::Const => {
                    let name = self.get_auto()?;
                    let byte = self.get_u8()?;
                    Instruction::Store {
                        name,
                        value: self.parse_instruction_value(Opcode::from(byte))?,
                        is_const: true
                    }
                },
                Opcode::Assign => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    let target = self.parse_instruction_value(Opcode::from(byte))?;

                    self.ip += 1;
                    let op = match self.chunk.bytes[self.ip] {
                        0 => AssignmentOperator::Assign,
                        1 => AssignmentOperator::Add,
                        2 => AssignmentOperator::Sub,
                        _ => AssignmentOperator::Assign
                    };

                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    let value = self.parse_instruction_value(Opcode::from(byte))?;

                    Instruction::Update { target, op, value }
                },
                Opcode::Return => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    Instruction::Return(self.parse_instruction_value(Opcode::from(byte))?)
                },
                Opcode::Break => Instruction::Break,
                opcode => Instruction::Value(self.parse_instruction_value(opcode)?)
            }
        )
    }

    pub fn parse_instruction_value(&mut self, opcode: Opcode) -> RuntimeResult<InstructionValue> {
        Ok(
            match opcode {
                Opcode::True => InstructionValue::Boolean(true),
                Opcode::False => InstructionValue::Boolean(false),
                Opcode::Null => InstructionValue::Null,
                Opcode::Str => InstructionValue::String(self.get_u8()? as u32),
                Opcode::StrLong => InstructionValue::String(self.get_u32()?),
                Opcode::Word => InstructionValue::Word(self.get_u8()? as u32),
                Opcode::WordLong => InstructionValue::Word(self.get_u32()?),
                Opcode::Int => InstructionValue::Int(self.get_u8()? as u32),
                Opcode::IntLong => InstructionValue::Int(self.get_u32()?),
                Opcode::Float => InstructionValue::Float(self.get_u8()? as u32),
                Opcode::FloatLong => InstructionValue::Float(self.get_u32()?),
                Opcode::Invert => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    let target = self.parse_instruction_value(Opcode::from(byte))?;
                    InstructionValue::Invert(Box::new(target))
                },
                Opcode::BinaryOperation => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    if byte > BinaryOperator::VARIANT_SIZE {
                        return Err(Error::new_untraced(
                            ErrorKind::UnexpectedByte { byte }, 
                            format!("Expected a byte within the range of (0..{}).", BinaryOperator::VARIANT_SIZE)
                        ));
                    }

                    self.ip += 1;
                    let lhs = self.chunk.bytes[self.ip];
                    let parsed_lhs = Box::new(self.parse_instruction_value(Opcode::from(lhs))?);
                    self.ip += 1;
                    let rhs = self.chunk.bytes[self.ip];

                    InstructionValue::BinaryOperation(
                        parsed_lhs,
                        unsafe { std::mem::transmute(byte) }, 
                        Box::new(self.parse_instruction_value(Opcode::from(rhs))?)
                    )
                },
                Opcode::Call => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    let target = self.parse_instruction_value(Opcode::from(byte))?;
                    let len = self.get_u8()?;
                    let mut params = Vec::new();

                    for _ in 0..len {
                        self.ip += 1;
                        match Opcode::from(self.chunk.bytes[self.ip]) {
                            Opcode::RestParam => {
                                let byte = self.get_u8()?;
                                params.push((self.parse_instruction_value(Opcode::from(byte))?, true));
                            },
                            opcode => params.push((self.parse_instruction_value(opcode)?, false))
                        }
                    }

                    InstructionValue::Call(Box::new(target), params)
                },
                Opcode::Group => {
                    self.ip += 1;
                    let byte = self.chunk.bytes[self.ip];
                    InstructionValue::Group(Box::new(self.parse_instruction_value(Opcode::from(byte))?))
                },
                Opcode::Attr => {
                    self.ip += 1;
                    let lhs = self.chunk.bytes[self.ip];
                    let parsed_lhs = Box::new(self.parse_instruction_value(Opcode::from(lhs))?);
                    self.ip += 1;
                    let rhs = self.chunk.bytes[self.ip];

                    InstructionValue::Attribute(
                        parsed_lhs,
                        Box::new(self.parse_instruction_value(Opcode::from(rhs))?)
                    )
                },
                Opcode::Array => {
                    let len = self.get_u32()?;
                    let mut vec = Vec::new();
                    for _ in 0..len {
                        self.ip += 1;
                        let byte = self.chunk.bytes[self.ip];
                        vec.push(self.parse_instruction_value(Opcode::from(byte))?);
                    }
                    
                    InstructionValue::Array(vec)
                },
                Opcode::Dict => {
                    let len = self.get_u32()?;
                    let mut dict = Vec::new();
                    for _ in 0..len {
                        let key = self.get_auto()?;
                        self.ip += 1;
                        let byte = self.chunk.bytes[self.ip];
                        dict.push((key, self.parse_instruction_value(Opcode::from(byte))?));
                    }

                    InstructionValue::Dict(dict)
                },
                Opcode::Ternary => {
                    macro_rules! next_instruction_value {
                        () => {{
                            self.ip += 1;
                            match self.chunk.bytes.get(self.ip) {
                                #[allow(mutable_borrow_reservation_conflict)]
                                Some(byte) => Box::new(self.parse_instruction_value(Opcode::from(*byte))?),
                                None => return Err(Error::new_untraced(ErrorKind::DislocatedBytes, "Expected one byte to form a u8 but the chunk has an insufficient bytes."))
                            }
                        }};
                    }

                    InstructionValue::Ternary(
                        next_instruction_value!(),
                        next_instruction_value!(),
                        next_instruction_value!()
                    )
                },
                opcode => return Err(Error::new_untraced(
                    ErrorKind::UnknownOpcode { opcode }, 
                    "Detected an unexpected opcode."
                ))
            }
        )
    }

    pub(crate) fn get_u8(&mut self) -> RuntimeResult<u8> {
        self.ip += 1;
        match self.chunk.bytes.get(self.ip) {
            Some(byte) => Ok(*byte),
            None => Err(Error::new_untraced(ErrorKind::DislocatedBytes, "Expected one byte to form a u8 but the chunk has an insufficient bytes."))
        }
    }

    pub(crate) fn get_u32(&mut self) -> RuntimeResult<u32> {
        self.ip += 1;
        match self.chunk.bytes.get(self.ip..self.ip + 4) {
            Some(byte) => {
                self.ip += 3;
                Ok(u32::from_le_bytes(byte.try_into().unwrap()))
            },
            None => Err(Error::new_untraced(ErrorKind::DislocatedBytes, "Expected 4 bytes to form a u32 but the chunk has an insufficient bytes."))
        }
    }

    pub(crate) fn get_auto(&mut self) -> RuntimeResult<u32> {
        self.ip += 1;
        match Opcode::from(self.chunk.bytes[self.ip]) {
            Opcode::Short => {
                self.ip += 1;
                match self.chunk.bytes.get(self.ip) {
                    Some(byte) => Ok(*byte as u32),
                    None => Err(Error::new_untraced(ErrorKind::DislocatedBytes, "Expected one byte to form a u8 but the chunk has an insufficient bytes."))
                }
            },
            Opcode::Long => {
                self.ip += 1;
                match self.chunk.bytes.get(self.ip..self.ip + 4) {
                    Some(byte) => {
                        self.ip += 3;
                        Ok(u32::from_le_bytes(byte.try_into().unwrap()))
                    },
                    None => Err(Error::new_untraced(ErrorKind::DislocatedBytes, "Expected 4 bytes to form a u32 but the chunk has an insufficient bytes."))
                }
            },
            opcode => return Err(Error::new_untraced(
                ErrorKind::UnknownOpcode { opcode }, 
                "Detected an unexpected opcode. Expected either `Long` or `Short` opcode."
            ))
        }
    }

    pub fn get_pointer(&self, id: u32) -> Value {
        self.pointers
            .get(id as usize)
            .unwrap_or(&NULL)
            .clone()
    }

    pub fn add_value(&mut self, constant_id: u32, value: Value, mutable: bool) -> Option<(u32, bool)> {
        let pointer: u32 = match value {
            Value::Dict(ref dict) => {
                match dict {
                    Dict::Map(_, Some(id)) => *id,
                    Dict::Map(entries, None) => {
                        let pointer = self.pointers.len() as u32;
                        self.pointers.push(Value::Dict(Dict::Map(entries.clone(), Some(pointer))));
                        pointer
                    },
                    Dict::Ref(id) => *id
                }
            },
            Value::Array(ref array) => {
                match array {
                    Array::Vec(_, Some(id)) => *id,
                    Array::Vec(vector, None) => {
                        let pointer = self.pointers.len() as u32;
                        self.pointers.push(Value::Array(Array::Vec(vector.clone(), Some(pointer))));
                        pointer
                    },
                    Array::Ref(id) => *id
                }
            },
            _ => {
                self.pointers.push(value);
                self.pointers.len() as u32 - 1
            }
        };

        self.local_map
            .last_mut()
            .unwrap()
            .insert(constant_id, (pointer, mutable))
    }

    pub fn get_value(&self, constant_id: &u32) -> &Value {
        for local_map in self.local_map.iter().rev() {
            if let Some((pointer, _)) = local_map.get(constant_id) {
                return self.pointers
                    .get(*pointer as usize)
                    .unwrap_or(&NULL)
            }
        }

        &NULL
    }

    pub fn get_value_pointer(&self, constant_id: &u32) -> &u32 {
        for local_map in self.local_map.iter().rev() {
            if let Some((pointer, _)) = local_map.get(constant_id) {
                return pointer;
            }
        }

        &0
    }

    pub fn terminate_execution(&mut self, error: Error) -> Value {
        // Terminating the execution by overflowing the ip from the actual instruction array length
        self.ip = self.chunk.bytes.len();
        self.error = Some(error);
        Value::Null
    }

    pub(crate) fn has_permission(&self, string: &str) -> bool {
        self.flags.contains_key(&("use-".to_owned() + string))
    }

}