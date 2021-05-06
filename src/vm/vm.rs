use std::env;
use std::collections::HashMap;
use std::fmt;
use super::vmcore::{ self, builtin };
use super::vmcore::result::{ ok, err };
use super::value::{ Value, ValueRegister, ValueIndex, ControlFlow, Dict, NativeFn };
use crate::lexer::parser::Position;
use crate::bytecode::reader::LogicalOperator;
use crate::common::{ fsize, get_line_col_by_line_data };
use crate::bytecode::main::BytecodeCompiler;
use crate::bytecode::reader::{ BytecodeReader, InstructionValue, Instruction };
use crate::dict;

#[derive(Debug)]
pub struct RuntimeError {
    pub message: String,
    pub filename: String,
    pub call_frames: Vec<String>,
    pub start: usize,
    pub end: usize,
    pub line: usize,
    pub col: usize
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut address = String::new();

        for frame in self.call_frames.clone().into_iter().rev() {
            address += &format!("    at {}\n", frame).to_string();
        }

        write!(f, "{} ({}:{}:{})\n{}", self.message, self.filename, self.line, self.col, address)
    }
}

#[derive(Clone, Debug)]
pub struct Frame {
    pub vi: usize,
    pub name: String
}

impl Frame {
    pub fn new(name: String, vm: &mut VM) -> Self {
        Self {
            vi: vm.value_register.len(),
            name
        }
    }
}

#[derive(Clone)]
pub struct VM {
    pub pos_map: Vec<(usize, Position)>,
    pub filename: String,
    pub reader: BytecodeReader,
    pub frames: Vec<Frame>,
    pub value_stack: Vec<Value>,
    pub value_register: Vec<ValueRegister>,
    pub body_line_data: Vec<usize>,
    pub flags: HashMap<String, String>
}

impl VM {

    pub fn new(compiler: BytecodeCompiler, filename: String, body: String, flags: HashMap<String, String>) -> Result<Self, RuntimeError> {
        let mut vm = Self {
            pos_map: compiler.pos_map.clone(),
            filename: filename,
            reader: BytecodeReader::new(compiler),
            frames: vec![Frame {
                name: "@runtime".to_string(),
                vi: 0
            }],
            value_stack: Vec::<Value>::new(),
            value_register: Vec::new(),
            body_line_data: Vec::new(),
            flags
        };

        for line in body.split("\n").collect::<Vec<&str>>().iter() {
            vm.body_line_data.push(line.len());
        }

        vm.init_core();
        vm.execute_body()?;
        Ok(vm)
    }

    pub fn default(filename: String, flags: HashMap<String, String>) -> Self {
        Self {
            pos_map: Vec::new(),
            filename,
            reader: BytecodeReader::default(),
            frames: vec![Frame {
                name: "@runtime".to_string(),
                vi: 0
            }],
            value_stack: Vec::<Value>::new(),
            value_register: Vec::new(),
            body_line_data: Vec::new(),
            flags
        }
    }

    pub fn execute_body(&mut self) -> Result<(), RuntimeError> {
        let mut instruction = Some(self.reader.init());
        
        while instruction.is_some() {
            self.execute_instruction(instruction.unwrap())?;
            instruction = self.reader.next();
        }
        
        Ok(())
    }

    pub fn execute_instruction(&mut self, instruction: Instruction) -> Result<(), RuntimeError> {
        match instruction {
            Instruction::Var(pos, name, value) => {
                let name = self.reader.get_constant(name as usize);
                let value = self.execute_value(value, pos)?;
                
                if self.value_exists(name.clone()) {
                    return Err(self.create_error(
                        format!("AssignmentError: Identifier {} has already declared.", name),
                        pos
                    ))
                }

                self.add_value(name, value, true);
                Ok(())
            },
            Instruction::Const(pos, name, value) => {
                let name = self.reader.get_constant(name as usize);
                let value = self.execute_value(value, pos)?;

                if self.value_exists(name.clone()) {
                    return Err(self.create_error(
                        format!("AssignmentError: Identifier {} has already declared.", name),
                        pos
                    ))
                }

                self.add_value(name, value, false);
                Ok(())
            },
            Instruction::Assign(pos, target, op, value) => {
                let val = self.execute_value(value, pos)?;
                self.execute_assignment(target, pos, val, op, true, 0)?;
                Ok(())
            },
            Instruction::Value(pos, val) => {
                self.execute_value(val, pos)?;
                Ok(())
            },
            Instruction::Condition(pos, main_chain, else_chunk) => {
                self.execute_condition_chain(main_chain, else_chunk, pos)?;
                Ok(())
            },
            Instruction::While(pos, condition, chunk) => {
                self.execute_while_loop(condition, chunk, pos)?;
                Ok(())
            },
            Instruction::Return(pos, val) => {
                println!("{}", builtin::inspect(self.execute_value(val, pos)?, self));
                std::process::exit(0);
            },
            Instruction::Break => std::process::exit(0),
            Instruction::Continue(_) => return Ok(())
        }
    }

    // TODO(Scientific-Guy): Make a better assignment executor.
    pub fn execute_assignment(
        &mut self, 
        value: InstructionValue, 
        pos: usize, 
        mut val: Value,
        op: u8,
        last_stack: bool,
        id: u32
    ) -> Result<u32, RuntimeError> {
        match value {
            InstructionValue::Word(i) => {
                let old_val = self.get_value_register(i)?;

                if last_stack {
                    if !old_val.mutable {
                        return Err(self.create_error(
                            format!("AssignmentToConstant: You cannot assign a value to a constant"),
                            pos
                        ))
                    }

                    val = val.borrow(self);
                    self.value_stack[old_val.id as usize] = match op {
                        0 => val,
                        1 => vmcore::add_values(self.value_stack[old_val.id as usize].clone(), val),
                        2 => vmcore::sub_values(self.value_stack[old_val.id as usize].clone(), val, self),
                        _ => Value::Null
                    }.to_pointer_value(old_val.id);

                    Ok(0)
                } else {
                    Ok(old_val.id)
                }
            },
            InstructionValue::Attr(raw_target, raw_attr) => {
                let target_id = self.execute_assignment(*raw_target, pos, val.clone(), op, false, id)? as usize;
                let target = self.value_stack[target_id].clone();
                let attr = self.execute_value(*raw_attr, pos)?;
                let attr_index = attr.to_value_index();

                match target.clone() {
                    Value::Dict(dict) => {
                        let mut entries = dict.entries(self);    
                        let old_entry = entries.get(&attr_index);                

                        match old_entry {
                            Some((old_val, is_mutable)) => {
                                if last_stack {
                                    if !is_mutable {
                                        let msg = format!("UnexpectedAttributeAccess: Property {} is readonly at {}.", builtin::inspect(attr, self), builtin::inspect(target.clone(), self));
                                        return Err(self.create_error(msg, pos))
                                    }
                                    
                                    match dict {
                                        Dict::Ref(pointer) | Dict::Map(_, Some(pointer)) => {
                                            // TODO(Scientific-Guy): Perform attribute value assignment without cloning entries and prevent borrow error.
                                            let mut new_entries = entries.clone();
                                            new_entries.insert(attr_index, (val.borrow(self), true));
                                            self.value_stack[pointer as usize] = match op {
                                                0 => Value::Dict(Dict::Map(new_entries, Some(pointer as u32))),
                                                1 => vmcore::add_values(old_val.clone(), val),
                                                2 => vmcore::sub_values(old_val.clone(), val, self),
                                                _ => Value::Null
                                            }.to_pointer_value(pointer);
                                        },
                                        _ => return Err(self.create_error(
                                            format!("SegmentationFault: Unexpected kind of dict {:?}.", target),
                                            pos
                                        ))
                                    }
                                }
    
                                Ok(target_id as u32)
                            },
                            None => {
                                if last_stack {
                                    if op != 0 {
                                        return Err(self.create_error(
                                            format!(
                                                "UnexpectedAssignment: You can only assign a value if the previous value is null but you have used a `{}` operator.",
                                                match op {
                                                    1 => "+=",
                                                    2 => "-=",
                                                    _ => "unknown"
                                                }
                                            ), 
                                            pos
                                        ))
                                    }

                                    entries.insert(attr_index, (val.borrow(self), true));
                                    match dict {
                                        Dict::Ref(pointer) | Dict::Map(_, Some(pointer)) => self.value_stack[pointer as usize] = Value::Dict(Dict::Map(entries, Some(pointer))),
                                        _ => return Err(self.create_error(
                                            format!("SegmentationFault: Unexpected kind of dict {:?}.", target),
                                            pos
                                        ))
                                    }

                                    Ok(0)
                                } else {
                                    Err(self.create_error(
                                        "UnexpectedAttributeAccess: You cannot access attributes of null.".to_string(), 
                                        pos
                                    ))
                                }
                            }
                        }
                    },
                    _ => Err(self.create_error(
                        format!("UnexpectedAttributeAccess: You cannot set attributes of type {}.", target.type_as_str()), 
                        pos
                    ))
                }
            },
            InstructionValue::Dict(_) | InstructionValue::Call(_, _) => return Err(self.create_error(
                format!("UnexpectedAssignment: You cannot directly set properties. Attempt to assign it as a variable and then assign it."), 
                pos
            )),
            _ => return Err(self.create_error(
                format!("UnexpectedAssignment: Detected an unexpected assignment."), 
                pos
            ))
        }
    }

    pub fn execute_value(&mut self, value: InstructionValue, pos: usize) -> Result<Value, RuntimeError> {
        match value {
            InstructionValue::True => Ok(Value::Boolean(true)),
            InstructionValue::False => Ok(Value::Boolean(false)),
            InstructionValue::Null => Ok(Value::Null),
            InstructionValue::Group(content) => Ok(self.execute_value(*content, pos)?),
            InstructionValue::Str(id) => Ok(Value::Str(self.reader.get_constant(id as usize))),
            InstructionValue::Word(id) => Ok(self.get_value(id)),
            InstructionValue::Num(num) => Ok(Value::Num(num)),
            InstructionValue::Dict(dict_entries) => {
                let mut entries = HashMap::new();
                for entry in dict_entries.iter() { 
                    let val = self.execute_value(entry.1.clone(), pos)?;
                    entries.insert(
                        ValueIndex::Str(self.reader.get_constant(entry.0 as usize)), 
                        (val.borrow(self), true)
                    );
                }
                
                Ok(Value::Dict(Dict::Map(entries, None)))
            },
            InstructionValue::Attr(raw_body, raw_attr) => {
                let attr = self.execute_value(*raw_attr, pos)?.to_value_index();
                let body = self.execute_value(*raw_body, pos)?;

                match body {
                    Value::Dict(entries) => match entries.entries(self).get(&attr) {
                        Some(val) => Ok(val.0.clone()),
                        None => Ok(Value::Null)
                    },
                    Value::Str(str) => match attr {
                        ValueIndex::Str(attr) => Ok(match attr.as_str() {
                            "length" => Value::Num(str.chars().count() as fsize),
                            "toLowerCase" => Value::NativeFn(Box::new(Value::Str(str)), |this, _, _| {
                                if let Value::Str(str) = this {
                                    Value::Str(str.to_lowercase())
                                } else {
                                    Value::Null
                                }
                            }),
                            "toUpperCase" => Value::NativeFn(Box::new(Value::Str(str)), |this, _, _| {
                                if let Value::Str(str) = this {
                                    Value::Str(str.to_uppercase())
                                } else {
                                    Value::Null
                                }
                            }),
                            "toNumber" => Value::NativeFn(Box::new(Value::Str(str)), |this, _, vm| {
                                if let Value::Str(str) = this {
                                    match str.parse::<fsize>() {
                                        Ok(num) => ok(vm, Value::Num(num)),
                                        Err(_) => err(vm, Value::Str("Improper number.".to_string()))
                                    }
                                } else {
                                    err(vm, Value::Str("Improper number.".to_string()))
                                }
                            }),
                            "startsWith" => Value::NativeFn(Box::new(Value::Str(str)), |this, args, vm| {
                                let str2 = match args.get(0) {
                                    Some(Value::Str(str)) => str.clone(),
                                    _ => builtin::panic("InvalidArgumentError: Expected 1 string type argument but found none.".to_string(), vm)
                                };

                                if let Value::Str(str) = this {
                                    Value::Boolean(str.starts_with(str2.as_str()))
                                } else {
                                    Value::Boolean(false)
                                }
                            }),
                            "endsWith" => Value::NativeFn(Box::new(Value::Str(str)), |this, args, vm| {
                                let str2 = match args.get(0) {
                                    Some(Value::Str(str)) => str.clone(),
                                    _ => builtin::panic("InvalidArgumentError: Expected 1 string type argument but found none.".to_string(), vm)
                                };

                                if let Value::Str(str) = this {
                                    Value::Boolean(str.ends_with(str2.as_str()))
                                    
                                } else {
                                    Value::Boolean(false)
                                }
                            }),
                            "includes" => Value::NativeFn(Box::new(Value::Str(str)), |this, args, vm| {
                                let str2 = match args.get(0) {
                                    Some(Value::Str(str)) => str.clone(),
                                    _ => builtin::panic("InvalidArgumentError: Expected 1 string type argument but found none.".to_string(), vm)
                                };

                                if let Value::Str(str) = this {
                                    Value::Boolean(str.contains(str2.as_str()))
                                } else {
                                    Value::Boolean(false)
                                }
                            }),
                            "escapeDebug" => Value::NativeFn(Box::new(Value::Str(str)), |this, _, _| {
                                if let Value::Str(str) = this {
                                    Value::Str(str.escape_debug().to_string())
                                } else {
                                    Value::Null
                                }
                            }),
                            "trim" => Value::NativeFn(Box::new(Value::Str(str)), |this, _, _| {
                                if let Value::Str(str) = this { Value::Str(str.trim().to_string()) } else { Value::Null }
                            }),
                            _ => Value::Null
                        }),
                        ValueIndex::Num(index) => Ok(match str.chars().nth(index.0 as usize) {
                            Some(char) => Value::Str(char.to_string()),
                            None => Value::Null
                        }),
                        _ => Ok(Value::Null), 
                    },
                    Value::Array(arr) => match attr {
                        ValueIndex::Num(num) => {
                            let index = match arr.get(num.0 as usize) {
                                Some(val) => *val,
                                None => return Ok(Value::Null)
                            } as usize;

                            Ok(match self.value_stack.get(index) {
                                Some(val) => val.clone(),
                                None => builtin::panic(format!("MemoryFailure: Could not find pointer {}.", index), self)
                            })
                        },
                        _ => Ok(Value::Null)
                    },
                    _ => return Err(self.create_error(
                        format!("UnexpectedAttributeAccess: You cannot access attributes of type {}.", body.type_as_str()), 
                        pos
                    ))
                }
            },
            InstructionValue::Call(val, params) => {
                let call_body = self.execute_value(*val, pos)?;
                let mut call_params = Vec::new();
                for param in params.iter() {
                    let val = self.execute_value(param.0.clone(), pos)?;
                    if param.1 {
                        call_params.extend(val.to_vec(self));
                        continue;
                    }

                    call_params.push(val)
                }

                match call_body {
                    Value::NativeFn(this, func) => {
                        self.create_frame("NativeFunction".to_string());
                        let res = Ok(func(*this, call_params, self));
                        self.frames.pop();
                        res
                    },
                    Value::Func(id, params, chunk, _) => self.execute_func(id, params, call_params, chunk),
                    _ => Err(self.create_error(
                        format!("UnexpectedTypeError: Type {} is not callable.", call_body.type_as_str()), 
                        pos
                    ))
                }
            },
            InstructionValue::Array(vec) => {
                let mut items = Vec::new();

                for item in vec {
                    let val = self.execute_value(item, pos)?;
                    self.value_stack.push(val);
                    items.push(self.value_stack.len() as u32 - 1);
                }

                Ok(Value::Array(items))
            },
            InstructionValue::Or(target, falsy_value) => Ok({
                let target_val = self.execute_value(*target, pos)?;
                if builtin::bool(target_val.clone()) {
                    target_val
                } else {
                    self.execute_value(*falsy_value, pos)?
                }
            }),
            InstructionValue::Func(id, params, chunk, is_async) => {
                let name = self.reader.get_constant(id as usize);
                let val = Value::Func(id, params, chunk.clone(), is_async);
                if name != "anonymous".to_string() {
                    self.add_value(name, val.clone(), false)
                }

                Ok(val)
            },
            InstructionValue::Invert(ident) => {
                Ok(Value::Boolean(!builtin::bool(self.execute_value(*ident, pos)?)))
            },
            InstructionValue::And(ident1, ident2) => {
                Ok(Value::Boolean(
                    builtin::bool(self.execute_value(*ident1, pos)?) &&
                    builtin::bool(self.execute_value(*ident2, pos)?)
                ))
            },
            InstructionValue::Ternary(bool, truthy, falsy) => {
                if builtin::bool(self.execute_value(*bool, pos)?) {
                    self.execute_value(*truthy, pos)
                } else {
                    self.execute_value(*falsy, pos)
                }
            },
            InstructionValue::Add(a, b) => Ok(vmcore::add_values(
                self.execute_value(*a, pos)?,
                self.execute_value(*b, pos)?
            )),
            InstructionValue::Sub(a, b) => Ok(vmcore::sub_values(
                self.execute_value(*a, pos)?,
                self.execute_value(*b, pos)?,
                self
            )),
            InstructionValue::Mult(a, b) => Ok(vmcore::mult_values(
                self.execute_value(*a, pos)?,
                self.execute_value(*b, pos)?,
                self
            )),
            InstructionValue::Div(a, b) => Ok(vmcore::div_values(
                self.execute_value(*a, pos)?,
                self.execute_value(*b, pos)?,
                self
            )),
            InstructionValue::Pow(a, b) => Ok(vmcore::pow_values(
                self.execute_value(*a, pos)?,
                self.execute_value(*b, pos)?,
                self
            )),
            InstructionValue::Condition(a, LogicalOperator::GreaterThan, b) => Ok({
                if let (Value::Num(a), Value::Num(b)) = (self.execute_value(*a, pos)?, self.execute_value(*b, pos)?) {
                    Value::Boolean(a > b)
                } else {
                    Value::Boolean(false)
                }
            }),
            InstructionValue::Condition(a, LogicalOperator::LessThan, b) => Ok({
                if let (Value::Num(a), Value::Num(b)) = (self.execute_value(*a, pos)?, self.execute_value(*b, pos)?) {
                    Value::Boolean(a < b)
                } else {
                    Value::Boolean(false)
                }
            }),
            InstructionValue::Condition(a, LogicalOperator::GreaterThanOrEqual, b) => Ok({
                if let (Value::Num(a), Value::Num(b)) = (self.execute_value(*a, pos)?, self.execute_value(*b, pos)?) {
                    Value::Boolean(a >= b)
                } else {
                    Value::Boolean(false)
                }
            }),
            InstructionValue::Condition(a, LogicalOperator::LessThanOrEqual, b) => Ok({
                if let (Value::Num(a), Value::Num(b)) = (self.execute_value(*a, pos)?, self.execute_value(*b, pos)?) {
                    Value::Boolean(a <= b)
                } else {
                    Value::Boolean(false)
                }
            }),
            InstructionValue::Condition(a, LogicalOperator::Equal, b) => Ok(Value::Boolean(self.execute_value(*a, pos)? == self.execute_value(*b, pos)?)),
            InstructionValue::Condition(a, LogicalOperator::NotEqual, b) => Ok(Value::Boolean(self.execute_value(*a, pos)? != self.execute_value(*b, pos)?)),
            i => Err(self.create_error(
                format!("UnknownRuntimeError: Unexpected value while rendering: {:?}.", i), 
                pos
            ))
        }
    }

    pub fn execute_func(
        &mut self,
        id: u32,
        param_ids: Vec<(u32, bool)>,
        params: Vec<Value>,
        chunk: Vec<u8>
    ) -> Result<Value, RuntimeError> {
        self.create_frame(self.reader.get_constant(id as usize));

        // TODO(Scientific-Guy): Make a better chunk reader instead of cloning the reader.
        let state = self.reader.get_state();
        self.reader.len = chunk.len();
        self.reader.ci = 0;
        self.reader.bytes = chunk;

        for i in 0..param_ids.len() {
            let val = {
                if !param_ids[i].1 {
                    match params.get(i as usize) {
                        Some(val) => val.clone(),
                        None => Value::Null
                    }
                } else {
                    match params.get(i..) {
                        Some(params) => {
                            let mut ids = vec![];
                            for param in params {
                                self.value_stack.push(param.clone());
                                ids.push(self.value_stack.len() as u32 - 1);
                            }

                            Value::Array(ids)
                        },
                        None => Value::Array(vec![])
                    }
                }
            };

            self.add_value(self.reader.get_constant(param_ids[i].0 as usize), val, true);
        }

        // TODO(Scientific-Guy): Prevent unwated bytes to overlap the function code.
        while self.reader.ci < self.reader.len {
            match self.reader.parse_byte(self.reader.bytes[self.reader.ci]) {
                Instruction::Return(pos, val) => {
                    let val = self.execute_value(val, pos);
                    self.reader.update_state(state.clone());
                    self.remove_frame();
                    return val;
                },
                Instruction::While(pos, condition, range) => {
                    if let Some(val) = self.execute_while_loop(condition, range, pos)? { 
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(val) 
                    }
                },
                Instruction::Condition(pos, main_chain, else_chunk) => {
                    if let ControlFlow::Return(val) = self.execute_condition_chain(main_chain, else_chunk, pos)? { 
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(val);
                    }
                },
                instruction => {
                    self.execute_instruction(instruction)?;
                }
            }
        }

        self.reader.update_state(state);
        self.remove_frame();
        Ok(Value::Null)
    }

    pub fn execute_while_loop(
        &mut self,
        condition: InstructionValue,
        chunk: Vec<u8>,
        pos: usize
    ) -> Result<Option<Value>, RuntimeError> {
        let mut instructions = Vec::new();
        let state = self.reader.get_state();
        self.reader.ci = 0;
        self.reader.len = chunk.len();
        self.reader.bytes = chunk.clone();
        self.create_frame("@while".to_string());

        while self.reader.ci < self.reader.len {
            instructions.push(self.reader.parse_byte(self.reader.bytes[self.reader.ci]));
        }

        while builtin::bool(self.execute_value(condition.clone(), pos)?) {
            for instruction in &instructions {
                match instruction {
                    Instruction::Break => {
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(None)
                    },
                    Instruction::Continue(_) => break,
                    Instruction::Return(pos, value) => {
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(Some(self.execute_value(value.clone(), *pos)?))
                    },
                    Instruction::Condition(pos, main_chain, else_chunk) => {
                        match self.execute_condition_chain(main_chain.clone(), else_chunk.clone(), *pos)? {
                            ControlFlow::None => (),
                            ControlFlow::Return(val) => {
                                self.reader.update_state(state);
                                self.remove_frame();
                                return Ok(Some(val))
                            },
                            ControlFlow::Break => {
                                self.reader.update_state(state);
                                self.remove_frame();
                                return Ok(None)
                            }
                        }
                    },
                    Instruction::While(pos, condition, chunk) => {
                        if let Some(val) = self.execute_while_loop(condition.clone(), chunk.clone(), *pos)? { 
                            self.reader.update_state(state);
                            self.remove_frame();
                            return Ok(Some(val)) 
                        }

                        self.reader.ci += 1;
                    },
                    _ => {
                        self.execute_instruction(instruction.clone())?;
                    }
                }
            }
        }

        self.reader.update_state(state);
        self.remove_frame();
        Ok(None)
    }

    pub fn execute_condition_chain(
        &mut self,
        main_chain: Vec<(InstructionValue, Vec<u8>)>,
        else_chunk: Option<Vec<u8>>,
        pos: usize
    ) -> Result<ControlFlow, RuntimeError> {
        for (instruction_value, chunk) in main_chain {
            if builtin::bool(self.execute_value(instruction_value, pos)?) {
                let state = self.reader.get_state();
                self.reader.ci = 0;
                self.reader.len = chunk.len();
                self.reader.bytes = chunk;
                self.create_frame("@condition".to_string());

                while self.reader.ci < self.reader.len {
                    match self.reader.parse_byte(self.reader.bytes[self.reader.ci]) {
                        Instruction::Break => {
                            self.reader.update_state(state);
                            self.remove_frame();
                            return Ok(ControlFlow::Break);
                        },
                        Instruction::Return(pos, val) => {
                            self.reader.update_state(state);
                            self.remove_frame();
                            return Ok(ControlFlow::Return(self.execute_value(val, pos)?));
                        },
                        Instruction::While(pos, condition, chunk) => {
                            if let Some(val) = self.execute_while_loop(condition, chunk, pos)? {
                                self.reader.update_state(state);
                                self.remove_frame();
                                return Ok(ControlFlow::Return(val));
                            }
                        },
                        Instruction::Condition(pos, main_chain, else_chunk) => {
                            match self.execute_condition_chain(main_chain, else_chunk, pos)? {
                                ControlFlow::None => (),
                                val => {
                                    self.reader.update_state(state);
                                    self.remove_frame();
                                    return Ok(val);
                                }
                            }
                        },
                        instruction => self.execute_instruction(instruction)?
                    }
                }

                self.reader.update_state(state);
                self.remove_frame();
                return Ok(ControlFlow::None);
            }
        }

        if else_chunk.is_some() {
            let chunk = else_chunk.unwrap();
            let state = self.reader.get_state();
            self.reader.ci = 0;
            self.reader.len = chunk.len();
            self.reader.bytes = chunk;
            self.create_frame("@condition".to_string());

            while self.reader.ci < self.reader.len {
                match self.reader.parse_byte(self.reader.bytes[self.reader.ci]) {
                    Instruction::Break => {
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(ControlFlow::Break);
                    },
                    Instruction::Return(pos, val) => {
                        self.reader.update_state(state);
                        self.remove_frame();
                        return Ok(ControlFlow::Return(self.execute_value(val, pos)?));
                    },
                    Instruction::While(pos, condition, chunk) => {
                        if let Some(val) = self.execute_while_loop(condition, chunk, pos)? {
                            self.reader.update_state(state);
                            self.remove_frame();
                            return Ok(ControlFlow::Return(val));
                        }
                    },
                    Instruction::Condition(pos, main_chain, else_chunk) => {
                        match self.execute_condition_chain(main_chain, else_chunk, pos)? {
                            ControlFlow::None => (),
                            val => {
                                self.reader.update_state(state);
                                self.remove_frame();
                                return Ok(val);
                            }
                        }
                    },
                    instruction => self.execute_instruction(instruction)?
                }
            }

            self.remove_frame();
            self.reader.update_state(state);
            return Ok(ControlFlow::None);
        }

        Ok(ControlFlow::None)
    }

    pub fn add_value(&mut self, name: String, mut val: Value, mutable: bool) {
        val = val.borrow(self);
        self.value_stack.push(val);
        self.value_register.push(ValueRegister {
            key: name,
            id: self.value_stack.len() as u32 - 1,
            depth: self.frames.len() as u32,
            mutable
        });
    }

    pub fn init_core(&mut self) {
        use super::vmcore::{ window, result, math, date };

        macro_rules! add_value {
            ($name:expr, $value:expr) => {
                self.add_value($name.to_string(), Value::from($value), false);
            };
        }

        // Builtin functions
        add_value!("print", builtin::print_api as NativeFn);
        add_value!("typeof", builtin::print_api as NativeFn);
        add_value!("panic", builtin::panic_api as NativeFn);
        add_value!("readline", builtin::readline_api as NativeFn);
        add_value!("prompt", builtin::prompt_api as NativeFn);
        add_value!("confirm", builtin::confirm_api as NativeFn);
        add_value!("Boolean", builtin::bool_api as NativeFn);
        add_value!("Ok", result::ok_api as NativeFn);
        add_value!("Err", result::err_api as NativeFn);

        // Builtin constants
        add_value!("inf", fsize::INFINITY);
        add_value!("NaN", fsize::NAN);

        // Bultin namespaces(dicts)
        let math = dict!(self, {
            "floor": math::floor_api as NativeFn,
            "round": math::round_api as NativeFn,
            "ceil": math::ceil_api as NativeFn,
            "trunc": math::trunc_api as NativeFn,
            "abs": math::abs_api as NativeFn,
            "sqrt": math::sqrt_api as NativeFn,
            "sin": math::sin_api as NativeFn,
            "cos": math::cos_api as NativeFn,
            "tan": math::tan_api as NativeFn,
            "random": math::random_api as NativeFn,
            "randomInt": math::random_int_api as NativeFn,
            "randomRange": math::random_range_api as NativeFn,
            "PI": 3.141592653589793,
            "E": 2.718281828459045,
        });

        let date = dict!(self, { 
            "now": date::get_current_time_ms_api as NativeFn, 
        });

        let mut window = dict!(self, extendable {
            "filename": self.filename.clone(),
            "platform": env::consts::OS,
            "arch": env::consts::ARCH,
            "platformFamily": env::consts::FAMILY,
            "version": "1.0.0",
            "repl": self.flags.get("repl").is_some(),
            "exit": window::exit_api as NativeFn,
            "inspect": window::inspect_api as NativeFn,
            "inspectTiny": window::inspect_tiny_api as NativeFn,
            "sleep": window::sleep_api as NativeFn,
        });

        if self.has_permission("env") {
            dict!(self, extend window => {
                "env": dict!(self, {
                    "get": window::get_env_api as NativeFn,
                    "set": window::set_env_api as NativeFn,
                    "delete": window::delete_env_api as NativeFn,
                    "all": window::all_env_api as NativeFn,
                }),
            });
        }

        add_value!("window", dict!(window));
        add_value!("Math", math);
        add_value!("Date", date);
    }

    pub fn has_permission(&self, name: &str) -> bool {
        self.flags.get(&("use-".to_string() + name)).is_some()
    }

    pub fn create_error(&self, message: String, pos_id: usize) -> RuntimeError {
        let pos = self.reader.get_position(pos_id);
        let (line, col) = get_line_col_by_line_data(self.body_line_data.clone(), pos.start);

        RuntimeError {
            call_frames: self.get_stack_trace(),
            start: pos.start,
            end: pos.end,
            line,
            col,
            message,
            filename: self.filename.clone()
        }
    }

    pub fn create_frame(&mut self, name: String) {
        self.frames.push(Frame {
            name,
            vi: self.value_register.len()
        })
    }

    pub fn remove_frame(&mut self) {
        self.value_register = self.value_register.splice(..self.frames.last().unwrap().vi, [].iter().cloned()).collect();
        self.frames.pop();
    }

    pub fn get_stack_trace(&self) -> Vec<String> {
        if self.flags.get("deep-stack-trace").is_some() {
            self.frames.iter()
            .map(|x| x.name.clone())
            .collect()
        } else {
            self.frames.iter()
            .filter(|x| !x.name.starts_with("@"))
            .map(|x| x.name.clone())
            .collect()
        }
    }

    pub fn get_value(&mut self, id: u32) -> Value {
        let mut i = self.value_register.len() - 1;
        let key = self.reader.get_constant(id as usize);
        let depth = self.frames.len() as u32;

        loop {
            let value = self.value_register[i].clone();
            if (value.key == key) && (value.depth <= depth) {
                return self.value_stack[value.id as usize].clone();
            }

            if i == 0 { return Value::Null }
            i -= 1;
        }
    }

    pub fn value_exists(&self, key: String) -> bool {
        let mut i = self.value_register.len() - 1;
        let depth = self.frames.len() as u32;

        loop {
            let value = self.value_register[i].clone();
            if (value.key == key) && (value.depth <= depth) {
                return true;
            }

            if i == 0 { return false }
            i -= 1;
        }
    }

    pub fn get_value_register(&mut self, id: u32) -> Result<ValueRegister, RuntimeError> {
        let mut i = self.value_register.len() - 1;
        let key = self.reader.get_constant(id as usize);
        let depth = self.frames.len() as u32;

        loop {
            let value = self.value_register[i].clone();
            if (value.key == key) && (value.depth <= depth) {
                return Ok(value);
            }

            if i == 0 {
                return Err(self.create_error(
                    format!("ExpectedValueStack: Expected an value stack for {}.", key),
                    self.reader.ci
                ))
            }

            i -= 1;
        }
    }

}