use std::env;
use std::collections::HashMap;
use std::fmt;
use super::value::{ Value, ValueRegister, ValueIndex };
use super::vmcore::{ builtin, window, result, memory, into_value_dict, math, date, builtin::inspect };
use super::vmcore;
use crate::lexer::parser::Position;
use crate::bytecode::reader::LogicalOperator;
use crate::common::{ fsize, get_line_col_by_line_data };
use crate::bytecode::main::BytecodeCompiler;
use crate::bytecode::reader::{ BytecodeReader, InstructionValue, Instruction };

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

#[derive(Clone)]
pub struct VM {
    pub pos_map: Vec<(usize, Position)>,
    pub filename: String,
    pub reader: BytecodeReader,
    pub call_stack: Vec<String>,
    pub value_stack: Vec<Value>,
    pub value_register: Vec<ValueRegister>,
    pub current_depth: u16,
    pub body_line_data: Vec<usize>,
    pub permissions: Vec<String>
}

impl VM {

    pub fn new(compiler: BytecodeCompiler, filename: String, body: String, permissions: Vec<String>) -> Result<Self, RuntimeError> {
        let mut vm = Self {
            pos_map: compiler.pos_map.clone(),
            filename: filename,
            reader: BytecodeReader::new(compiler),
            call_stack: Vec::new(),
            value_stack: Vec::<Value>::new(),
            value_register: Vec::new(),
            current_depth: 0,
            body_line_data: Vec::new(),
            permissions
        };

        for line in body.split("\n").collect::<Vec<&str>>().iter() {
            vm.body_line_data.push(line.len());
        }

        vm.init_core();
        vm.execute_body()?;
        Ok(vm)
    }

    pub fn default(filename: String, permissions: Vec<String>) -> Self {
        Self {
            pos_map: Vec::new(),
            filename,
            reader: BytecodeReader::default(),
            call_stack: Vec::new(),
            value_stack: Vec::<Value>::new(),
            value_register: Vec::new(),
            current_depth: 0,
            body_line_data: Vec::new(),
            permissions
        }
    }

    pub fn execute_body(&mut self) -> Result<(), RuntimeError> {
        let mut instruction = Some(self.reader.init());
        self.current_depth += 1;

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
                self.add_value(name, value, true);
                Ok(())
            },
            Instruction::Const(pos, name, value) => {
                let name = self.reader.get_constant(name as usize);
                let value = self.execute_value(value, pos)?;
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
                for (instruction_value, chunk) in main_chain {
                    if builtin::bool(self.execute_value(instruction_value, pos)?) {
                        let mut reader = self.reader.clone();
                        reader.pos_map.clear();
                        reader.ci = 0;
                        reader.len = chunk.len();
                        reader.bytes = chunk.clone();
                        self.current_depth += 1;

                        while reader.ci+1 < reader.len {
                            self.execute_instruction(reader.parse_byte(chunk[reader.ci]))?;
                        }

                        self.current_depth -= 1;
                        return Ok(());
                    }
                }

                if else_chunk.is_some() {
                    let mut reader = self.reader.clone();
                    let chunk = else_chunk.unwrap();
                    reader.pos_map.clear();
                    reader.ci = 0;
                    reader.len = chunk.len();
                    reader.bytes = chunk.clone();
                    self.current_depth += 1;

                    while reader.ci+1 < reader.len {
                        self.execute_instruction(reader.parse_byte(chunk[reader.ci]))?;
                    }

                    self.current_depth -= 1;
                }

                Ok(())
            },
            // TODO(Scientific-Guy): Make a better execution for the while loop.
            Instruction::While(pos, condition, chunk) => {
                let mut instructions = Vec::new();
                let mut reader = self.reader.clone();
                reader.pos_map.clear();
                reader.ci = 0;
                reader.len = chunk.len();
                reader.bytes = chunk.clone();
                self.current_depth += 1;

                while reader.ci < reader.len {
                    instructions.push(reader.parse_byte(reader.bytes[reader.ci]));
                }

                while builtin::bool(self.execute_value(condition.clone(), pos)?) {
                    for instruction in &instructions {
                        if let Instruction::Break(_) = instruction {
                            return Ok(());
                        } else {
                            self.execute_instruction(instruction.clone())?;
                        }
                    }
                }

                self.current_depth -= 1;
                Ok(())
            },
            Instruction::Return(pos, val) => {
                println!("{}", builtin::inspect(self.execute_value(val, pos)?, self));
                std::process::exit(0);
            },
            Instruction::Break(_) => std::process::exit(0)
        }
    }

    // TODO(Scientific-Guy): Make a better assignment executor.
    pub fn execute_assignment(
        &mut self, 
        value: InstructionValue, 
        pos: usize, 
        val: Value,
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

                    match op {
                        0 => self.value_stack[old_val.id as usize] = val,
                        1 => self.value_stack[old_val.id as usize] = vmcore::add_values(self.value_stack[old_val.id as usize].clone(), val),
                        2 => self.value_stack[old_val.id as usize] = vmcore::sub_values(self.value_stack[old_val.id as usize].clone(), val, self),
                        _ => ()
                    }

                    Ok(0)
                } else {
                    Ok(old_val.id)
                }
            },
            InstructionValue::Attr(raw_target, raw_attr) => {
                let target_id = self.execute_assignment(*raw_target, pos, val.clone(), op, false, id)?;
                let target = self.value_stack[target_id as usize].clone();
                let attr = self.execute_value(*raw_attr, pos)?;
                let attr_index = attr.to_value_index();

                match target.clone() {
                    Value::Dict(mut entries) => match entries.get(&attr_index) {
                        Some(old_val) => {
                            if last_stack {
                                if !old_val.1 {
                                    let msg = format!("UnexpectedAttributeAccess: Property {} is readonly at {}.", inspect(attr, self), inspect(target.clone(), self));
                                    return Err(self.create_error(msg, pos))
                                }

                                match op {
                                    0 => self.value_stack[old_val.0 as usize] = val,
                                    1 => self.value_stack[old_val.0 as usize] = vmcore::add_values(self.value_stack[old_val.0 as usize].clone(), val),
                                    2 => self.value_stack[old_val.0 as usize] = vmcore::sub_values(self.value_stack[old_val.0 as usize].clone(), val, self),
                                    _ => ()
                                }
                            }

                            Ok(target_id)
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

                                self.value_stack.push(val);
                                entries.insert(attr_index, (self.value_stack.len() as u32 - 1, true));
                                self.value_stack[target_id as usize] = Value::Dict(entries);
                                Ok(target_id)
                            } else {
                                Err(self.create_error(
                                    "UnexpectedAttributeAccess: You cannot access attributes of null.".to_string(), 
                                    pos
                                ))
                            }
                        }
                    },
                    _ => Err(self.create_error(
                        format!("UnexpectedAttributeAccess: You cannot access attributes of type {}.", target.type_as_str()), 
                        pos
                    ))
                }
            },
            _ => return Err(self.create_error(
                format!("UnexpectedAssignment: You can assing over only a mutable variable only."), 
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
                    self.value_stack.push(val);
                    entries.insert(
                        ValueIndex::Str(self.reader.get_constant(entry.0 as usize)), 
                        (self.value_stack.len() as u32 - 1, true)
                    );
                }
                
                Ok(Value::Dict(entries))
            },
            InstructionValue::Attr(raw_body, raw_attr) => {
                let attr = self.execute_value(*raw_attr, pos)?.to_value_index();
                let body = self.execute_value(*raw_body, pos)?;

                match body {
                    Value::Dict(entries) => match entries.get(&attr) {
                        Some(val) => Ok(self.value_stack[val.0 as usize].clone()),
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
                                        Ok(num) => result::ok(Value::Num(num), vm),
                                        Err(_) => result::err(Value::Str("Improper number.".to_string()), vm)
                                    }
                                } else {
                                    result::err(Value::Str("Improper number.".to_string()), vm)
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
                            _ => return Err(self.create_error(
                                format!("UnexpectedAttributeAccess: Unknown property {:?} accessing a string.", attr),
                                pos
                            ))
                        }),
                        ValueIndex::Num(index) => Ok(match str.chars().nth(index.0 as usize) {
                            Some(char) => Value::Str(char.to_string()),
                            None => Value::Null
                        }),
                        _ => return Err(self.create_error(
                            format!("UnexpectedAttributeAccess: Unknown property {:?} accessing a string.", attr),
                            pos
                        )), 
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
                        attr => return Err(self.create_error(
                            format!("UnexpectedAttributeAccess: Cannot find attribute {:?} in array.", attr), 
                            pos
                        ))
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
                    call_params.push(self.execute_value(param.clone(), pos)?)
                }

                match call_body {
                    Value::NativeFn(this, func) => {
                        self.call_stack.push("NativeFunction".to_string());
                        let res = Ok(func(*this, call_params, self));
                        self.call_stack.pop();
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
                let val = Value::Func(id, params, chunk, is_async);
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
        param_ids: Vec<u32>,
        params: Vec<Value>,
        chunk: Vec<u8>
    ) -> Result<Value, RuntimeError> {
        self.call_stack.push(self.reader.get_constant(id as usize));
        self.current_depth += 1;

        // TODO(Scientific-Guy): Make a better chunk reader instead of cloning the reader.
        let mut reader = self.reader.clone();
        // Cleaning to prevent unwanted cache.
        reader.pos_map.clear();
        reader.len = chunk.len();
        reader.ci = 0;
        reader.bytes = chunk;

        for i in 0..param_ids.len() {
            self.add_value(
                self.reader.get_constant(param_ids[i] as usize),
                match params.get(i as usize) {
                    Some(val) => val.clone(),
                    None => Value::Null
                },
                true
            );
        }

        loop {
            if (reader.ci+1) < reader.len {
                match reader.parse_byte(reader.bytes[reader.ci]) {
                    Instruction::Return(pos, val) => return self.execute_value(val, pos),
                    instruction => {
                        self.execute_instruction(instruction)?;
                    }
                }
            } else {
                break
            }
        }

        self.call_stack.pop();
        self.current_depth -= 1;
        Ok(Value::Null)
    }

    pub fn add_value(&mut self, name: String, val: Value, mutable: bool) {
        self.value_stack.push(val);
        self.value_register.push(ValueRegister {
            key: name,
            id: self.value_stack.len() as u32 - 1,
            depth: self.current_depth,
            mutable
        });
    }

    pub fn init_core(&mut self) {
        self.add_value("print".to_string(), Value::to_native_fn(builtin::print_api), false);
        self.add_value("typeof".to_string(), Value::to_native_fn(builtin::typeof_api), false);
        self.add_value("panic".to_string(), Value::to_native_fn(builtin::panic_api), false);
        self.add_value("readline".to_string(), Value::to_native_fn(builtin::readline_api), false);
        self.add_value("prompt".to_string(), Value::to_native_fn(builtin::prompt_api), false);
        self.add_value("confirm".to_string(), Value::to_native_fn(builtin::confirm_api), false);
        self.add_value("inf".to_string(), Value::Num(fsize::INFINITY), false);
        self.add_value("boolean".to_string(), Value::to_native_fn(builtin::bool_api), false);
        self.add_value("Ok".to_string(), Value::to_native_fn(result::ok_api), false);
        self.add_value("Err".to_string(), Value::to_native_fn(result::err_api), false);

        let math_entries = into_value_dict(vec![
            ("floor", Value::to_native_fn(math::floor_api), false),
            ("round", Value::to_native_fn(math::round_api), false),
            ("ceil", Value::to_native_fn(math::ceil_api), false),
            ("trunc", Value::to_native_fn(math::trunc_api), false),
            ("abs", Value::to_native_fn(math::abs_api), false),
            ("sqrt", Value::to_native_fn(math::sqrt_api), false),
            ("sin", Value::to_native_fn(math::sin_api), false),
            ("cos", Value::to_native_fn(math::cos_api), false),
            ("tan", Value::to_native_fn(math::tan_api), false),
            ("random", Value::to_native_fn(math::random_api), false),
            ("randomRange", Value::to_native_fn(math::random_range_api), false),
            ("randomInt", Value::to_native_fn(math::random_int_api), false),
            ("PI", Value::Num(3.141592653589793), false),
            ("E", Value::Num(2.718281828459045), false)
        ], self);

        let date_entries = into_value_dict(vec![
            ("now", Value::to_native_fn(date::get_current_time_ms_api), false)
        ], self);

        let mut window_entries = vec![
            ("filename", Value::Str(self.filename.clone()), false),
            ("platform", Value::Str(env::consts::OS.to_string()), false),
            ("arch", Value::Str(env::consts::ARCH.to_string()), false),
            ("platformFamily", Value::Str(env::consts::FAMILY.to_string()), false),
            ("version", Value::Str("1.0.0".to_string()), false),
            ("exit", Value::to_native_fn(window::exit_api), false),
            ("inspect", Value::to_native_fn(window::inspect_api), false),
            ("sleep", Value::to_native_fn(window::sleep_api), false)
        ];

        if self.permissions.contains(&"env".to_string()) {
            window_entries.push((
                "env", 
                (into_value_dict(vec![
                    ("get", Value::to_native_fn(window::get_env_api), false),
                    ("set", Value::to_native_fn(window::set_env_api), false),
                    ("all", Value::to_native_fn(window::all_env_api), false),
                    ("delete", Value::to_native_fn(window::delete_env_api), false)
                ], self)),
                false
            ));
        }

        if self.permissions.contains(&"memory".to_string()) {
            window_entries.push((
                "memory",
                into_value_dict(vec![
                    ("getByPointer", Value::to_native_fn(memory::get_by_pointer_api), false),
                    ("push", Value::to_native_fn(memory::push_api), false),
                    ("len", Value::to_native_fn(memory::len_api), false)
                ], self),
                false
            ))
        }

        let window = into_value_dict(window_entries, self);
        self.add_value("Math".to_string(), math_entries, false);
        self.add_value("Date".to_string(), date_entries, false);
        self.add_value("window".to_string(), window, false);
    }

    pub fn create_error(&self, message: String, pos_id: usize) -> RuntimeError {
        let pos = self.reader.get_position(pos_id);
        let (line, col) = get_line_col_by_line_data(self.body_line_data.clone(), pos.start);

        RuntimeError {
            call_frames: self.call_stack.clone(),
            start: pos.start,
            end: pos.end,
            line,
            col,
            message,
            filename: self.filename.clone()
        }
    }

    pub fn get_value(&mut self, id: u32) -> Value {
        let mut i = self.value_register.len() - 1;
        let key = self.reader.get_constant(id as usize);

        loop {
            let value = self.value_register[i].clone();
            if value.key == key && (value.depth <= self.current_depth) {
                return self.value_stack[value.id as usize].clone();
            }
            
            if i == 0 { break }
            i -= 1;
        }

        Value::Null
    }

    pub fn get_value_register(&mut self, id: u32) -> Result<ValueRegister, RuntimeError> {
        let mut i = self.value_register.len() - 1;
        let key = self.reader.get_constant(id as usize);

        loop {
            let value = self.value_register[i].clone();
            if value.key == key && (value.depth <= self.current_depth) {
                return Ok(value)
            }
            
            if i == 0 {
                break Err(self.create_error(
                    format!("ExpectedValueStack: Expected an valye stack for {}.", key),
                    self.reader.ci
                ))
            }

            i -= 1;
        }
    }

}