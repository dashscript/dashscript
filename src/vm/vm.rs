use std::env;
use std::collections::HashMap;
use crate::common::fsize;

use super::{ 
    value::{ Value, ValueRegister, ValueIndex },
    vmcore::{ builtin, window, result, memory, into_value_dict, math, date, builtin::inspect }
};

use crate::{
    lexer::parser::Position,
    common::{ get_line_col_by_line_data },
    bytecode::{
        main::BytecodeCompiler,
        reader::{
            BytecodeReader,
            InstructionValue,
            Instruction
        }
    }
};

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

    pub fn new(compiler: BytecodeCompiler, filename: String, body: String, permissions: Vec<String>) -> Result<VM, RuntimeError> {
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

    pub fn execute_body(&mut self) -> Result<(), RuntimeError> {
        let mut instruction = Some(self.reader.init());
        self.current_depth += 1;

        while instruction.is_some() {
            match instruction.unwrap() {
                Instruction::Var(pos, name, value) => {
                    let name = self.reader.get_constant(name as usize);
                    let value = self.execute_value(value, pos)?;
                    self.add_value(name, value, true);
                },
                Instruction::Assign(pos, target, op, value) => {
                    let val = self.execute_value(value, pos)?;
                    self.execute_assignment(target, pos, val, op, true, 0)?;
                },
                Instruction::Value(pos, val) => {
                    self.execute_value(val, pos)?;
                }
            }

            instruction = self.reader.next();
        }
        
        Ok(())
    }

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
                match op {
                    0 => {
                        if last_stack {
                            if !old_val.mutable {
                                return Err(self.create_error(
                                    format!("AssignmentToConstant: You cannot assign a value to a constant"),
                                    pos
                                ))
                            }

                            self.value_stack[old_val.id as usize] = val;
                            Ok(0)
                        } else {
                            Ok(old_val.id)
                        }
                    },
                    _ => Ok(0)
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
                                    0 => {
                                        self.value_stack[old_val.0 as usize] = val;
                                    },
                                    _ => () // Ignore
                                }
                            }

                            Ok(target_id)
                        },
                        None => {
                            if last_stack {
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
            InstructionValue::True => Ok(Value::True),
            InstructionValue::False => Ok(Value::False),
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
                            "length" => Value::Num(str.len() as fsize),
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
                    Value::NativeFn(func) => {
                        self.call_stack.push("NativeFunction".to_string());
                        let res = Ok(func(call_params, self));
                        self.call_stack.pop();
                        res
                    },
                    _ => Err(self.create_error(
                        format!("UnexpectedTypeError: Type {} is not callable.", call_body.type_as_str()), 
                        pos
                    ))
                }
            },
            _ => Err(self.create_error(
                format!("UnknownRuntimeError: Unexpected value while rendering."), 
                pos
            ))
        }
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
        self.add_value("print".to_string(), Value::NativeFn(builtin::print_api), false);
        self.add_value("typeof".to_string(), Value::NativeFn(builtin::typeof_api), false);
        self.add_value("panic".to_string(), Value::NativeFn(builtin::panic_api), false);
        self.add_value("readline".to_string(), Value::NativeFn(builtin::readline_api), false);
        self.add_value("prompt".to_string(), Value::NativeFn(builtin::prompt_api), false);
        self.add_value("confirm".to_string(), Value::NativeFn(builtin::confirm_api), false);
        self.add_value("Ok".to_string(), Value::NativeFn(result::ok_api), false);
        self.add_value("Err".to_string(), Value::NativeFn(result::err_api), false);
        self.add_value("bool".to_string(), Value::NativeFn(builtin::bool_api), false);
        self.add_value("inf".to_string(), Value::Num(fsize::INFINITY), false);

        let math_entries = into_value_dict(vec![
            ("floor", Value::NativeFn(math::floor_api), false),
            ("round", Value::NativeFn(math::round_api), false),
            ("ceil", Value::NativeFn(math::ceil_api), false),
            ("trunc", Value::NativeFn(math::trunc_api), false),
            ("abs", Value::NativeFn(math::abs_api), false),
            ("sqrt", Value::NativeFn(math::sqrt_api), false),
            ("sin", Value::NativeFn(math::sin_api), false),
            ("cos", Value::NativeFn(math::cos_api), false),
            ("tan", Value::NativeFn(math::tan_api), false),
            ("random", Value::NativeFn(math::random_api), false),
            ("randomRange", Value::NativeFn(math::random_range_api), false),
            ("randomInt", Value::NativeFn(math::random_int_api), false),
            ("PI", Value::Num(3.141592653589793), false),
            ("E", Value::Num(2.718281828459045), false)
        ], self);

        let date_entries = into_value_dict(vec![
            ("now", Value::NativeFn(date::get_current_time_ms_api), false)
        ], self);

        let mut window_entries = vec![
            ("filename", Value::Str(self.filename.clone()), false),
            ("platform", Value::Str(env::consts::OS.to_string()), false),
            ("arch", Value::Str(env::consts::ARCH.to_string()), false),
            ("platformFamily", Value::Str(env::consts::FAMILY.to_string()), false),
            ("exit", Value::NativeFn(window::exit_api), false),
            ("inspect", Value::NativeFn(window::inspect_api), false),
            ("sleep", Value::NativeFn(window::sleep_api), false)
        ];

        if self.permissions.contains(&"env".to_string()) {
            window_entries.push((
                "env", 
                (into_value_dict(vec![
                    ("get", Value::NativeFn(window::get_env_api), false),
                    ("set", Value::NativeFn(window::set_env_api), false),
                    ("all", Value::NativeFn(window::all_env_api), false),
                    ("delete", Value::NativeFn(window::delete_env_api), false)
                ], self)),
                false
            ));
        }

        if self.permissions.contains(&"memory".to_string()) {
            window_entries.push((
                "memory",
                into_value_dict(vec![
                    ("getByPointer", Value::NativeFn(memory::get_by_pointer_api), false),
                    ("push", Value::NativeFn(memory::push_api), false),
                    ("len", Value::NativeFn(memory::len_api), false)
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