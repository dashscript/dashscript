use super::opcode::*;
use crate::{CompilerError, CompilerErrorKind, Position, ASTBuild, Expr, BinOp, AssignOp};
use crate::ast::constant_pool;

pub type OptionalValue<T> = Option<(T, u8)>;

pub struct FunctionFlags {
    pub instance_function: bool,
    pub async_function: bool
}

impl From<u8> for FunctionFlags {
    fn from(byte: u8) -> Self {
        Self {
            instance_function: byte & Self::INSTANCE == Self::INSTANCE,
            async_function: byte & Self::ASYNC == Self::ASYNC
        }
    }
}

impl Into<u8> for FunctionFlags {
    fn into(self) -> u8 {
        let mut byte = 0;

        if self.instance_function { byte |= Self::INSTANCE }
        if self.async_function { byte |= Self::ASYNC }

        byte
    }
}

impl FunctionFlags {
    pub const INSTANCE: u8 = 1;
    pub const ASYNC: u8 = 2;
}

#[derive(Debug, Clone, Default, Copy)]
pub struct Local {
    pub name: u32,
    pub depth: u16,
    pub is_upvalue: bool,
    pub is_const: bool
}

#[derive(Debug, Clone, Default, Copy)]
pub struct Upvalue {
    pub index: u8,
    pub is_const: bool,
    pub is_local: bool
}

impl PartialEq for Upvalue {
    fn eq(&self, other: &Self) -> bool {
        self.index == other.index
    }
}

impl Eq for Upvalue {}

#[derive(Debug, Clone, Default)]
pub struct Closure {
    pub upvalues: Vec<Upvalue>,
    pub locals: Vec<Local>,
    pub index: u16,
    pub max_slots: u8,
}

#[derive(Debug, Clone, Default)]
pub struct LoopHandler {
    pub ip: usize,
    pub break_offset_holders: Vec<usize>
}

#[derive(Debug, Clone, Default)]
pub struct BytecodeCompiler {
    pub ast: ASTBuild,
    pub bytes: Vec<u8>,
    pub position_map: Vec<(usize, Position)>,
    pub errors: Vec<CompilerError>,
    pub closures: Vec<Closure>,
    pub(crate) index: usize,
    pub(crate) depth: u16,
    pub(crate) line_data: Vec<u32>,
    pub(crate) try_blocks: Vec<(usize, usize, u8)>,
    loop_handler: LoopHandler,
    current_statement_index: usize
}

impl BytecodeCompiler {

    pub fn new(ast: ASTBuild) -> Result<Self, Vec<CompilerError>> {
        let mut line_data = Vec::new();

        for line in ast.body.split("\n") {
            line_data.push(line.len() as u32);
        }
        
        let mut this = Self { 
            ast, 
            line_data,
            closures: vec![Closure::default()],
            ..Default::default()  
        };

        this.bytes.push(0);
        for statement in this.ast.statements.clone()  {
            this.current_statement_index = statement.index;
            if this.load_expr(statement.expr) {
                this.bytes.push(POP);
            }

            this.add_position(statement.index);
            this.index += 1;
        }

        if this.errors.len() == 0 { 
            this.bytes[0] = this.closures.pop().unwrap().max_slots;
            this.end_loop();
            Ok(this) 
        } else { Err(this.errors) }
    }

    // TODO(Scientific-Guy): Make a system to insert `POP` bytecode where the values are useless.
    pub fn load_expr(&mut self, expr: Expr) -> bool {
        macro_rules! load_statement {
            ($statement:expr) => {{
                self.current_statement_index = $statement.index;
                if self.load_expr($statement.expr) {
                    self.bytes.push(POP);
                }

                self.add_position($statement.index);   
            }};
        }

        match expr {
            Expr::Store(constant_id, expr_value, is_constant) => {
                let slot = self.declare(constant_id, is_constant, self.current_statement_index);
                self.load_expr(*expr_value);
                self.bytes.extend_from_slice(&[SET_LOCAL, slot]);
                return false;
            },
            Expr::Assign { target, op, value } => {
                macro_rules! write_bytes {
                    ($set_op:expr, $get_op:expr, $index:expr) => {{
                        match op {
                            AssignOp::Assign => {
                                self.load_expr(*value);
                                self.bytes.extend_from_slice(&[$set_op, $index])
                            },
                            AssignOp::Add => {
                                self.bytes.extend_from_slice(&[$get_op, $index]);
                                self.load_expr(*value);
                                self.bytes.extend_from_slice(&[ADD, $set_op, $index]);
                            },
                            AssignOp::Sub => {
                                self.bytes.extend_from_slice(&[$get_op, $index]);
                                self.load_expr(*value);
                                self.bytes.extend_from_slice(&[SUB, $set_op, $index]);
                            }
                        }
                    }};
                }

                match *target {
                    Expr::Word(constant_id) => {
                        let last_closure_index = self.closures.len() as u16 - 1;
                
                        if let Some((_, index)) = self.get_local(last_closure_index, constant_id) {
                            write_bytes!(SET_LOCAL, GET_LOCAL, index);
                        } else if let Some((_, index)) = self.get_upvalue(last_closure_index, constant_id) {
                            write_bytes!(SET_UPVALUE, GET_UPVALUE, index);
                        } else {
                            self.error(CompilerErrorKind::UnknownValue { name: self.ast.constant_pool.get_string(constant_id).to_string() }, self.current_statement_index);
                        }
                    },
                    Expr::Attribute(target, attr) => {
                        match op {
                            AssignOp::Assign => {
                                self.load_expr(*value);
                                self.load_expr(*target);
                                self.load_expr(*attr);
                                self.bytes.push(SET_ATTR);
                            },
                            AssignOp::Add => {
                                self.load_expr(*target.clone());
                                self.load_expr(*attr.clone());
                                self.bytes.push(GET_ATTR);
                                self.load_expr(*value);
                                self.bytes.push(ADD);
                                self.load_expr(*target);
                                self.load_expr(*attr);
                                self.bytes.push(SET_ATTR);
                            },
                            AssignOp::Sub => {
                                self.load_expr(*target.clone());
                                self.load_expr(*attr.clone());
                                self.bytes.push(GET_ATTR);
                                self.load_expr(*value);
                                self.bytes.push(SUB);
                                self.load_expr(*target);
                                self.load_expr(*attr);
                                self.bytes.push(SET_ATTR);
                            }
                        }
                    },
                    _ => ()
                }

                return false;
            },
            Expr::If { branches, else_branch } => {
                let mut offsets = Vec::new();

                for (condition, statements) in branches {
                    self.load_expr(condition);
                    self.bytes.extend_from_slice(&[JUMP_NOT_IF, 0, 0]);
                    let offset_ip = self.bytes.len();
                    self.depth += 1;
                    for statement in statements {
                        load_statement!(statement);
                    }

                    self.drop_locals();
                    self.bytes.extend_from_slice(&[JUMP, 0, 0]);
                    offsets.push(self.bytes.len());
                    self.update_offset(offset_ip);
                }

                if let Some(statements) = else_branch {
                    self.depth += 1;
                    for statement in statements {
                        load_statement!(statement);
                    }

                    self.drop_locals();
                }

                for offset in offsets {
                    let bytes = ((self.bytes.len() - offset) as u16).to_le_bytes();
                    self.bytes[offset - 2] = bytes[0];
                    self.bytes[offset - 1] = bytes[1];
                }

                return false;
            },
            Expr::Return(return_value) => {
                self.load_expr(*return_value);
                self.bytes.push(RETURN);
                return false;
            },
            Expr::Break => {
                self.bytes.extend_from_slice(&[JUMP, 0, 0]);
                self.loop_handler.break_offset_holders.push(self.bytes.len());
                return false;
            },
            Expr::Continue => {
                self.bytes.push(JUMP_BACK);
                if self.loop_handler.ip == 0 {
                    self.bytes.extend_from_slice(&[1, 0]);
                } else {
                    let len = self.bytes.len();
                    self.bytes.extend_from_slice(&((len - self.loop_handler.ip) as u16).to_le_bytes());
                }

                return false;
            },
            Expr::For { name, in_, inner } => { 
                self.load_expr(*in_);
                self.bytes.push(ITER);
                let loop_ip = self.bytes.len();

                // Basically safe because the pointer it is reading is valid one
                let enclosing = unsafe { std::ptr::read(&mut self.loop_handler) };
                self.loop_handler = LoopHandler { ip: loop_ip - 4, break_offset_holders: Vec::new() };
                self.depth += 1;

                let slot = self.declare(name, false, self.current_statement_index);
                self.bytes.extend_from_slice(&[ITER_NEXT, slot, 0, 0]);

                for statement in inner {
                    load_statement!(statement);
                }

                self.bytes.push(JUMP_BACK);
                self.bytes.extend_from_slice(&((self.bytes.len() - loop_ip + 4) as u16).to_le_bytes());

                let offset_bytes = ((self.bytes.len() - loop_ip) as u16).to_le_bytes();
                self.bytes[loop_ip + 2] = offset_bytes[0];
                self.bytes[loop_ip + 3] = offset_bytes[1];
                
                self.end_loop();
                self.drop_locals();
                self.loop_handler = enclosing;

                return false;
            },
            Expr::While(condition, statements) => { 
                let loop_ip = self.bytes.len();

                // Basically safe because the pointer it is reading is valid one
                let enclosing = unsafe { std::ptr::read(&mut self.loop_handler) };
                self.loop_handler = LoopHandler { ip: loop_ip, break_offset_holders: Vec::new() };
                self.depth += 1;

                self.load_expr(*condition);
                let offset_ip = self.bytes.len();
                self.bytes.extend_from_slice(&[JUMP_NOT_IF, 0, 0]);

                for statement in statements {
                    load_statement!(statement);
                }

                self.bytes.push(JUMP_BACK);
                self.bytes.extend_from_slice(&((self.bytes.len() - loop_ip) as u16).to_le_bytes());
                self.update_offset(offset_ip + 3);
                self.end_loop();
                self.drop_locals();
                self.loop_handler = enclosing;

                return false;
            },
            Expr::Import { module, as_ } => {
                self.bytes.push(IMPORT);
                self.bytes.extend_from_slice(&module.to_le_bytes());
                return match as_ {
                    Some(id) => {
                        self.bytes.push(SET_GLOBAL);
                        self.load_constant_without_op(id);
                        false
                    },
                    None => true
                }
            },
            Expr::Try { try_inner, expect_inner } => {
                self.depth += 1;

                let try_offset_ip = self.bytes.len();
                for statement in try_inner {
                    load_statement!(statement);
                }

                self.drop_locals();
                self.bytes.extend_from_slice(&[JUMP, 0, 0]);
                self.depth += 1;

                let error_slot = self.declare(1, true, self.current_statement_index);
                let offset_ip = self.bytes.len();
                for statement in expect_inner {
                    load_statement!(statement);
                }

                self.drop_locals();
                self.update_offset(offset_ip);
                self.try_blocks.push((try_offset_ip, offset_ip, error_slot));
            },
            Expr::Null => self.bytes.push(NULL),
            Expr::Boolean(boolean) => self.bytes.push(boolean as u8),
            Expr::String(constant_id) => self.load_constant(constant_id, STRING, STRING_LONG),
            Expr::Word(constant_id) => {
                let last_closure_index = self.closures.len() as u16 - 1;
                
                if let Some((_, index)) = self.get_local(last_closure_index, constant_id) {
                    self.bytes.extend_from_slice(&[GET_LOCAL, index]);
                } else if let Some((_, index)) = self.get_upvalue(last_closure_index, constant_id) {
                    self.bytes.extend_from_slice(&[GET_UPVALUE, index]);
                } else {
                    self.bytes.push(GET_GLOBAL);
                    self.load_constant_without_op(constant_id);
                }
            },
            Expr::Int(constant_id) => self.load_constant(constant_id, INT, INT_LONG),
            Expr::Float(constant_id) => self.load_constant(constant_id, FLOAT, FLOAT_LONG),
            Expr::BinaryOperation { lhs, op, rhs } => {
                macro_rules! push_op {
                    ($op:expr) => {{
                        self.load_expr(*lhs);
                        self.load_expr(*rhs);
                        self.bytes.push($op);
                    }};
                }
                
                match op {
                    BinOp::Add => push_op!(ADD),
                    BinOp::Subtract => push_op!(SUB),
                    BinOp::Multiply => push_op!(MUL),
                    BinOp::Divide => push_op!(DIV),
                    BinOp::Power => push_op!(POW),
                    BinOp::Rem => push_op!(REM),
                    BinOp::And => push_op!(AND),
                    BinOp::Equal => push_op!(EQ),
                    BinOp::NotEqual => push_op!(NEQ),
                    BinOp::GreaterThan => push_op!(GT),
                    BinOp::GreaterThanOrEqual => push_op!(GTE),
                    BinOp::LessThan => push_op!(LT),
                    BinOp::LessThanOrEqual => push_op!(LTE),
                    BinOp::BitAnd => push_op!(BITAND),
                    BinOp::BitOr => push_op!(BITOR),
                    BinOp::BitXor => push_op!(BITXOR),
                    BinOp::Shr => push_op!(SHR),
                    BinOp::Shl => push_op!(SHL),
                    BinOp::Or => {
                        self.load_expr(*lhs);
                        self.bytes.extend_from_slice(&[JUMP_IF, 0, 0]);
                        let offset_ip = self.bytes.len();
                        self.bytes.push(POP);
                        self.load_expr(*rhs);
                        self.update_offset(offset_ip);
                    }
                }
            },
            Expr::Attribute(target, attribute) => {
                self.load_expr(*target);
                self.load_expr(*attribute);
                self.bytes.push(GET_ATTR);
            },
            Expr::Not(expr) => {
                self.load_expr(*expr);
                self.bytes.push(NOT);
            },
            Expr::Call(target, params) => {
                let len = params.len() as u8;
                // TODO(Scientific-Guy): Add bytecode support for rest parameters
                for expr in params {
                    self.load_expr(expr);
                }
                
                let op = match *target {
                    Expr::Attribute(parent, method) => {
                        self.load_expr(*parent);
                        self.load_expr(*method);
                        CALL_CHILD
                    },
                    expr => {
                        self.load_expr(expr);
                        CALL
                    }
                };

                self.bytes.extend_from_slice(&[op, len]);
            },
            Expr::Array(vector) => {
                let len = vector.len() as u32;
                for expr in vector {
                    self.load_expr(expr);
                }

                self.bytes.push(ARRAY);
                self.load_constant_without_op(len);
            },
            Expr::Dict(dict) => {
                let len = dict.len() as u32;
                for (constant_id, expr) in dict {
                    self.load_constant(constant_id, STRING, STRING_LONG);
                    self.load_expr(expr);
                }
                
                self.bytes.push(DICT);
                self.load_constant_without_op(len);
            },
            Expr::Function { name, parameters, inner, is_async } => {
                self.bytes.extend_from_slice(&[FUNC, 0, 0]);
                let offset_ip = self.bytes.len();

                // Basically safe because the pointer it is reading is valid one
                let enclosing_loop = unsafe { std::ptr::read(&mut self.loop_handler) };
                self.loop_handler = LoopHandler { ip: self.bytes.len() - 1, break_offset_holders: Vec::new() };

                self.depth += 1;
                let mut closure = Closure {
                    locals: Vec::new(),
                    upvalues: Vec::new(),
                    index: self.depth,
                    max_slots: parameters.len() as u8
                };

                for constant_id in parameters {
                    closure.locals.push(Local {
                        depth: self.depth,
                        is_upvalue: false,
                        is_const: false,
                        name: constant_id
                    });
                }

                self.closures.push(closure);
                for statement in inner {
                    load_statement!(statement);
                }

                // Just incase if there is no return statement at the end of the
                // function it can mess up the vm.
                if *self.bytes.last().unwrap() != RETURN {
                    self.bytes.extend_from_slice(&[NULL, RETURN]);
                }
            
                self.depth -= 1;
                self.update_offset(offset_ip);

                let closure = self.closures.pop().unwrap();
                self.bytes.extend_from_slice(&[closure.max_slots, closure.upvalues.len() as u8, is_async as u8]);
                for upvalue in &closure.upvalues {
                    self.bytes.extend_from_slice(&[upvalue.is_local as u8, upvalue.index]);
                }

                self.load_constant_without_op(name);
                self.end_loop();
                self.loop_handler = enclosing_loop;

                if name != constant_pool::ANONYMOUS_CONSTANT {
                    let slot = self.declare(name, true, self.current_statement_index);
                    self.bytes.extend_from_slice(&[SET_LOCAL, slot]);
                }

                return false;
            },
            Expr::Group(group) => { 
                self.load_expr(*group); 
            },
            Expr::Ternary(target, truthy, falsy) => {
                self.load_expr(*target);
                self.bytes.extend_from_slice(&[JUMP_NOT_IF, 0, 0]);
                let offset_ip = self.bytes.len();
                
                self.load_expr(*truthy);
                self.bytes.extend_from_slice(&[JUMP, 0, 0]);
                let jump_offset_ip = self.bytes.len();
                self.update_offset(offset_ip);
                
                self.load_expr(*falsy);
                self.update_offset(jump_offset_ip);
            },
            _ => ()
        }

        true
    }

    pub fn load_constant(&mut self, constant_id: u32, short_op: u8, long_op: u8) {
        if constant_id < u8::MAX as u32 {
            self.bytes.extend_from_slice(&[short_op, constant_id as u8]);
        } else {
            self.bytes.push(long_op);
            self.bytes.extend_from_slice(&constant_id.to_le_bytes());
        }
    }

    pub fn load_constant_without_op(&mut self, constant_id: u32) {
        // 0 has taken the short op and 1 has taken as long op
        if constant_id < u8::MAX as u32 {
            self.bytes.extend_from_slice(&[0, constant_id as u8]);
        } else {
            self.bytes.push(1);
            self.bytes.extend_from_slice(&constant_id.to_le_bytes());
        }
    }

    pub fn drop_locals(&mut self) {
        self.depth -= 1;
        let closure = self.closures.last_mut().unwrap();
        let mut index = closure.locals.len();

        if index == 0 {
            return
        }

        while index != 0 {
            index -= 1;

            let local = closure.locals[index];
            if local.depth <= self.depth { 
                return;
            }
            
            self.bytes.push(if local.is_upvalue { CLOSE_UPVALUE } else { POP });
            closure.locals.pop();
        }
    }

    pub fn update_offset(&mut self, offset_ip: usize) {
        let offset = self.bytes.len() - offset_ip;
        let offset_bytes = (offset as u16).to_le_bytes();
        self.bytes[offset_ip - 2] = offset_bytes[0];
        self.bytes[offset_ip - 1] = offset_bytes[1];
    }

    pub fn add_position(&mut self, start_index: usize) {
        if let Some(stmt) = self.ast.statements.get(self.index + 1) {
            self.position_map.push((
                self.bytes.len(), 
                Position {
                    start: start_index as u32, 
                    end: stmt.index as u32
                }
            ))
        } else {
            self.position_map.push((
                self.bytes.len(), 
                Position {
                    start: start_index as u32, 
                    end: (self.ast.body.len() - 1) as u32 
                }
            ))
        }
    }
    
    pub fn end_loop(&mut self) {
        let length = self.bytes.len();
        for holder in self.loop_handler.break_offset_holders.iter() {
            let bytes = ((length - holder) as u16).to_le_bytes();
            self.bytes[holder - 2] = bytes[0];
            self.bytes[holder - 1] = bytes[1];
        }
    }

    pub fn error(&mut self, kind: CompilerErrorKind, start_index: usize) {
        let line = self.line_from_start_index(start_index as u32);
        self.errors.push(CompilerError { kind, line });
    }

    pub fn get_local(&self, index: u16, name: u32) -> OptionalValue<Local> {
        let closure = self.closures.get(index as usize).unwrap();
        let mut index = closure.locals.len();
        
        if index == 0 { 
            return None 
        };
    
        while index > 0 {
            index -= 1;
            if closure.locals[index].name == name {
                return Some((closure.locals[index].clone(), index as u8));
            }
        }
    
        None
    }

    pub fn get_local_from_closure(&self, closure: &Closure, name: u32) -> OptionalValue<Local> {
        let mut index = closure.locals.len();
        
        if index == 0 { 
            return None 
        };
    
        while index > 0 {
            index -= 1;
            if closure.locals[index].name == name {
                return Some((closure.locals[index].clone(), index as u8));
            }
        }
    
        None
    }

    pub fn get_upvalue(&mut self, index: u16, constant_id: u32) -> OptionalValue<Upvalue> {
        if index == 0 {
            return None;
        }
        
        if let Some((local, local_index)) = self.get_local(index - 1, constant_id) {
            let closure = self.closures.get_mut(index as usize).unwrap();
            let length = closure.upvalues.len();
            let upvalue = Upvalue {
                index: local_index,
                is_const: local.is_const,
                is_local: true
            };

            closure.upvalues.push(upvalue);
            self.closures[index as usize - 1].locals[local_index as usize].is_upvalue = true;

            if length >= u8::MAX as usize {
                self.error(CompilerErrorKind::TooManyUpvalues, self.current_statement_index);
            }

            return Some((upvalue, length as u8));
        }

        if let Some((upvalue, upvalue_index)) = self.get_upvalue(index - 1, constant_id) {
            let closure = self.closures.get_mut(index as usize).unwrap();
            let length = closure.upvalues.len();
            let upvalue = Upvalue {
                index: upvalue_index,
                is_const: upvalue.is_const,
                is_local: false
            };

            closure.upvalues.push(upvalue);
            if length >= u8::MAX as usize {
                self.error(CompilerErrorKind::TooManyUpvalues, self.current_statement_index);
            }
    
            return Some((upvalue, length as u8));
        }
    
        None
    }

    pub fn declare(&mut self, constant_id: u32, is_constant: bool, start_index: usize) -> u8 {
        let last_closure = self.closures.last_mut().unwrap();
        let mut index = last_closure.locals.len();

        while index > 0 {
            index -= 1;
            
            if last_closure.locals[index].name == constant_id {
                if last_closure.locals[index].is_const {
                    let name = self.ast.constant_pool.get_string(constant_id);
                    self.error(CompilerErrorKind::AssignmentToConstant { name: name.to_string() }, start_index);
                    return 0;
                }
            }
        }

        last_closure.locals.push(Local {
            name: constant_id,
            depth: self.depth,
            is_const: is_constant,
            is_upvalue: false
        });

        let max_slots = last_closure.max_slots;
        if max_slots >= u8::MAX {
            self.error(CompilerErrorKind::TooManyLocals, start_index);
        } else {
            last_closure.max_slots += 1;
        }

        max_slots
    }

    pub(crate) fn line_from_start_index(&self, start_index: u32) -> usize {
        let mut current_index = 0;
        let mut index = 0;

        for line_length in self.line_data.iter() {
            current_index += line_length;
            index += 1;

            if current_index >= start_index {
                return index;
            }
        }

        index
    }

}