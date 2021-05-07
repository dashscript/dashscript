use crate::lexer::parser::Position;
use crate::ast::main::AST;
use crate::ast::types::{ StatementType, Statement, Identifier };

#[derive(Debug, Clone)]
pub struct BytecodeCompiler {
    pub ast: AST,
    pub bytes: Vec<u8>,
    pub constants: Vec<String>,
    pub pos_map: Vec<(usize, Position)>
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Opcode {
    True,
    False,
    Null,
    Str,
    StrLong, // The longer index for string
    Word,
    WordLong, // The longer index of word
    Num,
    Var,
    Const,
    Assign,
    Call,
    Attr,
    Add,
    Sub,
    Mult,
    Div,
    Pow,
    Ternary,
    Array,
    Dict,
    Group, 
    Await, 
    Invert,
    Or,
    And,
    In,
    Func,
    RestParam,
    Return,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
    While,
    Break,
    Continue,
    Condition,
    Long, // Used to discriminate is the index u32
    Short // Used to discriminate is the index u8
}

impl From<u8> for Opcode {
    fn from(byte: u8) -> Opcode {
        unsafe { std::mem::transmute::<u8, Opcode>(byte) }
    }
}

impl BytecodeCompiler {

    pub fn new(ast: AST) -> Self {
        let mut this = Self { 
            ast,
            bytes: Vec::new(),
            pos_map: Vec::new(),
            constants: vec!["anonymous".to_string()]
        };

        this.parse_to_bytes();
        this
    }

    pub fn parse_to_bytes(&mut self) {
        let mut i = 0;
        
        while i < self.ast.statements.len() {
            let stmt = self.ast.statements[i].clone();
            self.parse_byte(stmt);
            i += 1;
        }
    }

    pub fn parse_byte(&mut self, stmt: Statement) {
        match &stmt.val {
            StatementType::Var(name, val, is_const) => {
                self.bytes.push(if *is_const { Opcode::Const } else { Opcode::Var } as u8);
                self.push_constant_without_op(name);
                self.load_identifier(val);
            },
            StatementType::Assign(ident, op, val) => {
                self.bytes.push(Opcode::Assign as u8);
                self.load_identifier(ident);
                self.bytes.push(op.clone() as u8);
                self.load_identifier(val);
            },
            StatementType::Return(ident) => {
                self.bytes.push(Opcode::Return as u8);
                self.load_identifier(ident)
            },
            StatementType::Primary(ident) => self.load_identifier(ident),
            StatementType::Break => self.bytes.push(Opcode::Break as u8),
            StatementType::Continue => self.bytes.push(Opcode::Condition as u8),
            StatementType::While(ident, stmts) => {
                self.bytes.push(Opcode::While as u8);
                self.load_identifier(ident);
                
                // TODO(Scientific-Guy): Make a better way to gather chunk as bytecode instead of cloning and performing works.
                let si = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }

                self.bytes.splice(si..si, ((self.bytes.len() - si) as u16).to_le_bytes().iter().cloned());
            },
            StatementType::Condition(main_chain, else_chain) => {
                self.bytes.extend_from_slice(&[Opcode::Condition as u8, main_chain.len() as u8]);
                for (chain, stmts) in main_chain {
                    self.load_identifier(chain);
                    // TODO(Scientific-Guy): Make a better way to gather chunk as bytecode instead of cloning and performing works.
                    let si = self.bytes.len();
                    for stmt in stmts {
                        self.parse_byte(stmt.clone());
                    }

                    self.bytes.splice(si..si, ((self.bytes.len() - si) as u16).to_le_bytes().iter().cloned());
                }

                if else_chain.is_some() {
                    self.bytes.push(1);
                    // TODO(Scientific-Guy): Make a better way to gather chunk as bytecode instead of cloning and performing works.
                    let si = self.bytes.len();
                    for stmt in else_chain.clone().unwrap() {
                        self.parse_byte(stmt);
                    }

                    self.bytes.splice(si..si, ((self.bytes.len() - si) as u16).to_le_bytes().iter().cloned());
                } else {
                    self.bytes.push(0);
                }
            },
            _ => ()
        }
    }

    pub fn get_constant_idx(&self) -> [u8; 8] {
        (self.constants.len() - 1).to_le_bytes()
    }

    pub fn load_identifier(&mut self, ident: &Identifier) {
        match ident {
            Identifier::String(str) => self.push_constant(Opcode::Str, Opcode::StrLong, str),
            Identifier::Number(num) => {
                let cnum = num.clone();
                self.bytes.push(Opcode::Num as u8);
                self.bytes.extend_from_slice(&cnum.to_le_bytes());
            },
            Identifier::Word(wrd) => self.push_constant(Opcode::Word, Opcode::WordLong, wrd),
            Identifier::Call(ident, params) => {
                self.bytes.push(Opcode::Call as u8);
                self.load_identifier(ident);
                self.bytes.push(params.len() as u8);
                for param in params.iter() { 
                    if param.1 { self.bytes.push(Opcode::RestParam as u8) }
                    self.load_identifier(&param.0)
                };
            },
            Identifier::Attribute(ident, attr) => {
                self.bytes.push(Opcode::Attr as u8);
                self.load_identifier(ident);
                self.load_identifier(attr);
            },
            Identifier::Boolean(bool) => self.bytes.push(if *bool { Opcode::True } else { Opcode::False } as u8),
            Identifier::Null => self.bytes.push(Opcode::Null as u8),
            Identifier::Add(a, b) => {
                self.bytes.push(Opcode::Add as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Subtract(a, b) => {
                self.bytes.push(Opcode::Sub as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Multiply(a, b) => {
                self.bytes.push(Opcode::Mult as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Divide(a, b) => {
                self.bytes.push(Opcode::Div as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Pow(a, b) => {
                self.bytes.push(Opcode::Pow as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Ternary(bool, truthy, falsy) => {
                self.bytes.push(Opcode::Ternary as u8);
                self.load_identifier(bool);
                self.load_identifier(truthy);
                self.load_identifier(falsy);
            },
            Identifier::Array(vec) => {
                self.bytes.push(Opcode::Array as u8);
                self.bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for i in vec { self.load_identifier(i) };
            },
            Identifier::Dict(dict) => {
                self.bytes.push(Opcode::Dict as u8);
                self.bytes.extend_from_slice(&(dict.len() as u32).to_le_bytes());
                for i in dict { 
                    self.push_constant_without_op(&i.0.to_owned());
                    self.load_identifier(&i.1);
                }
            },
            Identifier::Func(name, params, stmts) => {
                self.bytes.push(Opcode::Func as u8);
                self.bytes.push(0);
                self.push_constant_without_op(name);
                self.bytes.push(params.len() as u8);
                for param in params {
                    if param.1 { self.bytes.push(Opcode::RestParam as u8) }
                    self.push_constant_without_op(param.0.as_str());
                };
                
                // TODO(Scientific-Guy): Make a better way to gather chunk as bytecode instead of cloning and performing works.
                let si = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }
                
                self.bytes.splice(si..si, ((self.bytes.len() - si) as u16).to_le_bytes().iter().cloned()); 
            },
            Identifier::AsyncFunc(name, params, stmts) => {
                self.bytes.push(Opcode::Func as u8);
                self.bytes.push(1);
                self.push_constant_without_op(name);
                self.bytes.push(params.len() as u8);
                for param in params {
                    if param.1 { self.bytes.push(Opcode::RestParam as u8) }
                    self.push_constant_without_op(param.0.as_str());
                };
                
                // TODO(Scientific-Guy): Make a better way to gather chunk as bytecode instead of cloning and performing works.
                let si = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }

                self.bytes.splice(si..si, ((self.bytes.len() - si) as u16).to_le_bytes().iter().cloned());
            },
            Identifier::Group(ident) => {
                self.bytes.push(Opcode::Group as u8);
                self.load_identifier(ident);
            },
            Identifier::Await(ident) => {
                self.bytes.push(Opcode::Await as u8);
                self.load_identifier(ident);
            },
            Identifier::Invert(ident) => {
                self.bytes.push(Opcode::Invert as u8);
                self.load_identifier(ident)
            },
            Identifier::And(a, b) => {
                self.bytes.push(Opcode::And as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Or(a, b) => {
                self.bytes.push(Opcode::Or as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::In(a, b) => {
                self.bytes.push(Opcode::In as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            },
            Identifier::Condition(a, op, b) => {
                self.bytes.push(match op.as_str() {
                    ">" => Opcode::GreaterThan,
                    "<" => Opcode::LessThan,
                    "==" => Opcode::Equal,
                    "!=" => Opcode::NotEqual,
                    ">=" => Opcode::GreaterThanOrEqual,
                    "<=" => Opcode::LessThanOrEqual,
                    _ => return
                } as u8);

                self.load_identifier(a);
                self.load_identifier(b);
            },
            // _ => ()
        }
    }

    pub fn push_constant_to_bytes(&mut self, op: Opcode) {
        self.bytes.push(op as u8);
        self.bytes.extend_from_slice(&self.get_constant_idx());
    }

    pub fn push_constant(&mut self, op: Opcode, long_op: Opcode, constant: &str) {
        for i in 0..self.constants.len() {
            if constant == self.constants[i] {
                if self.constants.len() >= u8::MAX as usize {
                    self.bytes.push(long_op as u8);
                    self.bytes.extend_from_slice(&(i as u32).to_le_bytes());
                    return
                }

                self.bytes.extend_from_slice(&[op as u8, i as u8]);
                return
            }
        }

        let i = self.constants.len();
        self.constants.push(constant.to_string());
        if i >= u8::MAX as usize {
            self.bytes.push(long_op as u8);
            self.bytes.extend_from_slice(&(i as u32).to_le_bytes());
            return
        }

        self.bytes.extend_from_slice(&[op as u8, i as u8]);
    }

    pub fn push_constant_without_op(&mut self, constant: &str) {
        for i in 0..self.constants.len() {
            if constant == self.constants[i] {
                if self.constants.len() >= u8::MAX as usize {
                    self.bytes.push(Opcode::Long as u8);
                    self.bytes.extend_from_slice(&(i as u32).to_le_bytes());
                    return
                }
                self.bytes.extend_from_slice(&[Opcode::Short as u8, i as u8]);
                return
            }
        }

        let i = self.constants.len();
        self.constants.push(constant.to_string());
        if i >= u8::MAX as usize {
            self.bytes.push(Opcode::Long as u8);
            self.bytes.extend_from_slice(&(i as u32).to_le_bytes());
            return
        }

        self.bytes.extend_from_slice(&[Opcode::Short as u8, i as u8]);
    }

    pub fn push_pos(&mut self, pos: Position) {
        self.pos_map.push((self.bytes.len(), pos));
    }

}