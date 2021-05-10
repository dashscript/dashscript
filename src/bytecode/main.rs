use dashscript_ast::{Position, AST, StatementType, Statement, Identifier, Condition};

#[derive(Debug, Clone)]
pub struct BytecodeCompiler {
    pub ast: AST,
    pub bytes: Vec<u8>,
    pub pos_map: Vec<(usize, Position)>,
    pub ci: usize,
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
    NumLong, // The longer index of number
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

impl From<&Condition> for Opcode {
    fn from(condition: &Condition) -> Opcode {
        match condition {
            Condition::Equal => Opcode::Equal,
            Condition::NotEqual => Opcode::NotEqual,
            Condition::GreaterThan => Opcode::GreaterThan,
            Condition::GreaterThanOrEqual => Opcode::GreaterThanOrEqual,
            Condition::LessThan => Opcode::LessThanOrEqual,
            Condition::LessThanOrEqual => Opcode::LessThanOrEqual
        }
    }
}

impl BytecodeCompiler {

    pub fn new(ast: AST) -> Self {
        let mut this = Self { 
            ast,
            bytes: Vec::new(),
            pos_map: Vec::new(),
            ci: 0
        };

        this.parse_to_bytes();
        this
    }

    pub fn parse_to_bytes(&mut self) {
        for statement in self.ast.statements.clone() {
            self.parse_byte(statement);
            self.ci += 1;
        }
    }

    pub fn parse_byte(&mut self, stmt: Statement) {
        match &stmt.val {
            StatementType::Var(register_id, val, is_const) => {
                self.add_position(stmt.start_index);
                self.bytes.push(if *is_const { Opcode::Const } else { Opcode::Var } as u8);
                self.push_constant_without_op(*register_id);
                self.load_identifier(val);
            },
            StatementType::Assign(ident, op, val) => {
                self.add_position(stmt.start_index);
                self.bytes.push(Opcode::Assign as u8);
                self.load_identifier(ident);
                self.bytes.push(op.clone() as u8);
                self.load_identifier(val);
            },
            StatementType::Return(ident) => {
                self.add_position(stmt.start_index);
                self.bytes.push(Opcode::Return as u8);
                self.load_identifier(ident)
            },
            StatementType::Primary(ident) => {
                self.add_position(stmt.start_index);
                self.load_identifier(ident);
            },
            StatementType::Break => {
                self.add_position(stmt.start_index);
                self.bytes.push(Opcode::Break as u8);
            },
            StatementType::Continue => {
                self.add_position(stmt.start_index);
                self.bytes.push(Opcode::Condition as u8);
            },
            StatementType::While(ident, stmts) => {
                self.add_position(stmt.start_index);
                self.bytes.push(Opcode::While as u8);
                self.load_identifier(ident);
                
                let start_index = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }

                self.insert_byte_count(start_index);
            },
            StatementType::Condition(main_chain, else_chain) => {
                self.add_position(stmt.start_index);
                self.bytes.extend_from_slice(&[Opcode::Condition as u8, main_chain.len() as u8]);
                for (chain, stmts) in main_chain {
                    self.load_identifier(chain);
                    let start_index = self.bytes.len();
                    for stmt in stmts {
                        self.parse_byte(stmt.clone());
                    }

                    self.insert_byte_count(start_index);
                }

                if else_chain.is_some() {
                    self.bytes.push(1);
                    let start_index = self.bytes.len();
                    for stmt in else_chain.clone().unwrap() {
                        self.parse_byte(stmt);
                    }

                    self.insert_byte_count(start_index);
                } else {
                    self.bytes.push(0);
                }
            },
            _ => ()
        }
    }

    pub fn load_identifier(&mut self, ident: &Identifier) {
        match ident {
            Identifier::String(register_index) => self.push_constant(Opcode::Str, Opcode::StrLong, *register_index),
            Identifier::Number(register_index) => self.push_constant(Opcode::Num, Opcode::NumLong, *register_index),
            Identifier::Word(register_index) => self.push_constant(Opcode::Word, Opcode::WordLong, *register_index),
            Identifier::Call(ident, params) => {
                self.bytes.push(Opcode::Call as u8);
                self.load_identifier(ident);
                self.bytes.push(params.len() as u8);
                for param in params.iter() { 
                    if param.1 { 
                        self.bytes.push(Opcode::RestParam as u8) 
                    }

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
            Identifier::Ternary(target, truthy, falsy) => {
                self.bytes.push(Opcode::Ternary as u8);
                self.load_identifier(target);
                self.load_identifier(truthy);
                self.load_identifier(falsy);
            },
            Identifier::Array(vec) => {
                self.bytes.push(Opcode::Array as u8);
                self.bytes.extend_from_slice(&(vec.len() as u32).to_le_bytes());
                for i in vec { 
                    self.load_identifier(i) 
                };
            },
            Identifier::Dict(dict) => {
                self.bytes.push(Opcode::Dict as u8);
                self.bytes.extend_from_slice(&(dict.len() as u32).to_le_bytes());
                for i in dict { 
                    self.push_constant_without_op(i.0);
                    self.load_identifier(&i.1);
                }
            },
            Identifier::Func(name_register_id, params, stmts) => {
                self.bytes.push(Opcode::Func as u8);
                self.push_constant_without_op(*name_register_id);
                self.bytes.push(params.len() as u8);
                for param in params {
                    if param.1 { 
                        self.bytes.push(Opcode::RestParam as u8) 
                    }

                    self.push_constant_without_op(param.0);
                };
                
                let start_index = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }
                
                self.insert_byte_count(start_index);
            },
            Identifier::AsyncFunc(name_register_id, params, stmts) => {
                self.bytes.push(Opcode::Func as u8);
                self.bytes.push(1);
                self.push_constant_without_op(*name_register_id);
                self.bytes.push(params.len() as u8);
                for param in params {
                    if param.1 { self.bytes.push(Opcode::RestParam as u8) }
                    self.push_constant_without_op(param.0);
                };
                
                let start_index = self.bytes.len();
                for stmt in stmts {
                    self.parse_byte(stmt.clone());
                }
                
                self.insert_byte_count(start_index);
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
            Identifier::Condition(a, condition_operator, b) => {
                self.bytes.push(Opcode::from(condition_operator) as u8);
                self.load_identifier(a);
                self.load_identifier(b);
            }
        }
    }

    pub fn push_constant(&mut self, short_op: Opcode, long_op: Opcode, constant: u32) {
        if constant < 255 {
            self.bytes.extend_from_slice(&[short_op as u8, constant as u8]);
        } else {
            self.bytes.push(long_op as u8);
            self.bytes.extend_from_slice(&constant.to_le_bytes())
        }
    }

    pub fn push_constant_without_op(&mut self, constant: u32) {
        if constant < 255 {
            self.bytes.extend_from_slice(&[Opcode::Short as u8, constant as u8]);
        } else {
            self.bytes.push(Opcode::Long as u8);
            self.bytes.extend_from_slice(&constant.to_le_bytes())
        }
    }

    pub fn add_position(&mut self, start_index: usize) {
        if let Some(statement) = self.ast.statements.get(self.ci + 1) {
            self.pos_map.push((
                self.bytes.len(), 
                Position::from((start_index, statement.start_index))
            ))
        } else {
            self.pos_map.push((
                self.bytes.len(), 
                Position::from((start_index, self.ast.body.len() - 1))
            ))
        }
    }

    pub fn insert_byte_count(&mut self, start_index: usize) {
        self.bytes.splice(
            start_index..start_index, 
            ((self.bytes.len() - start_index) as u16).to_le_bytes().iter().cloned() 
        );
    }

}