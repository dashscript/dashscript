use crate::common::fsize;
use crate::lexer::parser::{
    Position,
    AssignmentOp
};

#[derive(Debug, Clone)]
pub enum Identifier {
    String(String),
    Number(fsize),
    Word(String),
    Attribute(Box<Identifier>, Box<Identifier>),
    Call(Box<Identifier>, Vec<Identifier>),
    Add(Box<Identifier>, Box<Identifier>),
    Subtract(Box<Identifier>, Box<Identifier>),
    Multiply(Box<Identifier>, Box<Identifier>),
    Divide(Box<Identifier>, Box<Identifier>),
    Pow(Box<Identifier>, Box<Identifier>),
    Ternary(Box<Identifier>, Box<Identifier>, Box<Identifier>),
    Boolean(bool),
    Array(Vec<Identifier>),
    Dict(Vec<(String, Identifier)>),
    Group(Box<Identifier>),
    Func(String, Vec<FuncParam>, Vec<Statement>),
    AsyncFunc(String, Vec<FuncParam>, Vec<Statement>),
    Or(Box<Identifier>, Box<Identifier>),
    And(Box<Identifier>, Box<Identifier>),
    Invert(Box<Identifier>),
    Condition(Box<Identifier>, String, Box<Identifier>),
    Await(Box<Identifier>),
    In(Box<Identifier>, Box<Identifier>),
    Null
}

impl Identifier {
    
    pub fn is_null(&self) -> bool {
        match self {
            Identifier::Null => true,
            _ => false
        }
    }

}

impl Default for Identifier {
    fn default() -> Self { Identifier::Null }
}

#[derive(Debug, Clone)]
pub enum ForStmt {
    In(String, Identifier, Vec<Statement>),
    Stmts(Vec<Statement>, Vec<Statement>)
}

#[derive(Debug, Clone)]
pub enum StatementType {
    Var(String, Identifier, bool),
    Assign(Identifier, AssignmentOp, Identifier),
    Return(Identifier),
    Primary(Identifier),
    Condition(Vec<(Identifier, Vec<Statement>)>, Option<Vec<Statement>>),
    While(Identifier, Vec<Statement>),
    In(String, Identifier),
    For(ForStmt),
    Break,
    Continue,
    None,
    // TODO(Scientific-Guy): Make a better class object.
    Class {
        name: String,
        extends: Option<String>,
        props: Vec<ClassProp>,
        methods: Vec<ClassMethod>
    }
}

#[derive(Default, Debug, Clone)]
pub struct Statement {
    pub val: StatementType,
    pub pos: Position
}

impl std::default::Default for StatementType {
    fn default() -> StatementType {
        StatementType::None
    }
}

pub type ClassProp = (String, Identifier, bool);
pub type ClassMethod = (String, Vec<FuncParam>, Vec<Statement>, bool, bool);
pub type FuncParam = (String, bool);