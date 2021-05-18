use dashscript_bytecode::{BinaryOperator, AssignmentOperator};

#[derive(Clone)]
pub enum InstructionValue {
    String(u32),
    Int(u32),
    Float(u32),
    Word(u32),
    Boolean(bool),
    Invert(Box<InstructionValue>),
    Call(Box<InstructionValue>, Vec<(InstructionValue, bool)>),
    Ternary(Box<InstructionValue>, Box<InstructionValue>, Box<InstructionValue>),
    Group(Box<InstructionValue>),
    BinaryOperation(Box<InstructionValue>, BinaryOperator, Box<InstructionValue>),
    Attribute(Box<InstructionValue>, Box<InstructionValue>),
    Array(Vec<InstructionValue>),
    Dict(Vec<(u32, InstructionValue)>),
    Null
}

impl InstructionValue {
    pub fn get_type(&self) -> String {
        match self {
            | Self::String(_) 
            | Self::Int(_) 
            | Self::Float(_) 
            | Self::Boolean(_) 
            | Self::Array(_)
            | Self::Dict(_)
            | Self::Null => "constant",
            Self::Word(_) | Self::Attribute(_, _) => "ident",
            Self::Invert(_) | Self::BinaryOperation(_, _, _) | Self::Ternary(_, _, _) => "operation",
            Self::Group(_) => "group",
            Self::Call(_, _) => "call"
        }.to_string()
    }
}

#[derive(Clone)]
pub enum Instruction {
    Store {
        name: u32,
        value: InstructionValue,
        is_const: bool
    },
    Update {
        target: InstructionValue,
        op: AssignmentOperator,
        value: InstructionValue
    },
    Return(InstructionValue),
    Break,
    Value(InstructionValue)
}