use dashscript_lexer::{Position, AssignmentOperator};

#[derive(Debug, Clone)]
pub enum Identifier {
    String(u32), // (constant_register_id)
    Number(u32), // (constant_register_id)
    Word(u32), // (constant_register_id)
    Attribute(Box<Identifier>, Box<Identifier>), // (target_identifier, key_identifier)
    Call(Box<Identifier>, Vec<(Identifier, bool)>), // (target_identifier, [(param_identifier, is_rest_parameter)])
    Add(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Subtract(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Multiply(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Divide(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Pow(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Ternary(Box<Identifier>, Box<Identifier>, Box<Identifier>), // (target_identifier, thruthy_identifier, faly_identifier)
    Boolean(bool), // (boolean)
    Array(Vec<Identifier>), // [value_identifier]
    Dict(Vec<(u32, Identifier)>), // [(constant_register_id, value_identifier)]
    Group(Box<Identifier>), // (grouped_identifier)
    Func(u32, Vec<FuncParam>, Vec<Statement>), // (constant_register_id, [function_param], [statement])
    AsyncFunc(u32, Vec<FuncParam>, Vec<Statement>), // (constant_register_id, [function_param], [statement])
    Or(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    And(Box<Identifier>, Box<Identifier>), // (lhs_identifier, rhs_identifier)
    Invert(Box<Identifier>), // (indentifier_to_invert)
    Condition(Box<Identifier>, Condition, Box<Identifier>), // (lhs_identifier, condition, rhs_identifier)
    Await(Box<Identifier>), // (target_identifier)
    In(Box<Identifier>, Box<Identifier>),
    Null
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
    // (constant_register_id, assigned_identifier, is_constant)
    Var(u32, Identifier, bool),
    Assign(Identifier, AssignmentOperator, Identifier),
    Return(Identifier),
    Primary(Identifier),
    Condition(Vec<(Identifier, Vec<Statement>)>, Option<Vec<Statement>>),
    While(Identifier, Vec<Statement>),
    In(String, Identifier),
    For(ForStmt),
    Break,
    Continue,
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

#[derive(Debug, Clone)]
pub enum Condition {
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

impl Default for StatementType {
    fn default() -> Self { Self::Primary(Identifier::Null) }
}

pub type ClassProp = (String, Identifier, bool);
pub type ClassMethod = (String, Vec<FuncParam>, Vec<Statement>, bool, bool);
pub type FuncParam = (String, bool); // (name, is_rest_parameter)