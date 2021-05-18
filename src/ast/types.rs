use dashscript_lexer::AssignmentOperator;

#[repr(u8)]
#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Power,
    Rem,
    Or,
    And,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    Equal,
    NotEqual,
}

impl BinaryOperator {
    pub const VARIANT_SIZE: u8 = BinaryOperator::NotEqual as u8;
}

#[derive(Debug, Clone)]
pub enum Identifier {
    String(u32), // (constant_register_id)
    Int(u32), // (constant_register_id)
    Float(u32), // (constant_register_id)
    Word(u32), // (constant_register_id)
    Attribute(Box<Identifier>, Box<Identifier>), // (target_identifier, key_identifier)
    Call(Box<Identifier>, Vec<(Identifier, bool)>), // (target_identifier, [(param_identifier, is_rest_parameter)])
    BinaryOperation(Box<Identifier>, BinaryOperator, Box<Identifier>), // (lhs_identifier, binary_operator, rhs_identifier)
    Ternary(Box<Identifier>, Box<Identifier>, Box<Identifier>), // (target_identifier, thruthy_identifier, faly_identifier)
    Boolean(bool), // (boolean)
    Array(Vec<Identifier>), // [value_identifier]
    Dict(Vec<(u32, Identifier)>), // [(constant_register_id, value_identifier)]
    Group(Box<Identifier>), // (grouped_identifier)
    Func(u32, Vec<FuncParam>, Vec<Statement>), // (constant_register_id, [function_param], [statement])
    AsyncFunc(u32, Vec<FuncParam>, Vec<Statement>), // (constant_register_id, [function_param], [statement])
    Invert(Box<Identifier>), // (indentifier_to_invert)
    Await(Box<Identifier>), // (target_identifier)
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
    Var(u32, Identifier, bool), // (constant_register_id, assigned_identifier, is_constant)
    Assign(Identifier, AssignmentOperator, Identifier), // (target_identifier, operator, assigned_identifier)
    Return(Identifier), // (returned_identifier)
    Primary(Identifier), // (primiary_identifier)
    Condition(Vec<(Identifier, Vec<Statement>)>, Option<Vec<Statement>>), // ([(condition, [statement_to_execute])], Option([statement_to_execute]))
    While(Identifier, Vec<Statement>), // (condition, [statement_to_execute])
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
    pub start_index: usize
}

impl Default for StatementType {
    fn default() -> Self { Self::Primary(Identifier::Null) }
}

pub type ClassProp = (String, Identifier, bool);
pub type ClassMethod = (String, Vec<FuncParam>, Vec<Statement>, bool, bool);
pub type FuncParam = (u32, bool); // (name, is_rest_parameter)