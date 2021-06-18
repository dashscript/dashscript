#[derive(Debug, Clone, Copy)]
pub enum BinOp {
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
    BitAnd,
    BitOr,
    BitXor,
    Shr,
    Shl
}

#[derive(Debug, Clone, Copy)]
pub enum AssignOp {
    Add,
    Sub,
    Assign
}

#[repr(u8)]
#[derive(Debug, Clone)]
pub enum Expr {
    // Expressions
    String(u32), // (constant_register_id)
    Int(u32), // (constant_register_id)
    Float(u32), // (constant_register_id)
    Word(u32), // (constant_register_id)
    Boolean(bool), // (boolean)
    Ternary(Box<Expr>, Box<Expr>, Box<Expr>), // (target_expr, truthy_expr, falsy_expr)
    Attribute(Box<Expr>, Box<Expr>), // (target_expr, attr_expr)
    Call(Box<Expr>, Vec<Expr>), // (target_expr, parameters)
    Array(Vec<Expr>), // [value_expr]
    Dict(Vec<(u32, Expr)>), // [(constant_register_id, value_expr)]
    Group(Box<Expr>), // (grouped_expr)
    Not(Box<Expr>), // (expr_to_invert)
    Await(Box<Expr>), // (await_expr)
    Function {
        name: u32, // The function name constant register id
        parameters: Vec<u32>, // The parameters name registers
        inner: Vec<Stmt>, // The body of the function
        is_async: bool // Boolean stating is the function async or not
    },
    BinaryOperation {
        lhs: Box<Expr>, // Lhs expression
        rhs: Box<Expr>, // Rhs expression
        op: BinOp // The operator
    },
    Null,

    // Statements
    While(Box<Expr>, Vec<Stmt>), // (condition, [statement_to_execute])
    Return(Box<Expr>), // (returned_identifier)
    Store(u32, Box<Expr>, bool), // (constant_register_id, assigned_identifier, is_constant)
    Import {
        module: u32, // The name of the module
        as_: Option<u32> // The as name of the module
    },
    Try {
        try_inner: Vec<Stmt>, // The code to try 
        expect_inner: Vec<Stmt> // The code to run if the code fails
    },
    If {
        branches: Vec<(Expr, Vec<Stmt>)>, // The branches of the if elif
        else_branch: Option<Vec<Stmt>> // The final else branch if exists
    },
    Assign {
        target: Box<Expr>, // The target to assing
        op: AssignOp, // The operator used to assign
        value: Box<Expr> // The value which is assigned
    },
    For {
        name: u32,
        in_: Box<Expr>,
        inner: Vec<Stmt>
    },
    Break,
    Continue
}

impl Default for Expr {
    fn default() -> Self { Expr::Null }
}

#[derive(Default, Debug, Clone)]
pub struct Stmt {
    pub expr: Expr,
    pub index: usize
}