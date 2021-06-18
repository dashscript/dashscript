use std::fmt;

#[derive(Debug, Clone)]
pub enum CompilerErrorKind {
    AssignmentToConstant { name: String },
    TooManyLocals,
    TooManyUpvalues,
    UnknownValue { name: String }
}

impl CompilerErrorKind {
    fn to_string(&self) -> String {
        match self {
            Self::AssignmentToConstant { name } => format!("Assignment to a constant \"{}\".", name),
            Self::TooManyLocals => format!("Found too many locals. Try to save some locals in forms of object and functions into classes."),
            Self::TooManyUpvalues => format!("Found too many upvalues captured in the function. Try to use only few required upvalues."),
            Self::UnknownValue { name } => format!("Value {} does not exists.", name)
        }
    }
}

impl fmt::Display for CompilerErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone)]
pub struct CompilerError {
    pub kind: CompilerErrorKind,
    pub line: usize
}