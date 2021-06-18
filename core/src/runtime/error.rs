use std::string::ToString;
use crate::Value;

#[derive(Clone, Debug)]
pub enum RuntimeErrorKind {
    Panic { value: Value },
    UnknownByte { byte: u8 },
    UnexpectedByte { found: u8, wanted: (u8, u8) },
    InsufficientBytes { needed: u32 },
    TooSmallStack { minimum_len: u32, actual_len: usize },
    CalledAnUncallable { value_type: String },
    ExperimentalUsage { name: String, tracking_issue: Option<u32> },
    AssignmentToConstant { name: String },
    SelfNotFound { name: String },
    CannotAssignToReadonlyProperty
}

impl ToString for RuntimeErrorKind {
    fn to_string(&self) -> String {
        match self {
            Self::Panic { value } => value.to_string(),
            Self::UnknownByte { byte } => format!("Found an unknown byte: {}", byte),
            Self::UnexpectedByte { found, wanted: (start, end) } => {
                format!("Found unexpected byte {} but expected a byte in the range of ({}..{}).", found, start, end)
            },
            Self::InsufficientBytes { needed } => {
                let byte_type = match needed {
                    1 => "u8",
                    2 => "u16",
                    4 => "u32",
                    8 => "u64",
                    _ => return format!("Expected {} more bytes.", needed)
                };

                format!("Expected {} more bytes to form a {}.", needed, byte_type)
            },
            Self::TooSmallStack { minimum_len, actual_len } => {
                format!("Stack had a length of {} but expected the stack minimum length as {}", actual_len, minimum_len)
            },
            Self::CalledAnUncallable { value_type } => format!("You cannot call of type {}.", value_type),
            Self::ExperimentalUsage { name, tracking_issue } => {
                if let Some(issue_number) = tracking_issue {
                    format!("{} is still experimental. View #{} (https://github.com/dashscript/dashscript/issues/{}).", name, issue_number, issue_number)
                } else {
                    format!("{} is still experimental.", name)
                }
            },
            Self::AssignmentToConstant { name } => format!("Assignment to a constant {}.", name),
            Self::SelfNotFound { name } => format!("Could not find self for instance function {}.", name),
            Self::CannotAssignToReadonlyProperty => "Cannot assign to readonly properties.".to_string()
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    pub(crate) kind: RuntimeErrorKind, // The error kind
    position: Option<(usize, usize)>, // (line, colume)
    trace: Option<Vec<String>>, // The trace address
}

impl RuntimeError {
    pub(crate) fn new_untraced(kind: RuntimeErrorKind) -> Self {
        Self { 
            kind,
            position: None,
            trace: None
        }
    }
}

impl From<Value> for RuntimeError {
    fn from(value: Value) -> Self {
        Self {
            kind: RuntimeErrorKind::Panic { value },
            position: None,
            trace: None
        }
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;