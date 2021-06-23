use std::string::ToString;
use std::io::{Error as IoError, ErrorKind as IoErrorKind};
use crate::{Value, TinyString, Vm};

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
    CannotAssignToReadonlyProperty,
    CallingAObjectWithoutProperPrototype,
    ExtendingObjectToAObjectWithoutProperPrototype
}

impl From<Value> for RuntimeErrorKind {
    fn from(value: Value) -> Self {
        Self::Panic { value }
    }
}

impl From<(u32, usize)> for RuntimeErrorKind {
    fn from(tuple: (u32, usize)) -> Self {
        Self::TooSmallStack {
            minimum_len: tuple.0,
            actual_len: tuple.1
        }
    }
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
            Self::CannotAssignToReadonlyProperty => "Cannot assign to readonly properties.".to_string(),
            Self::CallingAObjectWithoutProperPrototype => "Cannot call a dict which has no prototype.".to_string(),
            Self::ExtendingObjectToAObjectWithoutProperPrototype => "You cannot extend a object to a object which has no good prototype.".to_string()
        }
    }
}

#[derive(Clone, Debug)]
pub struct RuntimeError {
    pub(crate) kind: RuntimeErrorKind,
    line: Option<u32>, 
    trace: Option<Vec<TinyString>>
}

impl RuntimeError {
    pub(crate) fn new_untraced(kind: RuntimeErrorKind) -> Self {
        Self { 
            kind,
            line: None,
            trace: None
        }
    }

    pub(crate) fn new<K: Into<RuntimeErrorKind>>(vm: &Vm, kind: K) -> Self {
        let position_ = vm.chunk.get_position(vm.ip);
        let mut trace_ = Vec::new();

        for frame in vm.call_stack.iter() {
            trace_.push(frame.name());
        }

        Self {
            kind: kind.into(),
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(trace_)
        }
    }

    pub(crate) fn new_io(vm: &mut Vm, error: IoError) -> Self {
        let position_ = vm.chunk.get_position(vm.ip);
        let mut trace_ = Vec::new();

        for frame in vm.call_stack.iter() {
            trace_.push(frame.name());
        }

        Self {
            kind: RuntimeErrorKind::Panic { value: Value::from_io_error(vm, error) },
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(trace_)
        }
    }

    pub(crate) fn new_str(vm: &mut Vm, message: &str) -> Self {
        let position_ = vm.chunk.get_position(vm.ip);
        let mut trace_ = Vec::new();

        for frame in vm.call_stack.iter() {
            trace_.push(frame.name());
        }

        Self {
            kind: RuntimeErrorKind::from(Value::String(vm.allocate_static_str(message))),
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(trace_)
        }
    }

    pub(crate) fn new_kind(vm: &mut Vm, kind: RuntimeErrorKind) -> Self {
        let position_ = vm.chunk.get_position(vm.ip);
        let mut trace_ = Vec::new();

        for frame in vm.call_stack.iter() {
            trace_.push(frame.name());
        }

        Self {
            kind,
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(trace_)
        }
    }
}

impl From<Value> for RuntimeError {
    fn from(value: Value) -> Self {
        Self {
            kind: RuntimeErrorKind::Panic { value },
            line: None,
            trace: None
        }
    }
}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

impl Value {
    pub fn from_io_error(vm: &mut Vm, error: IoError) -> Value {
        let static_str = match error.kind() {
            IoErrorKind::NotFound => "Not found.",
            IoErrorKind::PermissionDenied => "Permission denied for access.",
            IoErrorKind::ConnectionRefused => "Connection was refused.",
            IoErrorKind::ConnectionReset => "Connection was reset.",
            IoErrorKind::ConnectionAborted => "Connection was aborted",
            IoErrorKind::NotConnected => "Connection was not connected yet.",
            IoErrorKind::AddrInUse => "The address provied was already in use.",
            IoErrorKind::AddrNotAvailable => "The provided address is not available.",
            IoErrorKind::BrokenPipe => "Broken pipe.",
            IoErrorKind::AlreadyExists => "Already exists.",
            IoErrorKind::WouldBlock => "The operation needs to block to complete, but the blocking operation was requested to not occur.",
            IoErrorKind::InvalidInput => "Something invalid was provided to process.",
            IoErrorKind::InvalidData => "Data not valid for the operation were encountered.",
            IoErrorKind::TimedOut => "The I/O operation's timeout expired, causing it to be canceled.",
            IoErrorKind::WriteZero => "Found result as \"Ok(0)\" in \"write\" (Internals).",
            IoErrorKind::Interrupted => "The operation was interrupted.",
            IoErrorKind::UnexpectedEof => "An error returned when an operation could not be completed because an \"end of file\" was reached prematurely.",
            _ => "Untracable error."
        };

        Value::String(vm.allocate_static_str(static_str))
    }
}