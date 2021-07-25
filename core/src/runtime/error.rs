use std::io::{Error as IoError, ErrorKind as IoErrorKind};
use crate::{Value, TinyString, Vm};

#[derive(Clone, Debug)]
pub struct RuntimeError {
    pub(crate) message: TinyString,
    pub(super) catchable: bool,
    pub(super) ip: usize,
    line: Option<u32>, 
    trace: Option<Vec<TinyString>>
}

impl RuntimeError {

    pub(crate) fn new<M>(vm: &Vm, message: M) -> Self
    where
        M: Into<TinyString>
    {
        let position_ = vm.chunk.get_position(vm.fiber.ip);

        Self {
            message: message.into(),
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(vm.fiber.complete_trace()),
            catchable: true,
            ip: vm.fiber.ip
        }
    }

    pub(crate) fn new_with_ip<M>(vm: &Vm, message: M, ip: usize) -> Self
    where
        M: Into<TinyString>
    {
        let position_ = vm.chunk.get_position(ip);

        Self {
            message: message.into(),
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(vm.fiber.complete_trace()),
            catchable: true,
            ip
        }
    }

    pub(crate) fn new_uncatchable<M>(vm: &Vm, message: M) -> Self
    where
        M: Into<TinyString>
    {
        let position_ = vm.chunk.get_position(vm.fiber.ip);

        Self {
            message: message.into(),
            line: Some(vm.chunk.get_line(position_)),
            trace: Some(vm.fiber.complete_trace()),
            catchable: false,
            ip: vm.fiber.ip
        }
    }

    pub(crate) fn to_value(self, vm: &mut Vm) -> Value {
        Value::String(vm.allocate_value_ptr(self.message))
    }

}

pub type RuntimeResult<T> = Result<T, RuntimeError>;

fn io_error_to_string<'a>(io_error_kind: IoErrorKind) -> &'a str {
    match io_error_kind {
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
    }
}

impl From<IoErrorKind> for TinyString {
    fn from(kind: IoErrorKind) -> Self {
        Self::from(io_error_to_string(kind))
    }
}

impl From<IoError> for TinyString {
    fn from(error: IoError) -> Self {
        Self::from(io_error_to_string(error.kind()))
    }
}