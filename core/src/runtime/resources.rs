use std::rc::Rc;
use std::any::{Any, TypeId};
use std::collections::BTreeMap;
use std::io::{ErrorKind, Write, Read};
use std::process::{Child, ChildStdin, ChildStdout, ChildStderr};

#[derive(Default, Clone)]
pub struct ResourceTable {
    resource_table: BTreeMap<u32, Rc<dyn Resource>>,
    next_rid: u32
}

impl ResourceTable {

    pub fn get<T: Resource>(&self, resource_id: u32) -> Option<Rc<T>> {
        self.resource_table.get(&resource_id)
            .and_then(|rc| unsafe {
                if rc.type_id() == TypeId::of::<T>() {
                    Some((*(rc as *const Rc<_> as *const Rc<T>)).clone())
                } else { None }
            })
    }

    pub fn get_io(&self, resource_id: u32) -> Option<Rc<dyn IoResource>> {
        self.resource_table.get(&resource_id)
            .and_then(|rc| unsafe {
                if rc.kind() == ResourceKind::Io {
                    Some((*(rc as *const Rc<_> as *const Rc<dyn IoResource>)).clone())
                } else { None }
            })
    }

    pub fn add<T: Resource>(&mut self, resource: T) -> u32 {
        let rid = self.next_rid;
        assert!(self.resource_table.insert(self.next_rid, Rc::new(resource)).is_none());
        self.next_rid += 1;
        rid
    }

    pub fn remove(&mut self, rid: u32) -> Option<Rc<dyn Resource>> {
        self.resource_table.remove(&rid)
    }

}

pub type ResourceError<T = ()> = Result<T, ErrorKind>;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ResourceKind {
    Io,
    Child,
    Bool,
    None
}

pub trait Resource: Any + 'static {
    fn kind(&self) -> ResourceKind;
    fn close(&self) -> ResourceError { 
        Ok(()) 
    }
}

pub trait IoResource: Resource + Any + 'static {
    fn read(&self, _buf: &mut [u8]) -> ResourceError<usize> { 
        Err(ErrorKind::Interrupted)
    }

    fn write(&self, _buf: &[u8]) -> ResourceError<usize> {
        Err(ErrorKind::Interrupted)
    }

    fn flush(&self) -> ResourceError {
        Err(ErrorKind::Interrupted)
    }
}

pub struct ChildResource(pub Box<Child>);
pub struct ChildStdinResource(pub Box<ChildStdin>);
pub struct ChildStdoutResource(pub Box<ChildStdout>);
pub struct ChildStderrResource(pub Box<ChildStderr>);

#[derive(Copy, Clone)]
pub struct BoolResource(pub bool);

impl Resource for ChildResource {
    fn kind(&self) -> ResourceKind {
        ResourceKind::Child
    }

    fn close(&self) -> ResourceError {
        match unwrap_ref_as_mut(self.0.as_ref()).kill() {
            Ok(_) => Ok(()),
            Err(error) => Err(error.kind())
        }
    }
}

impl Resource for ChildStdinResource {
    fn kind(&self) -> ResourceKind {
        ResourceKind::Io
    }
}

impl Resource for ChildStdoutResource {
    fn kind(&self) -> ResourceKind {
        ResourceKind::Io
    }
}

impl Resource for ChildStderrResource {
    fn kind(&self) -> ResourceKind {
        ResourceKind::Io
    }
}

impl Resource for BoolResource {
    fn close(&self) -> ResourceError {
        unsafe { std::ptr::write(self as *const BoolResource as *mut bool, true) }
        Ok(())
    }

    fn kind(&self) -> ResourceKind {
        ResourceKind::Bool
    }
}

impl IoResource for ChildStdinResource {
    fn write(&self, buf: &[u8]) -> ResourceError<usize> {
        match unwrap_ref_as_mut(self.0.as_ref()).write(buf) {
            Ok(n) => Ok(n),
            Err(error) => Err(error.kind())
        }
    }

    fn flush(&self) -> ResourceError {
        match unwrap_ref_as_mut(self.0.as_ref()).flush() {
            Ok(_) => Ok(()),
            Err(error) => Err(error.kind())
        }
    }
}

impl IoResource for ChildStdoutResource {
    fn read(&self, buf: &mut [u8]) -> ResourceError<usize> {
        match unwrap_ref_as_mut(self.0.as_ref()).read(buf) {
            Ok(n) => Ok(n),
            Err(error) => Err(error.kind())
        }
    }
}

impl IoResource for ChildStderrResource {
    fn read(&self, buf: &mut [u8]) -> ResourceError<usize> {
        match unwrap_ref_as_mut(self.0.as_ref()).read(buf) {
            Ok(n) => Ok(n),
            Err(error) => Err(error.kind())
        }
    }
}

fn unwrap_ref_as_mut<'a, T>(ref_: &T) -> &'a mut T {
    unsafe { &mut *(ref_ as *const T as *mut T) }
}