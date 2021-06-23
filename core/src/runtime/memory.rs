extern crate alloc;

use alloc::alloc::Layout;
use std::marker::PhantomData;
use std::{ptr, mem};
use crate::{Value, Map, ValueIter, TinyString};
use super::object::{self, ObjectKind};

pub(crate) const USIZE_SIZE: usize = mem::size_of::<usize>();
pub(crate) const USIZE_ALIGN: usize = mem::align_of::<usize>();

pub const fn next_alignment(size: usize, align: usize) -> usize {
    let rem = size % align;
    if rem == 0 { size } else { size + (align - rem) }
}

pub const fn max_align_of<T>() -> usize {
    let t_align = mem::align_of::<T>();
    if t_align >= GcHeader::ALIGN { t_align } else { GcHeader::ALIGN }
}

pub const fn get_layout_size<T>() -> usize {
    next_alignment(GcHeader::SIZE, mem::align_of::<T>()) + mem::size_of::<T>()
}

pub fn create_layout<T>() -> Layout {
    let t_align = mem::align_of::<T>();
    let size = next_alignment(GcHeader::SIZE, t_align) + mem::size_of::<T>();
    Layout::from_size_align(size, if t_align >= GcHeader::ALIGN { t_align } else { GcHeader::ALIGN }).unwrap()
}

pub fn create_gc_layout<T>() -> GcLayout {
    let t_align = mem::align_of::<T>();
    let size = next_alignment(GcHeader::SIZE, t_align) + mem::size_of::<T>();
    let rem = GcHeader::SIZE % t_align;
    GcLayout {
        layout: Layout::from_size_align(size, if t_align >= GcHeader::ALIGN { t_align } else { GcHeader::ALIGN }).unwrap(),
        offset: if rem == 0 { GcHeader::SIZE } else { GcHeader::SIZE + (t_align - rem) },
        size
    }
}

// A layout representation sent by the layout creating functions
// above to make data collection easier.
#[derive(Debug, Clone)]
pub struct GcLayout {
    pub layout: Layout,
    pub offset: usize,
    pub size: usize
}

// An header stored before the actual value which contains the
// the marked state of the value with the type info
// TODO(Scientific-Guy): Make a way to use values without a header
#[derive(Debug, Clone, Default, Copy)]
pub struct GcHeader(pub(crate) bool);

impl GcHeader {

    pub const SIZE: usize = mem::size_of::<Self>();
    pub const ALIGN: usize = mem::size_of::<Self>();
    
    pub unsafe fn unwrap_<T: Clone>(pointer: *const u8) -> T {
        let pointer = pointer.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
        (*(pointer as *const T)).clone()
    }

    pub unsafe fn unwrap_ref_<'a, T>(pointer: *const u8) -> &'a T {
        let pointer = pointer.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
        & *(pointer as *const T)
    }

    pub unsafe fn unwrap_mut_<'a, T>(pointer: *const u8) -> &'a mut T {
        let pointer = pointer.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
        &mut *(pointer as *mut T)
    }

    pub unsafe fn mark(pointer: *const u8) {
        if pointer.is_null() {
            panic!("Could not mark the pointer while sweeping garbage.");
        }

        ptr::write(pointer as *mut bool, true);
    }

}

macro_rules! deallocate {
    // Special deallocation for ValueIter
    ($ptr:expr, ValueIter) => {{
        let offset = next_alignment(GcHeader::SIZE, mem::align_of::<ValueIter>());
        (*($ptr.add(offset) as *mut ValueIter)).dealloc();
        alloc::alloc::dealloc($ptr as _, create_layout::<ValueIter>());
    }};

    ($ptr:expr, $type:ty) => {{
        let offset = next_alignment(GcHeader::SIZE, mem::align_of::<$type>());
        ptr::drop_in_place($ptr.add(offset) as *mut $type);
        alloc::alloc::dealloc($ptr as _, create_layout::<$type>());
    }};
}

// An handler to handle the pointer.
#[derive(Debug, Clone)]
pub struct GcHandle(pub(crate) *const GcHeader, pub(crate) ObjectKind);

impl GcHandle {

    pub fn unwrap_header(&self) -> &GcHeader {
        // GcHeader is required for a value in GcHandle
        if self.0.is_null() {
            panic!("Ptr {} is null.", self.0 as usize);
        }

        unsafe { &*(self.0 as *const GcHeader) }
    }

    pub fn marked(&self) -> bool {
        self.unwrap_header().0
    }

    // This might be unsafe if you deallocated the handler at the wrong time
    pub unsafe fn dealloc(&self) {
        if self.0.is_null() {
            panic!("Ptr {} is null.", self.0 as usize);
        }
        
        match self.1 {
            ObjectKind::NativeFunction => deallocate!(self.0, object::NativeFunction),
            ObjectKind::Array => deallocate!(self.0, Vec<Value>),
            ObjectKind::Map => deallocate!(self.0, Map),
            ObjectKind::Function => deallocate!(self.0, object::Function),
            ObjectKind::Iterator => deallocate!(self.0, ValueIter),
            ObjectKind::String => deallocate!(self.0, TinyString)
        }
    }

    pub unsafe fn dealloc_if_unreachable(&self) -> bool {
        if self.0.is_null() {
            panic!("Ptr {} is null.", self.0 as usize);
        }

        let pointer = self.0 as *const GcHeader;

        if !(*pointer).0 {
            match self.1 {
                ObjectKind::NativeFunction => deallocate!(self.0, object::NativeFunction),
                ObjectKind::Array => deallocate!(self.0, Vec<Value>),
                ObjectKind::Map => deallocate!(self.0, Map),
                ObjectKind::Function => deallocate!(self.0, object::Function),
                ObjectKind::Iterator => deallocate!(self.0, ValueIter),
                ObjectKind::String => deallocate!(self.0, TinyString)
            };

            true
        } else {
            ptr::write(pointer as *mut bool, false);
            false
        }
    }

}

#[derive(Debug, Hash)]
pub struct ValuePtr<T>(pub(crate) *const u8, PhantomData<T>);

impl<T> ValuePtr<T> {

    pub fn new(ptr: *const u8) -> Self {
        if ptr.is_null() {
            panic!("Pointer {:?} provided for [`ValuePtr::new`] failed.", ptr);
        }

        Self(ptr, PhantomData)
    }

    pub(crate) fn new_unchecked(ptr: *const u8) -> Self {
        Self(ptr, PhantomData)
    }

    pub fn as_ptr(&self) -> *const u8 {
        self.0
    }

    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.0 as *mut u8
    }

    pub fn unwrap(&self) -> T
    where
        T: Clone
    {
        unsafe {
            let pointer = self.0.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
            (*(pointer as *const T)).clone()
        }
    }

    pub fn unwrap_ref<'a>(&self) -> &'a T {
        unsafe {
            let pointer = self.0.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
            & *(pointer as *const T)
        }
    }

    pub fn unwrap_mut<'a>(&self) -> &'a mut T {
        unsafe {
            let pointer = self.0.add(next_alignment(GcHeader::SIZE, mem::align_of::<T>()));
            &mut *(pointer as *mut T)
        }
    }

}

impl ValuePtr<TinyString> {
    pub fn unwrap_bytes(&self) -> &[u8] {
        self.unwrap_ref().as_bytes()
    }
}

impl<T> Clone for ValuePtr<T> {
    fn clone(&self) -> Self {
        Self(self.0, PhantomData)
    }
}

impl<T> Copy for ValuePtr<T> {}

impl<T> PartialEq for ValuePtr<T> {
    fn eq(&self, other: &Self) -> bool {
        self.0 == other.0
    }
}

impl<T> Eq for ValuePtr<T> {}