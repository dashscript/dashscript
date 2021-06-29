extern crate alloc;

use alloc::alloc::Layout;
use std::hash::{Hash, Hasher};
use std::ptr::{self, NonNull};
use std::ops::{Deref, DerefMut, Add};
use std::fmt::{self, Display, Formatter, Debug};
use std::slice::Iter;
use crate::runtime::memory::{USIZE_SIZE, USIZE_ALIGN};

pub struct TinyString(NonNull<u8>);

impl TinyString {

    pub fn new(bytes: &[u8]) -> Self {
        unsafe {
            let length = bytes.len();
            let str_ptr = alloc::alloc::alloc(Layout::from_size_align(USIZE_SIZE + length, USIZE_ALIGN).unwrap());
            let non_null = NonNull::new(str_ptr).expect("Could not allocate a tiny string.");

            ptr::write(str_ptr as *mut usize, length);
            ptr::copy_nonoverlapping(bytes.as_ptr(), str_ptr.add(USIZE_SIZE), length);

            Self(non_null)
        }
    }

    pub fn len(&self) -> usize {
        unsafe { *(self.0.as_ptr() as *const usize) }
    }

    pub fn to_bytes(&self) -> &[u8] {
        let ptr = self.0.as_ptr();
        unsafe {
            std::slice::from_raw_parts(ptr.add(USIZE_SIZE), *(ptr as *const usize))
        }
    }

    pub fn to_string(&self) -> String {
        (**self).to_string()
    }

    // Unsafe because the bytes of the string gets deallocates when the actual
    // TinyString gets dropped which makes this unsafe clone's pointer be null.
    pub unsafe fn ptr_clone(&self) -> Self { 
        Self(NonNull::new_unchecked(self.0.as_ptr())) 
    }

    pub fn as_ptr(&self) -> *const u8 {
        let ptr = self.0.as_ptr();
        if ptr.is_null() {
            panic!("Requested a pointer for the TinyString which is null.")
        }

        ptr
    }

    pub fn is_null(&self) -> bool {
        self.0.as_ptr().is_null()
    }

    // Unsafe because the ptr requested needs not to be null whereas this function
    // returns the pointer directly without checking the pointer for null.
    pub unsafe fn as_ptr_unchecked(&self) -> *const u8 {
        self.0.as_ptr()
    }

    pub fn chars(&self) -> TinyChars<Iter<u8>> {
        TinyChars { bytes: self.to_bytes().iter() }
    }

    pub fn char_code_at(&self, index: usize) -> Option<u32> {
        let mut chars = self.chars();
        let mut i = 0;

        while let Some(code) = chars.next() {
            if i == index {
                return Some(code);
            }

            i += 1;
        }

        None
    } 

}

impl Deref for TinyString {
    type Target = str;
    fn deref(&self) -> &str {
        unsafe {
            let ptr = self.0.as_ptr();
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr.add(USIZE_SIZE), *(ptr as *const usize)))
        }
    }
}

impl DerefMut for TinyString {
    fn deref_mut(&mut self) -> &mut str {
        unsafe {
            let ptr = self.0.as_ptr();
            std::str::from_utf8_unchecked_mut(std::slice::from_raw_parts_mut(ptr.add(USIZE_SIZE), *(ptr as *const usize)))
        }
    }
}

impl Display for TinyString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", &**self)
    }
}

impl Debug for TinyString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", &**self)
    }
}

impl Drop for TinyString {
    fn drop(&mut self) {
        unsafe {
            let str_ptr = self.0.as_ptr();
            if str_ptr.is_null() {
                panic!("Failed dropping a TinyString.");
            }

            let len = *(str_ptr as *const usize);
            
            // Deallocate the length and the bytes
            ptr::drop_in_place(self.deref_mut());
            alloc::alloc::dealloc(str_ptr as _, Layout::from_size_align(USIZE_SIZE + len, USIZE_ALIGN).unwrap());
        }
    }
}

impl Clone for TinyString {
    fn clone(&self) -> Self {
        unsafe {
            let old_ptr = self.0.as_ptr();
            let length = *(old_ptr as *const usize);
            let str_ptr = alloc::alloc::alloc(Layout::from_size_align(USIZE_SIZE + length, USIZE_ALIGN).unwrap());
            let non_null = NonNull::new(str_ptr).expect("Could not allocate a tiny string.");

            ptr::write(str_ptr as *mut usize, length);
            ptr::copy_nonoverlapping(old_ptr.add(USIZE_SIZE), str_ptr.add(USIZE_SIZE), length);

            Self(non_null)
        }
    }
}

impl Add for TinyString {
    type Output = Self;

    // A new tiny string gets created here instead of updating the
    // original one.
    fn add(self, rhs: Self) -> Self {
        unsafe {
            let bytes = [self.to_bytes(), rhs.to_bytes()].concat();
            let length = bytes.len();
            let str_ptr = alloc::alloc::alloc(Layout::from_size_align(USIZE_SIZE + length, USIZE_ALIGN).unwrap());
            let non_null = NonNull::new(str_ptr).expect("Could not allocate a tiny string.");

            ptr::write(str_ptr as *mut usize, length);
            ptr::copy_nonoverlapping(bytes.as_ptr(), str_ptr.add(USIZE_SIZE), length);

            Self(non_null)
        }
    }
}

impl PartialEq for TinyString {
    fn eq(&self, other: &Self) -> bool {
        self.to_bytes() == other.to_bytes()
    }
}

impl Eq for TinyString {}

impl Default for TinyString {
    fn default() -> Self {
        Self::new(&[])
    }
}

impl Hash for TinyString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.to_bytes().hash(state);
    }
}

impl From<&str> for TinyString {
    fn from(string: &str) -> Self {
        Self::new(string.as_bytes())
    }
}

impl From<String> for TinyString {
    fn from(string: String) -> TinyString {
        TinyString::new(string.as_bytes())
    }
}

const CONT_MASK: u8 = 0b0011_1111;

fn utf8_acc_cont_byte(ch: u32, byte: u8) -> u32 {
    (ch << 6) | (byte & CONT_MASK) as u32
}

pub struct TinyChars<'a, I: Iterator<Item = &'a u8>> {
    bytes:  I
}

impl<'a, I: Iterator<Item = &'a u8>> TinyChars<'a, I> {

    pub fn next_or_default(&mut self) -> u8 {
        match self.bytes.next() {
            Some(byte) => *byte,
            None => 0
        }
    }

}

impl<'a, I: Iterator<Item = &'a u8>> Iterator for TinyChars<'a, I> {
    type Item = u32;

    fn next(&mut self) -> Option<u32> {
        let x = *self.bytes.next()?;
        if x < 128 {
            return Some(x as u32);
        }

        let init = (x & (0x7F >> 2)) as u32;
        let y = self.next_or_default();
        let mut ch = utf8_acc_cont_byte(init, y);

        if x >= 0xE0 {
            let z = self.next_or_default();
            let y_z = utf8_acc_cont_byte((y & CONT_MASK) as u32, z);
            ch = init << 12 | y_z;

            if x >= 0xF0 {
                let w = self.next_or_default();
                ch = (init & 7) << 18 | utf8_acc_cont_byte(y_z, w);
            }
        }

        Some(ch)
    }
}