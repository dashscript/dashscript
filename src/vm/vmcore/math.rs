use crate::vm::value::Value;
use crate::vm::vm::VM;
use crate::common::fsize;
use super::date::get_current_time;
use super::builtin::panic;

// Xorshift 128 based random number generator
pub struct Rng {
    x: u32,
    y: u32,
    z: u32,
    w: u32
}

impl Rng {

    const X: u32 = 123456789;
    const Y: u32 = 362436069;
    const Z: u32 = 521288629;
    const W: u32 = 88675123;

    pub fn new() -> Self {
        let seed = get_current_time().as_millis() as u32;
        Self {
            x: Rng::X ^ seed,
            y: Rng::Y ^ seed,
            z: Rng::Z,
            w: Rng::W
        }
    }

    pub fn random(&mut self) -> u32 {
        let t = self.x ^ self.x.wrapping_shl(11);
        self.x = self.y; 
        self.y = self.z; 
        self.z = self.w;
        self.w ^= self.w.wrapping_shr(19) ^ t ^ t.wrapping_shr(8);
        self.w
    }

}

fn arg_to_float(arg: Option<&Value>) -> fsize {
    match arg {
        Some(Value::Num(num)) => *num,
        _ => 0.0
    }
}

pub fn floor_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).floor())
}

pub fn trunc_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).trunc())
}

pub fn ceil_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).ceil())
}

pub fn round_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).round())
}

pub fn abs_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).abs())
}

pub fn sqrt_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).sqrt())
}

pub fn sin_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).sin())
}

pub fn cos_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).cos())
}

pub fn tan_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(arg_to_float(args.get(0)).tan())
}

pub fn random_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(Rng::new().random() as fsize / u32::MAX as fsize)
}

pub fn random_int_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    Value::Num(Rng::new().random() as fsize)
}

pub fn random_range_api(_this: Value, args: Vec<Value>, vm: &mut VM) -> Value {
    let (start, end) = match args.get(0..2) {
        Some([Value::Num(start), Value::Num(end)]) => (*start as u32, *end as u32),
        _ => panic("InvalidArgumentError: Expected a valid 2 number type argument.".to_string(), vm)
    };

    if start > end {
        panic("InvalidArgumentError: Expected start value to be greater than end.".to_string(), vm)
    }

    Value::Num((start + (Rng::new().random() % (end - start + 1))) as fsize)
}