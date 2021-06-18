use std::ptr::NonNull;
use std::ops::Deref;
use std::io;
use crate::{Vm, Value, TinyString, NativeFunction, RuntimeError, ValueIter, GcHeader};

pub mod modules;
pub mod builtin;
pub mod map_builder;
pub mod json;
pub mod window;
pub mod methods;

// Easy way to create objects instead of deep syntaxes
#[macro_export]
macro_rules! dict {
    ($vm:expr, {$($key:tt: $val:expr,)+}) => {{
        let mut map = std::collections::HashMap::new();
        $(map.insert(DictKey::Str($key.to_string()), (Value::from($val), false));)*
    }};

    ($vm:expr, extendable {$($key:tt: $val:expr,)+}) => {{
        let mut map = std::collections::HashMap::new();
        $(map.insert(DictKey::Str($key.to_string()), (Value::from($val), false));)*
        map
    }};

    (extend $map:expr => {$($key:tt: $val:expr,)+}) => {{
        $($map.insert(DictKey::Str($key.to_string()), (Value::from($val), false));)*
    }};
}

pub fn init(vm: &mut Vm) {

    vm.add_global("NaN", Value::NAN);
    vm.add_global("inf", Value::INFINITY);

    let math = builtin::init_math(vm);
    vm.add_global("Math", math);

    let date = builtin::init_date(vm);
    vm.add_global("Date", date);

    let json = builtin::init_json(vm);
    vm.add_global("JSON", json);

    if vm.permissions.memory {
        let memory = builtin::init_memory(vm);
        vm.add_global("Memory", memory);
    }

    if vm.permissions.child_process {
        let child_process = builtin::init_child_process(vm);
        vm.add_global("ChildProcess", child_process);
    }

    let window = window::init(vm);
    vm.add_global("window", window);

    methods::iterator::init(vm);
    methods::string::init(vm);
    methods::boolean::init(vm);
    methods::object::init(vm);
    methods::function::init(vm);
    methods::array::init(vm);

    macro_rules! native_fn {
        ($bytes:expr, $value:expr) => {{
            let name = TinyString::new($bytes);
            let constant_id = vm.chunk.constants.add_string(name.clone());
            let nf = NativeFunction {
                name, 
                func: $value,
                is_instance: false
            };

            let ptr = unsafe { NonNull::new_unchecked(vm.allocate(nf)) };
            vm.globals.insert(constant_id, (Value::NativeFn(ptr), false));
        }};
    }

    native_fn!(b"print", |_, args| {
        for arg in args {
            print!("{}", arg);
        }

        Ok(Value::Null)
    });

    native_fn!(b"println", |_, args| {
        for arg in args {
            println!("{}", arg);
        }

        Ok(Value::Null)
    });

    native_fn!(b"typeof", |vm, args| {
        let type_ = vm.allocate_str(args.get(0).unwrap_or_default().get_type());
        Ok(Value::String(type_))
    });

    native_fn!(b"throw", |_, args| {
        Err(RuntimeError::from(args.get(0).unwrap_or_default().clone()))
    });

    native_fn!(b"parseInt", |_, args| {
        match args.get(0)
            .unwrap_or_default()
            .unwrap_string()
            .parse::<isize>() 
        {
            Ok(int) => Ok(Value::Int(int)),
            Err(_) => Ok(Value::NAN)
        }
    });

    native_fn!(b"parseFloat", |_, args| {
        match args.get(0)
            .unwrap_or_default()
            .unwrap_string()
            .parse::<f64>() 
        {
            Ok(float) => Ok(Value::Float(float)),
            Err(_) => Ok(Value::NAN)
        }
    });

    native_fn!(b"readline", |vm, _| {
        let mut line = String::new();
        match io::stdin().read_line(&mut line) {
            Ok(_) => Ok(Value::String(vm.allocate_str_bytes(line.as_bytes()))),
            Err(_) => Ok(Value::Null) // Incase if there is an error it returns null
        }
    });

    native_fn!(b"isNaN", |_, args| Ok(Value::Bool(
        match args.get(0) {
            Some(value) => value.is_nan(),
            None => false
        }
    )));

    native_fn!(b"range", |vm, args| {
        let (start, end) = match args.as_slice() {
            [value] => (0, value.to_isize()),
            [start, end] => (start.to_isize(), end.to_isize()),
            _ => (0, 0)
        };

        let mut vec = Vec::new();
        for i in start..end {
            vec.push(Value::Int(i));
        }

        let ptr = vm.allocate(ValueIter::new(vec.as_slice()));
        Ok(Value::Iterator(unsafe { NonNull::new_unchecked(ptr) }))
    });

}

impl Value {
    pub(super) fn unwrap_string(&self) -> &str {
        match self {
            Self::String(bytes) => unsafe { &GcHeader::unwrap_ref::<TinyString>(bytes.as_ptr()).deref() },
            _ => "null"
        }
    }
}