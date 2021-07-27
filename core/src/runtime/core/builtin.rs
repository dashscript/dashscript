use std::time::{SystemTime, UNIX_EPOCH};
use crate::{Vm, Value, TinyString, Map, RuntimeError, Instance};
use super::map_builder::{MapBuilder, ClassBuilder};
use super::date::{self, UNIX_EPOCH_DATE};

fn random_isize() -> isize {
    let mut random: isize = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as isize;

    let mut rand = || {
        random ^= random << 13;
        random ^= random >> 17;
        random ^= random << 5;
        random
    };
        
    if std::mem::size_of::<isize>() <= 4 {
        rand()
    } else {
        (((rand() as i64) << 32) | (rand() as i64)) as isize
    }
}

fn random_usize() -> usize {
    let mut random: usize = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .unwrap()
        .as_millis() as usize;

    let mut rand = || {
        random ^= random << 13;
        random ^= random >> 17;
        random ^= random << 5;
        random
    };
        
    if std::mem::size_of::<usize>() <= 4 {
        rand()
    } else {
        (((rand() as u64) << 32) | (rand() as u64)) as usize
    }
}

pub fn init_math(vm: &mut Vm) -> Value {
    let mut math = MapBuilder::new(vm);

    // All the basic and important constants
    math.constant("E", Value::Float(std::f64::consts::E));
    math.constant("LN10", Value::Float(std::f64::consts::LN_10));
    math.constant("LN2", Value::Float(std::f64::consts::LN_2));
    math.constant("PI", Value::Float(std::f64::consts::PI));
    math.constant("LOG10E", Value::Float(std::f64::consts::LOG10_E));
    math.constant("LOG2E", Value::Float(std::f64::consts::LOG2_E));
    math.constant("SQRT2", Value::Float(std::f64::consts::SQRT_2));

    // All the basic method based methods
    macro_rules! add_method_based_native_fn {
        ($name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0) {
                    Some(Value::Int(int)) => Value::Int(int.$method()),
                    Some(Value::Float(float)) => Value::Float(float.$method()),
                    _ => Value::NAN
                }
            )) 
        };

        (float $name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0) {
                    Some(Value::Float(float)) => Value::Float(float.$method()),
                    Some(Value::Int(int)) => Value::Float((*int as f64).$method()),
                    _ => Value::NAN
                }
            )) 
        };

        (float2 $name:expr, $method:ident) => {
            math.native_fn($name, |_, args| Ok(
                match args.get(0..1) {
                    Some([Value::Float(float), num]) => {
                        let num = match num {
                            Value::Int(int) => *int as f64,
                            Value::Float(float) => *float,
                            _ => return Ok(Value::NAN)
                        };

                        Value::Float(float.$method(num))
                    },
                    Some([Value::Int(int), num]) => {
                        let num = match num {
                            Value::Int(int) => *int as f64,
                            Value::Float(float) => *float,
                            _ => return Ok(Value::NAN)
                        };

                        Value::Float((*int as f64).$method(num))
                    },
                    _ => Value::NAN
                }
            )) 
        };
    }

    add_method_based_native_fn!("abs", abs);
    add_method_based_native_fn!(float "sqrt", sqrt);
    add_method_based_native_fn!(float "acosh", acosh);
    add_method_based_native_fn!(float "acos", acos);
    add_method_based_native_fn!(float "asin", asin);
    add_method_based_native_fn!(float "asinh", asinh);
    add_method_based_native_fn!(float "atan", atan);
    add_method_based_native_fn!(float "atanh", atanh);
    add_method_based_native_fn!(float "cbrt", cbrt);
    add_method_based_native_fn!(float "ceil", ceil);
    add_method_based_native_fn!(float "cos", cos);
    add_method_based_native_fn!(float "cosh", cosh);
    add_method_based_native_fn!(float "exp", exp);
    add_method_based_native_fn!(float "floor", floor);
    add_method_based_native_fn!(float "round", round);
    add_method_based_native_fn!(float "log10", log10);
    add_method_based_native_fn!(float "log2", log2);
    add_method_based_native_fn!(float "sin", sin);
    add_method_based_native_fn!(float "sinh", sinh);
    add_method_based_native_fn!(float "tan", tan);
    add_method_based_native_fn!(float "tamh", tanh);
    add_method_based_native_fn!(float "trunc", trunc);
    add_method_based_native_fn!(float2 "hypot", hypot);
    add_method_based_native_fn!(float2 "log", log);
    add_method_based_native_fn!(float2 "atan2", atan2);

    math.native_fn("max", |_, args| Ok(
        match args.iter().max() {
            Some(value) => value.clone(),
            None => Value::Null
        }
    ));

    math.native_fn("min", |_, args| Ok(
        match args.iter().min() {
            Some(value) => value.clone(),
            None => Value::Null
        }
    ));

    math.native_fn("random", |_, _| Ok(Value::Float(random_usize() as f64 / usize::MAX as f64)));
    math.native_fn("randomUint", |_, _| Ok(Value::Int(random_usize() as isize)));
    math.native_fn("randomInt", |_, _| Ok(Value::Int(random_isize())));

    Value::Dict(math.allocate_with())
}

pub fn init_date(vm: &mut Vm) -> Value {
    let mut date = ClassBuilder::new(vm);

    fn unwrap_date(vm: &Vm, args: &[Value]) -> isize {
        match args.get(0) {
            Some(Value::Instance(ptr)) => {
                match ptr.unwrap_ref().properties.get(&vm.constants.__date) {
                    Some((Value::Int(int), _)) => *int,
                    _ => UNIX_EPOCH_DATE
                }
            },
            _ => UNIX_EPOCH_DATE
        }
    }

    fn unwrap_time(vm: &Vm, args: &[Value]) -> isize {
        match args.get(0) {
            Some(Value::Instance(ptr)) => {
                match ptr.unwrap_ref().properties.get(&vm.constants.__time) {
                    Some((Value::Int(int), _)) => *int,
                    _ => 0
                }
            },
            _ => 0
        }
    }

    fn unwrap_both(vm: &Vm, args: &[Value]) -> (isize, isize) {
        match args.get(0) {
            Some(Value::Instance(ptr)) => {
                let instance = ptr.unwrap_ref();
                match (instance.properties.get(&vm.constants.__date), instance.properties.get(&vm.constants.__time)) {
                    (Some((Value::Int(date), _)), Some((Value::Int(time), _))) => (*date, *time),
                    _ => (UNIX_EPOCH_DATE, 0)
                }
            },
            _ => (UNIX_EPOCH_DATE, 0)
        }
    }

    fn unwrap_date_entry(option: Option<&(Value, bool)>) -> isize {
        match option {
            Some((Value::Int(int), _)) => *int,
            _ => UNIX_EPOCH_DATE
        }
    }

    date.init(|vm, args| {
        match &args {
            [Value::Instance(ptr)] => {
                let instance = ptr.unwrap_mut();
                let (date, time) = date::from_system_time(SystemTime::now());
                instance.properties.insert(vm.constants.__date, (Value::Int(date), false)); 
                instance.properties.insert(vm.constants.__time, (Value::Int(time), false)); 
            },
            [Value::Instance(ptr), Value::Null] => {
                let instance = ptr.unwrap_mut();
                instance.properties.insert(vm.constants.__time, (Value::Int(0), false)); 
                instance.properties.insert(vm.constants.__date, (Value::Int(UNIX_EPOCH_DATE), false)); 
            },
            [Value::Instance(ptr), ms] => {
                let instance = ptr.unwrap_mut();
                let ms = ms.to_isize();
                let date = date::add_date_to_duration(UNIX_EPOCH_DATE, ms);
                instance.properties.insert(vm.constants.__date, (Value::Int(date), false)); 
                instance.properties.insert(vm.constants.__time, (Value::Int(ms % date::MS_PER_DAY), false)); 
            },
            [Value::Instance(ptr), year, month, day, ms] => {
                let instance = ptr.unwrap_mut();
                let ms = ms.to_isize();
                let date = match date::ymd_into_date_checked(year.to_isize(), month.to_u8(), day.to_u8()) {
                    Some(date) => date,
                    None => return Err(RuntimeError::new(vm, format!("[Date.init]: Improper date {}/{}/{}.", year, month, day)))
                };

                if ms > date::MS_PER_DAY {
                    return Err(RuntimeError::new(vm, format!("[Date.init]: Improper milliseconds {}.", ms)))
                }

                instance.properties.insert(vm.constants.__date, (Value::Int(date), false)); 
                instance.properties.insert(vm.constants.__time, (Value::Int(ms), false)); 
            }
            _ => return Err(RuntimeError::new(vm, "[Date.init]: Expected (Date, int) or (Date, null) or (Date, year, month, date, ms) arguments."))
        }

        Ok(Value::Null)
    });

    date.native_fn("now", |_, _| Ok(Value::Int(
        SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_millis() as isize
    )));

    date.prototype_fn("getDate", |vm, args| {
        let date = unwrap_date(vm, args);
        Ok(Value::Int(date::date_into_ymd(date).2 as isize))
    });

    date.prototype_fn("getYear", |vm, args| {
        let date = unwrap_date(vm, args);
        Ok(Value::Int(date >> 9))
    });

    date.prototype_fn("getMonth", |vm, args| {
        let date = unwrap_date(vm, args);
        Ok(Value::Int(date::date_into_ymd(date).1 as isize))
    });

    date.prototype_fn("getSeconds", |vm, args| {
        Ok(Value::Int(unwrap_time(vm, args) / date::MS_PER_SECOND))
    });

    date.prototype_fn("getMinutes", |vm, args| {
        Ok(Value::Int(unwrap_time(vm, args) / date::MS_PER_MINUTE))
    });

    date.prototype_fn("getHours", |vm, args| {
        Ok(Value::Int(unwrap_time(vm, args) / date::MS_PER_HOUR))
    });

    date.prototype_fn("getTime", |vm, args| {
        let (date, time) = unwrap_both(vm, args);
        Ok(Value::Int(date::date_time_to_ms(date, time)))
    });

    date.prototype_fn("setDate", |vm, args| {
        match args.get(0..2) {
            Some([Value::Instance(ptr), date]) => {
                let new_date = date.to_u8();
                let instance = ptr.unwrap_mut();
                let (year, month, _) = date::date_into_ymd(unwrap_date_entry(instance.properties.get(&vm.constants.__date)));

                {
                    let max_days = date::number_of_days_in_month(month, year).unwrap();
                    if (new_date <= 0) || (new_date > max_days) {
                        return Err(RuntimeError::new(vm, format!("[Date.setDate]: Expected a date within the range of 1..={}. But found {}.", max_days, new_date)));
                    }
                }

                instance.properties.insert(vm.constants.__date, (Value::Int(date::ymd_into_date(year, month, new_date)), false));
                Ok(Value::Null)
            },
            _ => Err(RuntimeError::new(vm, "[Date.setDate]: Expected (Date, int) arguments."))
        }
    });

    date.prototype_fn("setYear", |vm, args| {
        match args.get(0..2) {
            Some([Value::Instance(ptr), date]) => {
                let year = date.to_isize();
                let instance = ptr.unwrap_mut();
                let (_, month, date) = date::date_into_ymd(unwrap_date_entry(instance.properties.get(&vm.constants.__date)));

                if (year < -100_000) || (year > 100_000) {
                    return Err(RuntimeError::new(vm, "[Date.setYear]: Expected the year to be in the range of -100_000..100_000"));
                }

                instance.properties.insert(vm.constants.__date, (Value::Int(date::ymd_into_date(year, month, date)), false));
                Ok(Value::Null)
            },
            _ => Err(RuntimeError::new(vm, "[Date.setDate]: Expected (Date, int) arguments."))
        }
    });

    date.prototype_fn("setMonth", |vm, args| {
        match args.get(0..2) {
            Some([Value::Instance(ptr), date]) => {
                let month = date.to_u8();
                let instance = ptr.unwrap_mut();
                let (year, _, date) = date::date_into_ymd(unwrap_date_entry(instance.properties.get(&vm.constants.__date)));

                if (month == 0) || (month > 12) {
                    return Err(RuntimeError::new(vm, "[Date.setMonth]: Expected the month to be in the range of 1..=12"));
                }

                instance.properties.insert(vm.constants.__date, (Value::Int(date::ymd_into_date(year, month, date)), false));
                Ok(Value::Null)
            },
            _ => Err(RuntimeError::new(vm, "[Date.setDate]: Expected (Date, int) arguments."))
        }
    });

    date.prototype_fn("setTime", |vm, args| {
        match args.get(0..2) {
            Some([Value::Instance(ptr), time]) => {
                let instance = ptr.unwrap_mut();
                let ms = time.to_isize();
                let date = date::add_date_to_duration(UNIX_EPOCH_DATE, ms);
                instance.properties.insert(vm.constants.__date, (Value::Int(date), false)); 
                instance.properties.insert(vm.constants.__time, (Value::Int(ms % date::MS_PER_DAY), false)); 

                Ok(Value::Null)
            },
            _ => Err(RuntimeError::new(vm, "[Date.setDate]: Expected (Date, int) arguments."))
        }
    });

    macro_rules! set_amount {
        ($vm:expr, $args:expr, $x:expr) => {
            match $args.get(0..2) {
                Some([Value::Instance(ptr), value]) => {
                    let instance = ptr.unwrap_mut();
                    let value = value.to_isize();
                    instance.properties.insert($vm.constants.__time, (Value::Int(value * $x), false)); 
    
                    Ok(Value::Null)
                },
                _ => Err(RuntimeError::new($vm, "[Date.setDate]: Expected (Date, int) arguments."))
            }
        };
    }

    date.prototype_fn("setSeconds", |vm, args| {
        set_amount!(vm, args, 1000)
    });

    date.prototype_fn("setMinutes", |vm, args| {
        set_amount!(vm, args, date::MS_PER_MINUTE)
    });

    date.prototype_fn("setHours", |vm, args| {
        set_amount!(vm, args, date::MS_PER_HOUR)
    });

    Value::Dict(date.allocate_with())
}

pub fn init_json(vm: &mut Vm) -> Value {
    let mut json = MapBuilder::new(vm);

    json.native_fn("stringify", |vm, args| {
        let string = vm.allocate_with(
            TinyString::new(
                args.get(0)
                    .unwrap_or_default()
                    .json_stringify()
                    .as_bytes()
            )
        );

        Ok(Value::String(string))
    });

    Value::Dict(json.allocate_with())
}

pub fn init_process(vm: &mut Vm) -> Value {
    let mut process = ClassBuilder::new(vm);

    process.init(|vm, args| {
        // Unsafe becase this can chaneg stuff of other resources by wrong rid.
        if !vm.permissions._unsafe {
            return Err(RuntimeError::new(vm, "[Process.init]: Initiating process manually requires the `--unsafe` flag."))
        }

        match args.get(0..3) {
            Some([Value::Instance(ptr), rid, pid]) => {
                let instance = ptr.unwrap_mut();
                instance.properties.insert(vm.constants.rid, (*rid, true));
                instance.properties.insert(vm.constants.pid, (*pid, true)); 

                if let Some(&[stdin, stdout, stderr]) = args.get(4..7) {
                    instance.properties.insert(vm.constants.stdin, (stdin, true));
                    instance.properties.insert(vm.constants.stdout, (stdout, true));
                    instance.properties.insert(vm.constants.stderr, (stderr, true));
                }

                Ok(Value::Null)
            },
            _ => return Err(RuntimeError::new(vm, "[Process.init]: Expected (Process, rid, pid) arguments."))
        }
    });

    let (class, prototype) = process.allocate_with_with_prototype();
    vm.constants.process_prototype = prototype;

    Value::Dict(class)
}

pub fn initiate_process_instance(
    vm: &mut Vm, 
    rid: u32, 
    pid: u32, 
    stdout: Option<u32>,
    stdin: Option<u32>,
    stderr: Option<u32>
) -> Value {
    let mut properties = Map::with_capacity(2);
    properties.insert(vm.constants.pid, (Value::Int(pid as _), true));
    properties.insert(vm.constants.rid, (Value::Int(rid as _), true));

    macro_rules! if_let_stdio {
        ($($key:ident)+) => {
            $(if let Some(rid) = $key {
                properties.insert(vm.constants.$key, (Value::Int(rid as _), true));
            })+
        };
    }

    if_let_stdio! { stdout stdin stderr }

    Value::Instance(vm.allocate_with(
        Instance { 
            properties, 
            methods: vm.constants.process_prototype
        }
    ))
}