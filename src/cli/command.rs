use std::collections::HashMap;
use std::env;
use std::process::exit;

#[derive(Debug)]
pub struct Command {
    pub args: Vec<String>,
    pub flags: HashMap<String, String>
}

impl Command {

    pub fn new() -> Command {
        let args = env::args();
        let mut complete_args = vec![];
        let mut flags = HashMap::new();

        for arg in args {
            if arg.starts_with("--") {
                let split: Vec<&str> = arg[2..].split("=").collect();
                flags.insert(split[0].to_string(), match split.get(1) {
                    Some(val) => val.to_string(),
                    None => String::new()
                });
            } else {
                complete_args.push(arg);
            }
        }

        Self {
            args: complete_args,
            flags
        }
    }

    pub fn log_error(&self, reason: &str) -> ! {
        println!("{}", reason);
        exit(0)
    }

}