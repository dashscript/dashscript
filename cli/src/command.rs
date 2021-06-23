use std::collections::HashMap;
use std::env;
use std::fmt::Display;
use std::process::exit;
use dashscript_core::{CompilerError, ASTError, TinyString};

pub trait ErrorWritter {
    const KIND: &'static str;
    fn write_error(&self, index: usize);
}

impl ErrorWritter for CompilerError {
    const KIND: &'static str = "bytecode compiler";
    fn write_error(&self, index: usize) {
        println!("    {}. at [line {}]: {}", index, self.line, self.kind.to_string());
    }
}

impl ErrorWritter for ASTError {
    const KIND: &'static str = "ast";
    fn write_error(&self, index: usize) {
        let (filename, line, col) = self.position();
        let body = self.body();
        println!("    {}. at [{}:{}:{}] {}\n        {}\n        {}\n", index, filename, line, col, self, body.escape_debug(), "^".repeat(body.len()));
    }
}

#[derive(Debug)]
pub struct Cli {
    pub args: Vec<String>,
    pub flags: HashMap<TinyString, TinyString>
}

impl Cli {

    pub fn new() -> Cli {
        let args = env::args();
        let mut complete_args = vec![];
        let mut flags = HashMap::new();

        for arg in args {
            if arg.starts_with("--") {
                let split: Vec<&str> = arg[2..].split("=").collect();
                flags.insert(TinyString::new(split[0].as_bytes()), match split.get(1) {
                    Some(val) => TinyString::new(val.as_bytes()),
                    None => TinyString::new(&[])
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

    pub fn log_error<D: Display>(reason: D) -> ! {
        println!("{}", reason);
        exit(1)
    }

    pub fn log_errors<E: ErrorWritter>(errors: Vec<E>) -> ! {
        println!("Found {} {} based errors.", errors.len(), E::KIND);
        for (index, error) in errors.into_iter().enumerate() {
            error.write_error(index + 1);
        }

        exit(1)
    }

}