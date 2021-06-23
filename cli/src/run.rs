extern crate dashscript_core;

use std::path::PathBuf;
use dashscript_core::{AST, BytecodeCompiler, Vm, TinyString};
use crate::command::Cli;
use crate::read_file;

pub fn run(cli: &mut Cli) {
    if cli.args.len() <= 2 {
        Cli::log_error("InvalidFileError: No file name specified.");
    }

    let fname = &cli.args[2];
    let pathbuf = PathBuf::from(fname);
    let body = match read_file(pathbuf.clone()) {
        Ok(content) => content,
        Err(e) => Cli::log_error(&format!("InvalidFileError: Could not read file: {:?}", e))
    };

    let build = match AST::compile(&cli.args[2], &body) {
        Ok(build) => build,
        Err(errors) => Cli::log_errors(errors)
    };

    macro_rules! insert_flag {
        ($name:expr, $value:expr) => {
            cli.flags.insert($name.into(), $value)
        };
    }

    insert_flag!("filename", TinyString::new(cli.args[2].as_bytes()));

    let compiler = match BytecodeCompiler::new(build) {
        Ok(compiler) => compiler,
        Err(errors) => Cli::log_errors(errors)
    };

    match Vm::new(compiler.into(), cli.flags.clone(), pathbuf) {
        Ok(_) => (),
        Err(e) => println!("{:?}", e)
    };
}