use std::path::PathBuf;
use super::read_file;
use crate::cli::command::Command;
use crate::lexer::parser::Lexer;
use crate::ast::main::AST;
use crate::bytecode::main::BytecodeCompiler;
use crate::vm::vm::VM;

pub fn run(cli: &Command) {
    if cli.args.len() <= 2 {
        cli.log_error("InvalidFileError: No file name specified.".to_string());
    }

    let fname = &cli.args[2];
    let body = match read_file(PathBuf::from(fname)) {
        Ok(content) => content,
        Err(e) => {
            cli.log_error("InvalidFileError: Could not read file: ".to_string() + format!("{:?}", e).as_str());
            std::process::exit(0);
        }
    };

    let lexer = match Lexer::new(&cli.args[2], &body) {
        Ok(lexer) => lexer,
        Err(message) => {
            println!("{}", message);
            std::process::exit(0);
        }
    };

    let compiler = BytecodeCompiler::new(match AST::new(&lexer) {
        Ok(ast) => ast,
        Err(err) => {
            println!("{}", err);
            std::process::exit(0);
        }
    });

    // Flag for just development purpose to trace back errors.
    if cli.flags.get("show-bytes").is_some() {
        println!("Constants = {:?}", compiler.constants);
        println!("Instructions = {:?}", compiler.bytes);
        return;
    }

    match VM::new(
        compiler, 
        cli.args[2].clone(),
        body,
        cli.permissions()
    ) {
        Ok(_) => (),
        Err(msg) => {
            println!("{:?}", msg);
            std::process::exit(0);
        }
    };
}