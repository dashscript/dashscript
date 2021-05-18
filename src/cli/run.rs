use std::path::PathBuf;
use dashscript_lexer::Lexer;
use dashscript_ast::AST;
use dashscript_bytecode::BytecodeCompiler;
use dashscript_runtime::Vm;
use crate::command::Command;
use crate::read_file;

pub fn run(cli: &mut Command) {
    if cli.args.len() <= 2 {
        cli.log_error("InvalidFileError: No file name specified.");
    }

    let fname = &cli.args[2];
    let body = match read_file(PathBuf::from(fname)) {
        Ok(content) => content,
        Err(e) => cli.log_error(&format!("InvalidFileError: Could not read file: {:?}", e))
    };

    let lexer = match Lexer::new(&cli.args[2], &body) {
        Ok(lexer) => lexer,
        Err(message) => {
            println!("{}", message);
            std::process::exit(0);
        }
    };

    let ast = match AST::new(lexer) {
        Ok(ast) => ast,
        Err(message) => {
            println!("{}", message);
            std::process::exit(0);
        }
    };

    macro_rules! insert_flag {
        ($name:expr, $value:expr) => {
            cli.flags.insert($name.into(), $value)
        };
    }

    insert_flag!("filename", cli.args[2].clone());

    match Vm::new(BytecodeCompiler::new(ast).into(), cli.flags.clone()) {
        Ok(_) => (),
        Err(e) => println!("{:?}", e)
    };
}