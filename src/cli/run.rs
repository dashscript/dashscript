use std::path::PathBuf;
use dashscript_lexer::Lexer;
use dashscript_ast::AST;
use dashscript_bytecode::BytecodeCompiler;
use crate::read_file;
use crate::command::Command;

pub fn run(cli: &Command) {
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

    let _compiled = BytecodeCompiler::new(ast);
}