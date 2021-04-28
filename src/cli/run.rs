use std::{ path::PathBuf };
use std::fs::read_to_string;
use crate::{ 
    cli::command::Command, 
    lexer::parser::Lexer,
    ast::main::AST,
    bytecode::main::BytecodeCompiler,
    vm::vm::VM
};

pub fn run(cli: &Command) {

    if cli.args.len() <= 2 {
        cli.log_error("InvalidFileError: No file name specified.".to_string());
    }

    let fname = &cli.args[2];
    let body = match read_file(PathBuf::from(fname)) {
        Ok(content) => content,
        Err(e) => {
            cli.log_error("InvalidFileError: Could not read file: ".to_string() + format!("{:?}", e).as_str());
            String::new()
        }
    };

    let lexer = match Lexer::new(&cli.args[2], &body) {
        Ok(lexer) => lexer,
        Err(message) => {
            println!("{}", message);
            std::process::exit(0);
        }
    };

    let compiler = BytecodeCompiler::new(AST::new(&cli.args[2], &lexer));
    // println!("{:?}\n{:?}", compiler.bytes, compiler.constants);

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

fn read_file(fname: PathBuf) -> Result<String, std::io::Error> {
    let body = read_to_string(fname)?;
    Ok(String::from(body))
}