pub mod cli;
pub mod lexer;
pub mod ast;
pub mod bytecode;
pub mod vm;
pub mod common;

fn main() {

    let command = cli::command::Command::new();

    if command.args.len() == 1 {
        command.log_error("NoCommandError: No command has been provided to execute.".to_string());
    }

    if command.args[1] == "run" {
        cli::run::run(&command);
    }

}