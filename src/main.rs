pub mod cli;
pub mod lexer;
pub mod ast;
pub mod bytecode;
pub mod vm;
pub mod common;

fn main() {

    let command = cli::command::Command::new();

    if command.args.len() == 1 {
        cli::repl::start_repl(&command);
    }

    match command.args[1].as_str() {
        "run" => cli::run::run(&command),
        "repl" => cli::repl::start_repl(&command),
        _ => ()
    }

}