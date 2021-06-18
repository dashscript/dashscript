pub mod run;
pub mod command;
// pub mod repl;

use command::Cli;
use std::fs::read_to_string;
use std::path::PathBuf;

pub fn read_file(fname: PathBuf) -> Result<String, std::io::Error> {
    Ok(String::from(read_to_string(fname)?))
}

pub fn main() {
    let mut command = Cli::new();
    match command.args.get(1) {
        Some(cmd_name) => {
            match cmd_name as &str {
                "run" => run::run(&mut command),
                name => println!("CliError: Detected an unknown command \"{}\"", name)
            }
        },
        _ => println!("CliError: No command has been provided to execute.")
    }
}