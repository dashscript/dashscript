use std::path::PathBuf;
use crate::fmt::main::Formatter;
use crate::cli::command::Command;
use super::read_file;

pub fn fmt(cli: &Command) {
    if cli.args.len() <= 2 {
        cli.log_error("InvalidFileError: No file name specified.".to_string());
    }

    let fname = &cli.args[2];
    let fmt = Formatter::new(
        match read_file(PathBuf::from(fname)) {
            Ok(content) => content,
            Err(e) => {
                cli.log_error("InvalidFileError: Could not read file: ".to_string() + format!("{:?}", e).as_str());
                std::process::exit(0);
            }
        }, 
        match cli.flags.get("spaces") {
            Some(count) => count.parse().unwrap_or(4),
            None => 4
        }, fname
    );

    println!("{}", fmt);
}