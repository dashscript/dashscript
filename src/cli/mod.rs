pub mod run;
pub mod command;
pub mod repl;

use std::fs::read_to_string;
use std::path::PathBuf;

pub fn read_file(fname: PathBuf) -> Result<String, std::io::Error> {
    let body = read_to_string(fname)?;
    Ok(String::from(body))
}