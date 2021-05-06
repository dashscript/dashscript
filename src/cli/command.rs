use std::collections::HashMap;
use std::env;
use std::process::exit;

pub struct Command {
    pub args: Vec<String>,
    pub flags: HashMap<String, String>
}

impl Command {

    pub fn new() -> Command {
        let mut cmd = Command{
            args: env::args().collect(),
            flags: HashMap::new()
        };

        for arg in cmd.args.clone() {
            if arg.starts_with("--") {
                let split: Vec<&str> = arg[2..].split("=").collect();
                cmd.flags.insert(split[0].to_string(), match split.get(1) {
                    Some(val) => val.to_string(),
                    None => String::new()
                });
            }
        }

        cmd
    }

    pub fn log_error(&self, reason: String) {
        println!("{}", reason);
        exit(0);
    }

}