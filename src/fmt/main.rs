use std::fmt;
use std::process::exit;
use crate::lexer::parser::Lexer;
use crate::ast::main::AST;
use crate::ast::types::{ Statement };

pub struct Formatter {
    pub body: Vec<Statement>,
    pub result: String,
    pub spaces: u8
}

// Just for testing purposes.
impl fmt::Display for Formatter {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.result)
    }
}

impl Formatter {

    pub fn new(body: String, spaces: u8, filename: &String) -> Self {
        let mut fmt = Self {
            body: match AST::new(match Lexer::new(filename, &body) {
                Ok(ref lexer) => lexer,
                Err(e) => {
                    println!("{}", e);
                    exit(1);
                }
            }) {
                Ok(ast) => ast.statements,
                Err(e) => {
                    println!("{}", e);
                    exit(1);
                }
            },
            spaces,
            result: String::new()
        };

        fmt.format();
        fmt
    }

    fn format(&mut self) {}

}