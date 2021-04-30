use std::fmt;
use super::types::Statement;
use crate::common::get_line_col_by_body;
use crate::lexer::parser::{ Token, TokenType, Position, Lexer };

#[derive(Debug, Clone)]
pub struct AST {
    pub filename: String,
    pub statements: Vec<Statement>,
    pub tokens: Vec<Token>,
    pub body: String,
    pub ci: usize,
    pub len: usize
}

impl AST {

    pub fn new(lexer: &Lexer) -> Result<AST, ASTError> {
        let this = &mut AST { 
            filename: lexer.filename.clone(), 
            statements: Vec::new(),
            tokens: lexer.tokens.clone(),
            body: lexer.body.clone(),
            ci: 0,
            len: 0
        };

        this.parse_body()?;
        // println!("\nStatements:\n");
        // for i in this.statements.iter() { println!("{:#?}", i); }
        Ok(this.clone())
    }

    pub fn parse_body(&mut self) -> Result<(), ASTError> {
        self.len = self.tokens.len();

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos)?;
                    self.statements.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_word_statement(word.to_string(), token.pos)?;
                    self.statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.get_value_as_stmt()?;
                    self.statements.push(stmt);
                }
            }

            self.ci += 1;
        }

        Ok(())
    }

    pub fn next_token(&mut self, err: &str) -> Result<Token, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(t) => Ok(t.clone()),
            None => Err(self.create_error(self.tokens[self.ci - 2].pos, err))
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.ci].clone()
    }

    pub fn create_error(&self, pos: Position, reason: &str) -> ASTError {
        let (line, col) = get_line_col_by_body(self.body.clone(), pos.start);
        let body = self.body[pos.start as usize - 1..pos.end as usize].to_string();
        ASTError {
            line,
            col,
            filename: self.filename.clone(),
            body,
            reason: reason.to_string()
        }
    }

}

pub struct ASTError {
    pub line: usize,
    pub col: usize,
    pub filename: String,
    pub body: String,
    pub reason: String
}

impl fmt::Display for ASTError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pref: String = format!("    {}:{}:{} -> ", self.filename, self.line, self.col);
        write!(f, "SyntaxError: {}\n{}{}\n{}{}", self.reason, pref, self.body, " ".repeat(pref.len()), "^".repeat(self.body.len()))
    }
}