use std::fmt;
use dashscript_lexer::{Token, TokenType, Position, Lexer};
use crate::{Statement, ConstantPool, StatementType};

#[derive(Debug, Clone)]
pub struct AST {
    pub filename: String,
    pub statements: Vec<Statement>,
    pub tokens: Vec<Token>,
    pub constants: Vec<String>,
    pub body: String,
    pub ci: usize,
    pub len: usize,
    pub constant_pool: ConstantPool
}

impl AST {

    pub fn new(lexer: Lexer) -> Result<AST, ASTError> {
        let mut this = AST { 
            filename: lexer.filename, 
            statements: Vec::new(),
            constants: Vec::new(),
            tokens: lexer.tokens,
            body: lexer.body,
            ci: 0,
            len: 0,
            constant_pool: ConstantPool::default()
        };

        this.parse_body()?;
        // for i in this.statements.iter() { println!("{:#?}", i); }
        Ok(this.clone())
    }

    pub fn parse_body(&mut self) -> Result<(), ASTError> {
        self.len = self.tokens.len();
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(keyword) => {
                    let stmt = self.parse_keyword_statement(keyword, token.pos)?;
                    self.statements.push(stmt);
                },
                TokenType::Word(name) => {
                    self.ci += 1;
                    let stmt = self.parse_word_statement(name, token.pos)?;
                    self.statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let primary_stmt = Statement {
                        val: StatementType::Primary(self.parse_value("")?),
                        pos: token.pos
                    };

                    self.statements.push(primary_stmt);
                }
            }

            self.ci += 1;
        }

        Ok(())
    }
    
    pub fn create_error(&self, pos: Position, reason: &str) -> ASTError {
        let (line, col) = ASTError::get_line_col(self.body.clone(), pos.start);
        ASTError {
            line,
            col,
            filename: self.filename.clone(),
            body: self.body[pos.start as usize - 1..pos.end as usize].to_string(),
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

impl ASTError {
    pub fn get_line_col(body: String, start: usize) -> (usize, usize) {
        let lines: Vec<&str> = body.split("\n").collect();
        let mut i = 0;
        for line in 0..lines.len() {
            i += lines[line].len() + 1;
            if i > start {
                return (line+1, i - start);
            }
        }
    
        (lines.len(), 0)
    }
}

impl fmt::Display for ASTError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pref: String = format!("    {}:{}:{} -> ", self.filename, self.line, self.col);
        write!(f, "SyntaxError: {}\n{}{}\n{}{}", self.reason, pref, self.body, " ".repeat(pref.len()), "^".repeat(self.body.len()))
    }
}