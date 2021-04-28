use super::types::Statement;
use crate::{
    common::get_line_col_by_body,
    lexer::parser::{
        Token,
        TokenType,
        Position,
        Lexer
    }
};

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

    pub fn new(filename: &String, lexer: &Lexer) -> AST {
        let this = &mut AST { 
            filename: filename.clone(), 
            statements: Vec::new(),
            tokens: lexer.tokens.clone(),
            body: lexer.body.clone(),
            ci: 0,
            len: 0
        };

        this.parse_body();
        // println!("\nStatements:\n");
        // for i in this.statements.iter() { println!("{:#?}", i); }

        this.clone()
    }

    pub fn parse_body(&mut self) {
        self.len = self.tokens.len();

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos);
                    self.statements.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_word_statement(word.to_string(), token.pos);
                    self.statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.parse_value_as_stmt();
                    self.statements.push(stmt);
                }
            }

            self.ci += 1;
        }
    }

    pub fn next_token(&mut self, err: &str) -> Token {
        self.ci += 1;

        match self.tokens.get(self.ci) {
            Some(t) => t.clone(),
            None => {
                self.throw_error(self.tokens[self.ci - 2].pos, err);
                Token::default()
            }
        }
    }

    pub fn current_token(&self) -> Token {
        self.tokens[self.ci].clone()
    }

    pub fn has_next(&self) -> bool {
        self.ci+1 < self.len
    }

    pub fn throw_error(&self, pos: Position, reason: &str) {
        let (line, col) = get_line_col_by_body(self.body.clone(), pos.start);
        let pref: String = format!("    {}:{}:{} -> ", self.filename, line, col);
        let body = self.body[pos.start as usize - 1..pos.end as usize].to_string();
        println!("SyntaxError: {}\n{}{}\n{}{}", reason, pref, body, " ".repeat(pref.len()), "^".repeat(body.len()));
        std::process::exit(0);
    }

}