use std::fmt;
use crate::common::fsize;

#[derive(Copy, Clone, Default)]
pub struct Position {
    pub start: usize,
    pub end: usize
}

impl Position {

    pub fn new(lexer: &Lexer) -> Self {
        Self {
            start: lexer.ci,
            end: lexer.ci
        }
    }

    pub fn update(&mut self, lexer: &Lexer) -> Self { 
        self.end = lexer.ci; 
        self.clone()
    }

}

impl fmt::Debug for Position {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(fmt, "({}, {})", self.start, self.end)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Keyword {
    Var,
    Const,
    Func,
    Return,
    If,
    Elif,
    Else,
    Async,
    Await,
    Break,
    While,
    For,
    In,
    Continue,
    Class,
    Static
}

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOperator {
    Assign,
    Add,
    Sub
}

#[derive(Debug, Clone, PartialEq)]
pub enum LogicalOperator {
    Invert,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Null,
    Boolean(bool),
    Number(fsize),
    String(String),
    Word(String),
    Keyword(Keyword),
    Punc(char),
    Arithmetic(char),
    AssignmentOperator(AssignmentOperator),
    LogicalOperator(LogicalOperator)
}

#[derive(Clone, Default, Debug)]
pub struct Token {
    pub val: TokenType,
    pub pos: Position
}

impl Token {
    pub fn new(val: TokenType, pos: Position) -> Token {
        Token { val, pos }
    }
}

#[derive(Clone)]
pub struct Lexer {
    pub filename: String,
    pub body: String,
    pub len: usize,
    pub chars: Vec<char>,
    pub line: usize,
    pub last_line_ci: usize,
    pub ci: usize,
    pub tokens: Vec<Token>
}

impl Default for TokenType {
    fn default() -> Self { Self::Null }
}

impl Lexer {

    pub fn new(filename: &str, body: &str) -> Result<Lexer, LexerError> {
        let chars = body.chars().collect();
        let mut lexer = Lexer {
            filename: filename.to_owned(),
            body: body.to_owned(),
            chars,
            len: 0,
            line: 1,
            last_line_ci: 0,
            ci: 0,
            tokens: Vec::new()
        };

        lexer.parse()?;
        Ok(lexer)
    }

    pub fn get_col(&self) -> usize {
        self.ci - self.last_line_ci
    }

    pub fn parse(&mut self) -> Result<(), LexerError> {
        self.len = self.body.len();
        let mut next_char = Some(&self.chars[self.ci]);

        while next_char.is_some() {
            let char = *next_char.unwrap();

            if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || char == '_' {
                self.ci += 1;
                let word = self.parse_word(char);
                self.tokens.push(word);
                self.ci -= 1;
            } else if char >= '0' && char <= '9' {
                self.ci += 1;
                let num = self.parse_number(String::from(char))?;
                self.tokens.push(num);
            } else {
                match char {
                    '\'' | '"' => {
                        self.ci += 1;
                        let str = self.parse_string(char)?;
                        self.tokens.push(str);
                    },
                    '+' | '-' | '=' | '/' | '*' | '^' | '!' | '|' | '<' | '>' | '&' => self.parse_op(char)?,
                    '\r' | ' ' => (),
                    '\n' => self.next_line(),
                    ':' | ';' | '.' | ',' |'(' | ')' | '[' | ']' | '{' | '}' | '?' => self.tokens.push(Token::new(TokenType::Punc(char), Position::new(self))),
                    _ => return Err(LexerError::new(self, Position::new(self), "dserror(1): No unwanted syntax."))
                }
            }

            self.ci += 1;
            next_char = self.chars.get(self.ci);
        }

        Ok(())
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.last_line_ci = self.ci;
    }

}

pub struct LexerError {
    pub filename: String,
    pub line: usize,
    pub col: usize,
    pub body: String,
    pub reason: String
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let pref: String = format!("    {}:{}:{} -> ", self.filename, self.line, self.col);
        write!(f, "SyntaxError: {}\n{}{}\n{}{}", self.reason, pref, self.body, " ".repeat(pref.len()), "^".repeat(self.body.len()))
    }
}

impl LexerError {
    pub fn new(lexer: &Lexer, pos: Position, reason: &str) -> Self {
        let body = lexer.body[pos.start as usize..lexer.ci as usize].to_string();
        Self {
            filename: lexer.filename.clone(),
            line: lexer.line,
            col: lexer.get_col(),
            body,
            reason: reason.to_string()
        }
    }
}