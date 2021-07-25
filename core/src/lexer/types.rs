use std::fmt::{self, Formatter, Debug};
use crate::{Lexer, TinyString, LexerErrorKind};

#[derive(Copy, Clone, Default)]
pub struct Position {
    pub start: u32,
    pub end: u32
}

impl Position {
    pub fn new(lexer: &Lexer) -> Self {
        Self {
            start: lexer.index as u32,
            end: lexer.index as u32
        }
    }

    pub fn update(&mut self, lexer: &Lexer) -> Self { 
        self.end = lexer.index as u32; 
        *self
    }
}

impl Debug for Position {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> fmt::Result {
        write!(fmt, "({}, {})", self.start, self.end)
    }
}

impl From<(u32, u32)> for Position {
    fn from(pos: (u32, u32)) -> Self {
        Self {
            start: pos.0,
            end: pos.1
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Let,
    Const,
    Func,
    Return,
    Yield,
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
    Import,
    As,
    Try,
    Expect
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Null,
    True,
    False,
    Int(isize),
    Float(f64),
    String(TinyString),
    Word(TinyString),
    Keyword(Keyword),
    Error(LexerErrorKind),
    Add,
    Sub,
    Mul,
    Div,
    Pow,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    BitNot,
    Shr,
    Shl,
    Not,
    Equal,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    And,
    Or,
    Assign,
    AssignAdd,
    AssignSub,
    Colon,
    Semicolon,
    Dot,
    Comma,
    Question,
    ParenOpen,
    ParenClose,
    SqBraceOpen,
    SqBraceClose,
    CurlyBraceOpen,
    CurlyBraceClose
}

impl Default for TokenKind {
    fn default() -> Self { Self::Null }
}

#[derive(Clone, Default, Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position
}

impl Token {
    pub fn new(kind: TokenKind, position: Position) -> Self {
        Self { kind, position }
    }
}