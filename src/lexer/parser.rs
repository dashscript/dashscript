use crate::common::fsize;

#[derive(Debug, Clone, PartialEq)]
pub enum AssignmentOp {
    Assign,
    Add,
    Sub
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
    Null,
    Boolean(bool),
    Number(fsize),
    String(String),
    Word(String),
    Keyword(String),
    Attribute(String),
    Punc(char),
    Arithmetic(String),
    AssignmentOperator(AssignmentOp),
    LogicalOperator(String)
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

#[derive(Copy, Clone, Default, Debug)]
pub struct Position {
    pub start: usize,
    pub end: usize
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

impl std::default::Default for TokenType {
    fn default() -> TokenType {
        TokenType::Null
    }
}

impl Position {

    pub fn new(lexer: &Lexer) -> Self {
        Position {
            start: lexer.ci,
            end: lexer.ci
        }
    }

    pub fn update(&mut self, lexer: &Lexer) -> Self { 
        self.end = lexer.ci; 
        self.clone()
    }

}

impl Lexer {

    pub fn new(filename: &String, body: &String) -> Result<Lexer, String> {
        let chars = body.chars().collect();
        let mut lexer = Lexer {
            filename: filename.to_string(),
            body: body.to_string(),
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

    pub fn parse(&mut self) -> Result<(), String> {
        self.len = self.body.len();

        while self.ci < self.len {
            let char = self.chars[self.ci];
    
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
                    _ => return Err(create_syntax_error(self, Position::new(self), "dserror(1): No unwanted syntax."))
                }
            }

            self.ci += 1;
        }

        Ok(())
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.last_line_ci = self.ci;
    }

}

pub fn create_syntax_error(lexer: &Lexer, pos: Position, reason: &str) -> String {
    let pref: String = format!("    {}:{}:{} -> ", lexer.filename, lexer.line, lexer.get_col());
    let body = lexer.body[pos.start as usize..lexer.ci as usize].to_string();
    format!("SyntaxError: {}\n{}{}\n{}{}", reason, pref, body, " ".repeat(pref.len()), "^".repeat(body.len()))
}