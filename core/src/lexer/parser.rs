use crate::{Position, Token, TokenKind};

#[derive(Clone, Default, Debug)]
pub struct Lexer {
    pub index: usize,
    pub filename: String,
    pub body: String,
    pub chars: Vec<char>,
    line: usize,
    last_line_index: usize,
}

impl Lexer {

    pub fn new(filename: &str, body: &str) -> Self {
        let chars = body.chars().collect();
        Lexer {
            filename: filename.to_owned(),
            body: body.to_owned(),
            chars,
            line: 1,
            ..Default::default()
        }
    }

    pub fn col(&self) -> usize {
        self.index - self.last_line_index
    }

    pub fn pos(&self) -> (usize, usize) {
        (self.line, self.index - self.last_line_index)
    }

    pub fn next_line(&mut self) {
        self.line += 1;
        self.last_line_index = self.index;
    }

    pub fn next_char(&mut self) -> Option<&char> {
        let character = self.chars.get(self.index);
        self.index += 1;
        character
    }

}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        macro_rules! kind {
            ($kind:ident) => {{
                self.index += 1;
                return Some(Token {
                    kind: TokenKind::$kind,
                    position: Position::new(self)
                });
            }};
        }

        if let Some(&character) = self.chars.get(self.index) {
            if (character >= 'a' && character <= 'z') || (character >= 'A' && character <= 'Z') || character == '_' {
                self.index += 1;
                return Some(self.parse_word(character));
            } else if character >= '0' && character <= '9' {
                self.index += 1;
                let num = self.parse_number(character.to_string());
                self.index += 1;
                return Some(num);
            } else {
                match character {
                    '\'' | '"' => {
                        self.index += 1;
                        let string = self.parse_string(character);
                        self.index += 1;
                        return Some(string);
                    },
                    '+' | '-' | '=' | '!' | '|' | '<' | '>' | '&' => {
                        let op = self.parse_op(character);
                        self.index += 1;
                        return Some(op);
                    },
                    '\r' | ' ' => {
                        self.index += 1;
                        return self.next();
                    },
                    '\n' => {
                        self.index += 1;
                        self.next_line();
                        return self.next();
                    },
                    '*' => kind!(Mul),
                    '/' => kind!(Div),
                    '^' => kind!(BitXor),
                    ':' => kind!(Colon),
                    '%' => kind!(Rem),
                    ';' => kind!(Semicolon),
                    '.' => kind!(Dot),
                    ',' => kind!(Comma),
                    '(' => kind!(ParenOpen),
                    ')' => kind!(ParenClose),
                    '[' => kind!(SqBraceOpen),
                    ']' => kind!(SqBraceClose),
                    '{' => kind!(CurlyBraceOpen), 
                    '}' => kind!(CurlyBraceClose),
                    '?' => kind!(Question),
                    '#' => {
                        self.parse_comment();
                        return self.next();
                    },
                    character => {
                        self.index += 1;
                        return Some(Token {
                            kind: TokenKind::Error(LexerErrorKind::UnexpectedCharacter { character }),
                            position: Position::new(self)
                        });
                    }
                }
            }
        }

        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LexerErrorKind {
    UnexpectedCharacter { character: char },
    UnterminatedString,
    ImproperEExpression
}

impl ToString for LexerErrorKind {
    fn to_string(&self) -> String {
        match self {
            Self::UnexpectedCharacter { character } => format!("Character {} was never expected.", character),
            Self::UnterminatedString => "Found an unterminated string.".to_owned(),
            Self::ImproperEExpression => "Improper 'e' expression.".to_owned()
        }
    }
}