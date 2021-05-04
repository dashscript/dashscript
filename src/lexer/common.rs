use crate::common::fsize;
use super::{
    parser::{
        Lexer,
        Token,
        TokenType,
        Position,
        AssignmentOp,
        create_syntax_error
    },
    util::{
        get_word_token_type,
        resolve_escape_char,
        check_word_for_keywords
    }
};

impl Lexer {

    pub fn parse_string(&mut self, pref: char) -> Result<Token, String> {
        let mut content = String::new();
        let mut start = Position::new(self);

        while self.ci < self.len {
            let char = self.chars[self.ci];

            if char == pref {
                return Ok(Token::new(TokenType::String(content), start.update(self)));
            } else if char == '\\' {
                content += &resolve_escape_char(*self.chars.get(self.ci + 1).unwrap_or(&'\\')).to_string();
                self.ci += 1;
            } else {
                content += &char.to_string();
            }

            self.ci += 1;
        }

        Err(create_syntax_error(self, start, "dserror(8): Detected an unterminated string."))
    }

    pub fn parse_comment(&mut self) {
        while self.ci < self.len {
            let char = self.chars[self.ci];

            if char == '\n' {
                self.ci += 1;
                self.next_line();
                break;
            }

            self.ci += 1;
        }
    }

    pub fn parse_word(&mut self, char: char) -> Token {
        let mut content = String::from(char);
        let mut start = Position::new(self);

        while self.ci < self.len {
            let char = self.chars[self.ci];

            if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9')|| char == '_' {
                content += &char.to_string();
            } else if char == ' ' {
                self.ci += 1;
                return Token::new(check_word_for_keywords(content), start.update(self));
            } else {
                return Token::new(get_word_token_type(content), start.update(self));
            }

            self.ci += 1;
            
        }

        return Token::new(get_word_token_type(content), start.update(self));
    }

    pub fn parse_op(&mut self, char: char) -> Result<(), String> {
        let mut content = String::from(char);
        let mut start = Position::new(self);

        match self.chars.get(self.ci + 1) {
            Some(ch) => match ch {
                '+' | '-' | '=' | '/' | '*' | '^' | '|' | '&' => {
                    self.ci += 1;
                    
                    content += &ch.to_string();
                    match content.as_str() {
                        "==" | "!=" | ">=" | "<=" | "&&" | "||" => self.tokens.push(Token::new(TokenType::LogicalOperator(content.to_string()), start.update(self))),
                        "+=" => self.tokens.push(Token::new(TokenType::AssignmentOperator(AssignmentOp::Add), start.update(self))),
                        "-=" => self.tokens.push(Token::new(TokenType::AssignmentOperator(AssignmentOp::Sub), start.update(self))),
                        "//" => self.parse_comment(),
                        _ => return Err(create_syntax_error(self, start, "dserror(11): Unknown kind of operator."))
                    }
                },
                _ => {
                    let token = match char {
                        '+' | '-' | '/' | '*' | '^' => {
                            if char == '-' && ch.is_numeric() {
                                self.ci += 2;
                                #[allow(mutable_borrow_reservation_conflict)]
                                self.parse_number("-".to_string() + &ch.to_string())?
                            } else {
                                Token::new(TokenType::Arithmetic(content), start.update(self))
                            }
                        },
                        '=' => Token::new(TokenType::AssignmentOperator(AssignmentOp::Assign), start.update(self)),
                        '!' | '<' | '>' => Token::new(TokenType::LogicalOperator(char.to_string()), start.update(self)),
                        _ => return Err(create_syntax_error(self, start, "dserror(11): Unknown kind of operator."))
                    };

                    self.tokens.push(token);
                }
            },
            None => self.tokens.push(
                match char {
                    '+' | '-' | '/' | '*' | '^' => Token::new(TokenType::Arithmetic(content), start.update(self)),
                    '=' => Token::new(TokenType::AssignmentOperator(AssignmentOp::Assign), start.update(self)),
                    '!' | '<' | '>' => Token::new(TokenType::LogicalOperator(char.to_string()), start.update(self)),
                    _ => return Err(create_syntax_error(self, start, "dserror(11): Unknown kind of operator."))
                }
            )
        }

        Ok(())
    }

    pub fn parse_number(&mut self, pref: String) -> Result<Token, String> {
        let mut content = String::from(pref);
        let mut start = Position::new(self);

        while self.ci < self.len {
            let char = self.chars[self.ci];

            if (char >= '0' && char <= '9') || (char == '.' && content.find('.') == None) {
                content += &char.to_string();
            } else if char == 'e' {
                let a: fsize = content.parse().unwrap();
                self.ci += 1;
                if self.ci >= self.len {
                    return Err(create_syntax_error(self, start, "dserror(40) Expected arithmetic operator after `e` keyword"))
                }

                let op = self.chars[self.ci];
                let mut b = String::new();
                self.ci += 1;
                if self.ci >= self.len {
                    return Err(create_syntax_error(self, start, "dserror(40) Expected number after `e` keyword"))
                }

                while self.ci < self.len {
                    let char = self.chars[self.ci];

                    if char >= '0' && char <= '9' {
                        b += &char.to_string()
                    } else {
                        self.ci -= 1;
                        return Ok(Token::new(
                            TokenType::Number(match op {
                                '+' => a * (10.0 as fsize).powf(b.parse::<fsize>().unwrap()),
                                '-' => {
                                    let factoral = match content.split('.').collect::<Vec<&str>>().get(1) {
                                        Some(&x) => x.to_string(),
                                        None => String::new()
                                    };

                                    format!("0.{}{}{}", "0".repeat(a as usize - factoral.len()), a as u32, factoral).parse::<fsize>().unwrap()
                                },
                                _ => 0.0
                            }),
                            start
                        ));
                    }

                    self.ci += 1;
                }

                match b.parse::<fsize>() {
                    Ok(val) => {
                        match op {
                            '+' => a * (10.0 as fsize).powf(val),
                            '-' => {
                                let factoral = match content.split('.').collect::<Vec<&str>>().get(1) {
                                    Some(&x) => x.to_string(),
                                    None => String::new()
                                };

                                format!("0.{}{}{}", "0".repeat(a as usize - factoral.len()), a as u32, factoral).parse::<fsize>().unwrap()
                            },
                            _ => 0.0
                        };
                    },
                    _ => return Err(create_syntax_error(self, start, "dserror(40) Expected number after `e` keyword"))
                }
            } else {
                self.ci -= 1;
                return Ok(Token::new(TokenType::Number(content.parse().unwrap()), start.update(self)));
            }

            self.ci += 1;
        }

        Ok(Token::new(TokenType::Number(content.parse().unwrap()), start.update(self)))
    }

}