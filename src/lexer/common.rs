use crate::parser::{Lexer, Token, TokenType, Position, AssignmentOperator, LogicalOperator, LexerError};
use crate::util::{get_word_token_type, resolve_escape_char, check_word_for_keywords};

#[allow(non_camel_case_types)]
#[cfg(target_pointer_width = "64")]
pub type fsize = f64;

#[cfg(not(target_pointer_width = "64"))]
pub type fsize = f32;

impl Lexer {

    pub fn parse_string(&mut self, pref: char) -> Result<Token, LexerError> {
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

        Err(LexerError::new(self, start, "dserror(8): Detected an unterminated string."))
    }

    pub fn parse_comment(&mut self) {
        while self.ci < self.len {
            let char = self.chars[self.ci];

            if char == '\n' {
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

            if (char >= 'a' && char <= 'z') || (char >= 'A' && char <= 'Z') || (char >= '0' && char <= '9') || char == '_' {
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

    pub fn parse_op(&mut self, char: char) -> Result<(), LexerError> {
        let mut start = Position::new(self);

        match self.chars.get(self.ci + 1) {
            Some(ch) => match ch {
                '+' | '-' | '=' | '/' | '*' | '^' | '|' | '&' => {
                    self.ci += 1;
                    let content = char.to_string() + &ch.to_string();
                    self.tokens.push(
                        Token::new(
                            match &content as &str {
                                "==" => TokenType::LogicalOperator(LogicalOperator::Equal),
                                "!=" => TokenType::LogicalOperator(LogicalOperator::NotEqual),
                                ">=" => TokenType::LogicalOperator(LogicalOperator::GreaterThanOrEqual),
                                "<=" => TokenType::LogicalOperator(LogicalOperator::LessThanOrEqual),
                                "&&" => TokenType::LogicalOperator(LogicalOperator::And),
                                "||" => TokenType::LogicalOperator(LogicalOperator::Or),
                                "+=" => TokenType::AssignmentOperator(AssignmentOperator::Add),
                                "-=" => TokenType::AssignmentOperator(AssignmentOperator::Sub),
                                "//" => return Ok(self.parse_comment()),
                                _ => return Err(LexerError::new(self, start, "dserror(11): Unknown kind of operator."))
                            }, start.update(self)
                        )
                    );
                },
                _ => {
                    let token = Token::new(
                        match char {
                            '-' => {
                                if ch.is_numeric() {
                                    self.ci += 2;
                                    #[allow(mutable_borrow_reservation_conflict)]
                                    let num = self.parse_number("-".to_string() + &ch.to_string())?;
                                    self.tokens.push(num);
                                    return Ok(());
                                } else { TokenType::Arithmetic(char) }
                            }
                            '+' | '/' | '*' | '^' | '%' => TokenType::Arithmetic(char),
                            '=' => TokenType::AssignmentOperator(AssignmentOperator::Assign),
                            '!' => TokenType::LogicalOperator(LogicalOperator::Invert),
                            '>' => TokenType::LogicalOperator(LogicalOperator::GreaterThan),
                            '<' => TokenType::LogicalOperator(LogicalOperator::LessThan),
                            _ => return Err(LexerError::new(self, start, "dserror(11): Unknown kind of operator."))
                        }, start.update(self)
                    );

                    self.tokens.push(token);
                }
            },
            None => return Err(LexerError::new(
                self, start, 
                &format!("dserror(41): Unexpected operator '{}' at eof.", char) as &str
            ))
        }

        Ok(())
    }

    pub fn parse_number(&mut self, pref: String) -> Result<Token, LexerError> {
        let mut content = String::from(pref);
        let mut start = Position::new(self);
        let mut is_float = false;

        while self.ci < self.len {
            let char = self.chars[self.ci];

            if char >= '0' && char <= '9' {
                content += &char.to_string();
            } else if char == '.' && content.find('.').is_none() {
                content +=  &char.to_string();
                is_float = true;
            // TODO(Scientific-Guy): Make a better exponent expression parser.
            } else if char == 'e' {
                let initial_number: fsize = content.parse().unwrap();
                self.ci += 1;
                if self.ci >= self.len {
                    return Err(LexerError::new(self, start, "dserror(40) Expected arithmetic operator after `e` keyword"))
                }

                let op = self.chars[self.ci];
                let mut exponent_number = String::new();
                self.ci += 1;
                if self.ci >= self.len {
                    return Err(LexerError::new(self, start, "dserror(40) Expected number after `e` keyword"))
                }

                macro_rules! parse_e_expression {
                    () => {
                        Ok(Token::new(
                            match op {
                                '+' => if is_float {
                                    TokenType::Float(initial_number * 10_usize.pow(exponent_number.parse().unwrap()) as fsize)
                                } else {
                                    TokenType::Int(initial_number as isize * 10_isize.pow(exponent_number.parse().unwrap()))
                                },
                                // The method is here made through string formatting
                                // because the number loses its precision
                                '-' => {
                                    let factoral = match content.split('.').collect::<Vec<&str>>().get(1) {
                                        Some(x) => x.to_string(),
                                        None => String::new()
                                    };

                                    TokenType::Float(
                                        format!("0.{}{}{}", "0".repeat(initial_number as usize - factoral.len()), initial_number as u32, factoral)
                                            .parse::<fsize>()
                                            .unwrap()
                                    )
                                },
                                _ => TokenType::Int(0)
                            },
                            start.update(self)
                        ))
                    };
                }

                while self.ci < self.len {
                    let char = self.chars[self.ci];

                    if char >= '0' && char <= '9' {
                        exponent_number += &char.to_string()
                    } else {
                        self.ci -= 1;
                        return parse_e_expression!();
                    }

                    self.ci += 1;
                }

                return if exponent_number.len() == 0 {
                    Err(LexerError::new(self, start, "dserror(40) Expected number after `e` keyword"))
                } else {
                    parse_e_expression!()
                }
            } else {
                self.ci -= 1;
                return Ok(
                    Token::new(
                        if is_float {
                            TokenType::Float(content.parse().unwrap())
                        } else {
                            TokenType::Int(content.parse().unwrap())
                        },
                        start.update(self)
                    )
                )
            }

            self.ci += 1;
        }

        Ok(
            Token::new(
                if is_float {
                    TokenType::Float(content.parse().unwrap())
                } else {
                    TokenType::Int(content.parse().unwrap())
                },
                start.update(self)
            )
        )
    }

}