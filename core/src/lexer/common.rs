use crate::{
    Lexer, Token, TokenKind, Position, TinyString, LexerErrorKind
};

impl Lexer {

    pub fn parse_string(&mut self, pref: char) -> Token {
        let mut content = String::new();
        let mut start = Position::new(self);

        while let Some(&character) = self.chars.get(self.index) {
            if character == pref {
                return Token {
                    kind: TokenKind::String(TinyString::new(content.as_bytes())), 
                    position: start.update(self)
                };
            } else if character == '\\' {
                content += &Self::escape_char(self.chars[self.index + 1]).to_string();
                self.index += 1;
            } else {
                content += &character.to_string();
            }

            self.index += 1;
        }

        Token {
            kind: TokenKind::Error(LexerErrorKind::UnterminatedString),
            position: start.update(self)
        }
    }

    pub fn parse_comment(&mut self) {
        while let Some(&character) = self.chars.get(self.index) {
            if character == '\n' {
                self.next_line();
                break;
            }

            self.index += 1;
        }
    }

    pub fn parse_word(&mut self, initial_character: char) -> Token {
        let mut content = String::from(initial_character);
        let mut start = Position::new(self);

        while let Some(&character) = self.chars.get(self.index) {
            match character {
                'a'..='z' | 'A'..='Z' | '0'..='9' => content += &character.to_string(),
                ' ' => {
                    self.index += 1;
                    return Token {
                        kind: Self::resolve_keyword(content),
                        position: start.update(self)
                    }
                },
                _ => return Token {
                    kind: Self::resolve_kind(content),
                    position: start.update(self)
                }
            }

            self.index += 1;
            
        }

        Token {
            kind: Self::resolve_keyword(content),
            position: start.update(self)
        }
    }

    pub fn parse_op(&mut self, character: char) -> Token {
        let mut start = Position::new(self);

        match self.chars.get(self.index + 1) {
            Some(&next_character) => {
                match next_character {
                    '+' | '-' | '=' | '/' | '*' | '^' | '|' | '&' | '<' | '>' => {
                        self.index += 1;

                        Token {
                            kind: match std::str::from_utf8(&[character as u8, next_character as u8]).unwrap() {
                                "==" => TokenKind::Equal,
                                "!=" => TokenKind::NotEqual,
                                ">=" => TokenKind::GreaterThanOrEqual,
                                "<=" => TokenKind::LessThanOrEqual,
                                "&&" => TokenKind::And,
                                "||" => TokenKind::Or,
                                "+=" => TokenKind::AssignAdd,
                                "-=" => TokenKind::AssignSub,
                                "<<" => TokenKind::Shl,
                                ">>" => TokenKind::Shr,
                                _ => TokenKind::Error(LexerErrorKind::UnexpectedCharacter { character: next_character })
                            },
                            position: start.update(self)
                        }
                    },
                    _ => {
                        Token {
                            kind: match character {
                                '-' => {
                                    if next_character.is_numeric() {
                                        self.index += 2;
                                        return self.parse_number("-".to_string() + &next_character.to_string());
                                    } else { TokenKind::Sub }
                                }
                                '+' => TokenKind::Add,
                                '=' => TokenKind::Assign,
                                '!' => TokenKind::Not,
                                '>' => TokenKind::GreaterThan,
                                '<' => TokenKind::LessThan,
                                '|' => TokenKind::BitOr,
                                '&' => TokenKind::BitAnd,
                                _ => TokenKind::Error(LexerErrorKind::UnexpectedCharacter { character })
                            },
                            position: start.update(self)
                        }
                    }
                }
            },
            None => Token {
                kind: TokenKind::Error(LexerErrorKind::UnexpectedCharacter { character }),
                position: start.update(self)
            }
        }
    }

    pub fn parse_number(&mut self, mut content: String) -> Token {
        let mut start = Position::new(self);
        let mut is_float = false;

        while let Some(&character) = self.chars.get(self.index) {
            match character {
                '0'..='9' => content.push(character),
                '.' if content.find('.').is_none() => {
                    content.push(character);
                    is_float = true;
                },
                'e' => {
                    macro_rules! next_char {
                        () => {{
                            self.index += 1;
                            if self.index >= self.chars.len() {
                                return Token {
                                    kind: TokenKind::Error(LexerErrorKind::ImproperEExpression),
                                    position: start.update(self)
                                }
                            }
                        }};
                    }

                    let initial_number = content.parse::<f64>().unwrap();
                    next_char!();

                    let op = self.chars[self.index];
                    let mut exponent_number = String::new();
                    next_char!();

                    macro_rules! parse_e_expression {
                        () => {
                            Token {
                                kind: match op {
                                    '+' => TokenKind::Int(initial_number as isize * 10_isize.pow(exponent_number.parse().unwrap())),
                                    // The method is here made through string formatting
                                    // because the number loses its precision
                                    '-' => {
                                        let factoral = match content.split('.').collect::<Vec<&str>>().get(1) {
                                            Some(x) => x.to_string(),
                                            None => String::new()
                                        };
    
                                        TokenKind::Float(
                                            format!("0.{}{}{}", "0".repeat(initial_number as usize - factoral.len()), initial_number as u32, factoral)
                                                .parse::<f64>()
                                                .unwrap()
                                        )
                                    },
                                    _ => TokenKind::Int(0)
                                },
                                position: start.update(self)
                            }
                        };
                    }

                    // Parsing the number after the symbol
                    while let Some(&character) = self.chars.get(self.index) {
                        if character >= '0' && character <= '9' {
                            exponent_number.push(character);
                        } else {
                            self.index -= 1;
                            return parse_e_expression!();
                        }
    
                        self.index += 1;
                    }

                    return {
                        if exponent_number.len() == 0 {
                            return Token {
                                kind: TokenKind::Error(LexerErrorKind::ImproperEExpression),
                                position: start.update(self)
                            }
                        } else {
                            parse_e_expression!()
                        }
                    };
                },
                'd' if content == "0".to_owned() => {
                    self.index += 1;
                    content.clear();

                    while let Some(&character) = self.chars.get(self.index) {
                        if character.is_numeric() {
                            content.push(character);
                        } else {
                            self.index -= 1;
                            return Token {
                                kind: TokenKind::Int(content.parse().unwrap_or_default()),
                                position: start.update(self)
                            }
                        }

                        self.index += 1;
                    }

                    return Token {
                        kind: TokenKind::Int(content.parse().unwrap_or_default()),
                        position: start.update(self)
                    };
                },
                'x' if content == "0".to_owned() => {
                    self.index += 1;
                    content.clear();

                    while let Some(&character) = self.chars.get(self.index) {
                        if character.is_ascii_hexdigit() {
                            content.push(character);
                        } else {
                            self.index -= 1;
                            return Token {
                                kind: TokenKind::Int(isize::from_str_radix(&content, 16).unwrap_or_default()),
                                position: start.update(self)
                            };
                        }

                        self.index += 1;
                    }

                    return Token {
                        kind: TokenKind::Int(isize::from_str_radix(&content, 16).unwrap_or_default()),
                        position: start.update(self)
                    };
                },
                'o' if content == "0".to_owned() => {
                    self.index += 1;
                    content.clear();

                    while let Some(&character) = self.chars.get(self.index) {
                        if matches!(character, '0'..='8') {
                            content.push(character);
                        } else {
                            self.index -= 1;
                            return Token {
                                kind: TokenKind::Int(isize::from_str_radix(&content, 8).unwrap_or_default()),
                                position: start.update(self)
                            };
                        }

                        self.index += 1;
                    }

                    return Token {
                        kind: TokenKind::Int(isize::from_str_radix(&content, 8).unwrap_or_default()),
                        position: start.update(self)
                    };
                },
                'b' if content == "0".to_owned() => {
                    self.index += 1;
                    content.clear();

                    while let Some(&character) = self.chars.get(self.index) {
                        if character == '0' || character == '1' {
                            content.push(character);
                        } else {
                            self.index -= 1;
                            return Token {
                                kind: TokenKind::Int(isize::from_str_radix(&content, 2).unwrap_or_default()),
                                position: start.update(self)
                            };
                        }

                        self.index += 1;
                    }

                    return Token {
                        kind: TokenKind::Int(isize::from_str_radix(&content, 2).unwrap_or_default()),
                        position: start.update(self)
                    };
                },
                _ => {
                    self.index -= 1;
                    return Token {
                        kind: if is_float {
                            TokenKind::Float(content.parse().unwrap())
                        } else {
                            TokenKind::Int(content.parse().unwrap())
                        },
                        position: start.update(self)
                    };
                }
            }

            self.index += 1;
        }

        Token {
            kind: if is_float {
                TokenKind::Float(content.parse().unwrap())
            } else {
                TokenKind::Int(content.parse().unwrap())
            },
            position: start.update(self)
        }
    }

}