use crate::lexer::parser::{ TokenType, Position, Token };
use super::main::{ AST, ASTError };
use super::types::{ Statement, StatementType, Identifier };

impl AST {

    pub fn get_value(&mut self, err: &str) -> Result<Identifier, ASTError> {
        let mut res = Identifier::default();
        let mut has_parsed = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();
            let res_is_null = res.is_null();

            match &token.val {
                TokenType::String(str) if res_is_null => {
                    self.ci += 1;
                    res = self.get_identifier_as_word(Identifier::String(str.clone()))?;
                },
                TokenType::Word(word) if res_is_null => {
                    self.ci += 1;
                    res = self.get_identifier_as_word(Identifier::Word(word.to_string()))?;
                },
                TokenType::Number(num) if res_is_null => res = Identifier::Number(num.clone()),
                TokenType::Punc('[') if res_is_null => {
                    self.ci += 1;
                    res = self.get_array()?;
                },
                TokenType::Punc('{') if res_is_null => {
                    self.ci += 1;
                    res = self.get_dict()?;
                },
                TokenType::Punc('(') if res_is_null => {
                    res = self.get_parenthesis()?;
                },
                TokenType::Keyword(keyword) if res_is_null => match keyword.as_str() {
                    "func" => {
                        res = Identifier::Func(
                            "anonymous".to_string(),
                            self.get_function_params()?,
                            {
                                self.ci += 1;
                                self.get_function_body()?
                            }
                        );

                        self.ci -= 1;
                    },
                    "async" => {
                        self.ci += 1;
                        match self.tokens.get(self.ci) {
                            Some(Token {
                                val: TokenType::Keyword(keyword),
                                pos: _
                            }) if keyword.as_str() == "func" => (),
                            _ => return Err(self.create_error(self.tokens[self.ci].pos, "dserror(31): Expected `func` keyword after `async` keyword."))
                        }

                        res = Identifier::AsyncFunc(
                            "anonymous".to_string(),
                            self.get_function_params()?,
                            {
                                self.ci += 1;
                                self.get_function_body()?
                            }
                        );

                        self.ci -= 1;
                    },
                    "await" => res = Identifier::Await(Box::new(self.get_value("dserror(32): Expected a value to await.")?)),
                    _ => return Err(self.create_error(token.pos, "dserror(27): Illegal keyword to use as a value."))
                },
                TokenType::Arithmetic(char) => {
                    self.ci += 1;
                    let next = self.get_value("dserror(13): Improper arithmetic equation.")?;
                    self.ci += 1;

                    match char.as_str() {
                        "+" => res = Identifier::Add(Box::new(res), Box::new(next)),
                        "-" => res = Identifier::Subtract(Box::new(res), Box::new(next)),
                        "*" => res = Identifier::Multiply(Box::new(res), Box::new(next)),
                        "/" => res = Identifier::Divide(Box::new(res), Box::new(next)),
                        "^" => res = Identifier::Pow(Box::new(res), Box::new(next)),
                        _ => ()
                    }

                    if self.current_token().val == TokenType::Punc(';') { continue }
                },
                TokenType::LogicalOperator(op) => {
                    match op.as_str() {
                        "||" => {
                            self.ci += 1;
                            res = Identifier::Or(
                                Box::new(res), 
                                Box::new(self.get_value("dserror(28): Expected value after `or` logical operator.")?)
                            );

                            self.ci += 1;
                        },
                        "&&" => {
                            self.ci += 1;
                            res = Identifier::And(
                                Box::new(res), 
                                Box::new(self.get_value("dserror(28): Expected value after `and` logical operator.")?)
                            );

                            self.ci += 1;
                        },
                        "!" if res_is_null => {
                            self.ci += 1;
                            res = Identifier::Invert(
                                Box::new(self.get_value("dserror(7): Expected an value before termination of the statement.")?)
                            );

                            self.ci += 1;
                        },
                        "==" | "!=" | "<=" | ">=" | "<" | ">" => {
                            self.ci += 1;
                            res = Identifier::Condition(
                                Box::new(res),
                                op.to_string(),
                                Box::new(self.get_value("dserror(7): Expected an value before termination of the statement.")?)
                            );

                            self.ci += 1;
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                TokenType::Punc(';') => break,
                TokenType::Punc(',') | TokenType::Punc(')') | TokenType::Punc(']') | TokenType::Punc('}') | TokenType::Punc(':') if has_parsed => { self.ci -= 1; break },
                TokenType::Null => res = Identifier::Null,
                TokenType::Boolean(bool) => res = Identifier::Boolean(*bool),
                TokenType::Punc('?') if has_parsed => {
                    self.ci += 1;
                    let truthy = self.get_value("dserror(33): Improper ternary operator.")?;
                    self.ci += 2;

                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::Punc(':'),
                            pos: _
                        }) => (),
                        _ => return Err(self.create_error(token.pos, "dserror(33): Improper ternary operator."))
                    }

                    self.ci += 1;
                    let falsy = self.get_value("dserror(33): Improper ternary operator.")?;
                    self.ci += 1;
                    res = Identifier::Ternary(
                        Box::new(res),
                        Box::new(truthy),
                        Box::new(falsy)
                    );
                },
                _ => {
                    println!("{:#?} {:?}", token, res);
                    return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."));
                }
            }

            has_parsed = true;
            self.ci += 1;
        }

        if !has_parsed { self.create_error(self.tokens[self.ci - 1].pos, err); }
        self.ci -= 1;
        Ok(res)
    }

    pub fn get_value_as_stmt(&mut self) -> Result<Statement, ASTError> {
        Ok(Statement {
            val: StatementType::Primary(self.get_value("dserror(14): Unexpected identifier")?),
            pos: self.tokens[if self.ci >= self.len { self.ci - 1 } else { self.ci }].pos
        })
    }

    pub fn get_identifier_as_word(&mut self, word: Identifier) -> Result<Identifier, ASTError> {
        let mut res = word;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('.') => {
                    let wrd = self.next_word_as_str("dserror(2): Found an improper attribute declaration.")?;
                    res = Identifier::Attribute(Box::new(res), Box::new(Identifier::String(wrd)));
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let index = self.get_value("dserror(15): Improper indexing for the object.")?;
                    self.ci += 2;
                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::Punc(']'),
                            pos: _
                        }) => (),
                        _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object."))
                    }

                    res = Identifier::Attribute(
                        Box::new(res), 
                        Box::new(index)
                    );
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    let params = self.get_call_params()?;
                    res = Identifier::Call(Box::new(res), params);
                },
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }

        Ok(res)
    }

    pub fn get_sub_body(&mut self) -> Result<Vec<Statement>, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('{'),
                pos: _
            }) => self.ci += 1,
            _ => return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(30): Expected a start of a body."))
        }

        let mut statements: Vec<Statement> = Vec::new();
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('}') => return Ok(statements),
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos)?;
                    statements.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_word_statement(word.to_string(), token.pos)?;
                    statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.get_value_as_stmt()?;
                    statements.push(stmt);
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(35): Unexpected end of body."))
    }

    pub fn get_word_statement(&mut self, name: String, pos: Position) -> Result<Statement, ASTError> {
        let mut res = Identifier::Word(name);

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('.') => {
                    let wrd = self.next_word_as_str("dserror(2): Found an improper attribute declaration.")?;
                    res = Identifier::Attribute(Box::new(res), Box::new(Identifier::String(wrd)));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    let params = self.get_call_params()?;
                    res = Identifier::Call(Box::new(res), params);
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let index = self.get_value("dserror(15): Improper indexing for the object.")?;
                    self.ci += 1;

                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::Punc(']'),
                            pos: _
                        }) => (),
                        _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object."))
                    }

                    res = Identifier::Attribute(Box::new(res),  Box::new(index));
                },
                TokenType::AssignmentOperator(op) => {
                    self.ci += 1;
                    let stmt = Statement {
                        val: StatementType::Assign(res, op.clone(), self.get_value("dserror(7): Expected an value before termination of the statement.")?),
                        pos
                    };

                    self.ci += 1;
                    return Ok(stmt);
                },
                TokenType::Punc(';') => break,
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }
                
        Ok(Statement { val: StatementType::Primary(res), pos })
    }

}