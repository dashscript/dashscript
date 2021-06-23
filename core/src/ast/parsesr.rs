use crate::{
    Token, TokenType, Keyword, AssignmentOperator, LogicalOperator, Position,
    AST, ASTError, Expr, Stmt, StmtType, BinaryOperator, FuncParam, FunctionExpr
};

pub fn keyword_class(&mut self, index: usize) -> Stmt {
    let name = match self.next_token().kind {
        TokenKind::Word(name) => self.constant_pool.add_string(name),
        _ => {
            unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
            return Stmt::default();
        }
    };

    let extends = match self.next_token().kind {
        TokenKind::LessThanOrEqual => {
            match (self.next_token().kind, self.next_token().kind) {
                (TokenKind::Word(extends), TokenKind::CurlyBraceOpen) => Some(self.constant_pool.add_string(extends)),
                _ => {
                    unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                    return Stmt::default();
                }
            }
        },
        TokenKind::CurlyBraceOpen => None,
        _ => {
            unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
            return Stmt::default();
        }
    };

    let mut token = self.lexer.next().unwrap();
    let mut properties = Vec::new();
    let mut static_properties = Vec::new();

    loop {
        // There might be a chance which can cause many errors
        // due to the original first one. This is used to prevent
        // the overflow of duplicate chained errors.
        if self.had_error {
            return Stmt::default();
        }

        match token.kind {
            TokenKind::CurlyBraceClose => return Stmt {
                expr: Expr::Class { name, extends, properties, static_properties },
                index
            },
            TokenKind::Keyword(Keyword::Const) => {
                let name = match (self.next_token().kind, self.next_token().kind) {
                    (TokenKind::Word(name), TokenKind::Assign) => self.constant_pool.add_string(name),
                    _ => {
                        unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                        return Stmt::default();
                    }
                };

                properties.push((name, self.expression(ASTErrorKind::UnexpectedExpr), true));
                self.validate_current_semicolon();
            },
            TokenKind::Keyword(Keyword::Let) => {
                let name = match (self.next_token().kind, self.next_token().kind) {
                    (TokenKind::Word(name), TokenKind::Assign) => self.constant_pool.add_string(name),
                    _ => {
                        unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                        return Stmt::default();
                    }
                };

                properties.push((name, self.expression(ASTErrorKind::UnexpectedExpr), false));
                println!("{:?}", properties);
                self.validate_current_semicolon();
            },
            TokenKind::Keyword(Keyword::Func) => {
                let (name, parameters) = match self.next_token().kind {
                    TokenKind::Word(name) => {
                        match self.next_token().kind {
                            TokenKind::ParenOpen => (self.constant_pool.add_string(name), self.expression_function_params()),
                            _ => {
                                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                                return Stmt::default();
                            }
                        }
                    },
                    _ => {
                        unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                        return Stmt::default();
                    }
                };

                properties.push((name, Expr::Function {
                    name: constant_pool::ANONYMOUS_CONSTANT,
                    parameters,
                    is_async: false,
                    inner: self.expression_block()
                }, true));
            },
            TokenKind::Keyword(Keyword::Static) => {
                match self.next_token().kind {
                    TokenKind::Keyword(Keyword::Const) => {
                        let name = match (self.next_token().kind, self.next_token().kind) {
                            (TokenKind::Word(name), TokenKind::Assign) => self.constant_pool.add_string(name),
                            _ => {
                                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                                return Stmt::default();
                            }
                        };
    
                        static_properties.push((name, self.expression(ASTErrorKind::UnexpectedExpr), true));
                        self.validate_current_semicolon();
                    },
                    TokenKind::Keyword(Keyword::Let) => {
                        let name = match (self.next_token().kind, self.next_token().kind) {
                            (TokenKind::Word(name), TokenKind::Assign) => self.constant_pool.add_string(name),
                            _ => {
                                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                                return Stmt::default();
                            }
                        };
    
                        static_properties.push((name, self.expression(ASTErrorKind::UnexpectedExpr), false));
                        self.validate_current_semicolon();
                    },
                    TokenKind::Keyword(Keyword::Func) => {
                        let (name, parameters) = match self.next_token().kind {
                            TokenKind::Word(name) => {
                                match self.next_token().kind {
                                    TokenKind::ParenOpen => (self.constant_pool.add_string(name), self.expression_function_params()),
                                    _ => {
                                        unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                                        return Stmt::default();
                                    }
                                }
                            },
                            _ => {
                                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                                return Stmt::default();
                            }
                        };
    
                        static_properties.push((name, Expr::Function {
                            name: constant_pool::ANONYMOUS_CONSTANT,
                            parameters,
                            is_async: false,
                            inner: self.expression_block()
                        }, true));
                    },
                    _ => {
                        unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current);
                        return Stmt::default();
                    }
                }
            },
            TokenKind::Error(error) => self.error(token.position, ASTErrorKind::LexerError(error)),
            _ => {
                unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current);
                return Stmt::default();
            }
        }

        match self.lexer.next() {
            Some(next_token) => {
                token = next_token;
                self.current = token.clone();
            },
            None => break
        }
    }

    self.error(self.current.position, ASTErrorKind::UnexpectedEof);
    Stmt::default()
}

match token.kind {
    TokenType::Word(name) if is_null => {
        self.ci += 1;
        let index = self.constant_pool.add_string(name);
        result = self.parse_expr_as_object(Expr::Word(index))?;
    },
    TokenType::String(string) if is_null => {
        self.ci += 1;
        let index = self.constant_pool.add_string(string);
        result = self.parse_expr_as_object(Expr::String(index))?
    },
    TokenType::Int(int) if is_null => result = Expr::Int(self.constant_pool.add_int(*int)),
    TokenType::Float(float) if is_null => result = Expr::Float(self.constant_pool.add_float(*float)),
    TokenType::Boolean(bool) if is_null => result = Expr::Boolean(*bool),
    TokenType::Punc('[') if is_null => {
        self.ci += 1;
        result = self.parse_array()?;
    },
    TokenType::Punc('{') if is_null => {
        self.ci += 1;
        result = self.parse_dict()?;
    },
    TokenType::Punc('(') if is_null => {
        self.ci += 1;
        result = self.parse_parenthesis()?;
    },
    TokenType::Arithmetic(char) => {
        self.ci += 1;
        let next = self.parse_value("dserror(13): Improper arithmetic equation.")?;
        match char {
            '+' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Add, Box::new(next)),
            '-' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Subtract, Box::new(next)),
            '*' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Multiply, Box::new(next)),
            '/' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Divide, Box::new(next)),
            '^' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Power, Box::new(next)),
            '%' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Rem, Box::new(next)),
            _ => result = Expr::Null
        };
    },
    TokenType::LogicalOperator(op) => {
        match op {
            LogicalOperator::Or => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::Or,
                    Box::new(self.parse_value("dserror(28): Expected value after `or` logical operator.")?)
                );
            },
            LogicalOperator::And => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::And,
                    Box::new(self.parse_value("dserror(28): Expected value after `and` logical operator.")?)
                );
            },
            LogicalOperator::Invert if is_null => {
                self.ci += 1;
                result = Expr::Not(
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::Equal => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::Equal,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::NotEqual => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::NotEqual,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::GreaterThan => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::GreaterThan,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::GreaterThanOrEqual => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::GreaterThanOrEqual,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::LessThan => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::LessThan,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            LogicalOperator::LessThanOrEqual => {
                self.ci += 1;
                result = Expr::BinaryOperation(
                    Box::new(result), 
                    BinaryOperator::LessThanOrEqual,
                    Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                );
            },
            _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
        }
    },
    TokenType::Punc('?') if has_parsed => {
        self.ci += 1;
        let truthy = self.parse_value("dserror(33): Improper ternary operator.")?;
        self.ci += 1;
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(':')) {
            return Err(self.create_error(token.pos, "dserror(33): Improper ternary operator."));
        }

        self.ci += 1;
        let falsy = self.parse_value("dserror(33): Improper ternary operator.")?;
        result = Expr::Ternary(Box::new(result), Box::new(truthy), Box::new(falsy));
    },
    TokenType::Keyword(keyword) => {
        match keyword {
            Keyword::Await if is_null => {
                self.ci += 1;
                result = Expr::Await(Box::new(self.parse_value("dserror(32): Expected a value to await.")?));
            },
            Keyword::Func if is_null => {
                self.ci += 1;
                result = Expr::Func(self.parse_function(0, false)?);
            },
            Keyword::Async if is_null => {
                self.ci += 1;
                if !matches_token!(some self.tokens.get(self.ci), TokenType::Keyword(Keyword::Func)) {
                    return Err(self.create_error(token.pos, "dserror(31): Expected `func` keyword after `async` keyword."));
                }

                self.ci += 1;
                result = Expr::Func(self.parse_function(0, true)?);
            },
            _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
        }
    },
    TokenType::Null => result = Expr::Null,
    | TokenType::Punc(';') 
    | TokenType::Punc(',') 
    | TokenType::Punc(')') 
    | TokenType::Punc(']') 
    | TokenType::Punc('}') 
    | TokenType::Punc(':') if has_parsed => { 
        self.ci -= 1; 
        return Ok(result);
    },
    _ => {
        self.error(token.position, ASTErrorKind::UnexpectedExpr);
        return result;
    }
}

impl AST {

    pub fn hparse_value(&mut self, err: &str) -> Result<Expr, ASTError> {
        let mut result = Expr::default();
        let mut has_parsed = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();
            let is_null = match result { 
                Expr::Null => true, 
                _ => false 
            };

            match &token.val {
                TokenType::Word(name) if is_null => {
                    self.ci += 1;
                    let index = self.constant_pool.add_string(name);
                    result = self.parse_expr_as_object(Expr::Word(index))?;
                },
                TokenType::String(string) if is_null => {
                    self.ci += 1;
                    let index = self.constant_pool.add_string(string);
                    result = self.parse_expr_as_object(Expr::String(index))?
                },
                TokenType::Int(int) if is_null => result = Expr::Int(self.constant_pool.add_int(*int)),
                TokenType::Float(float) if is_null => result = Expr::Float(self.constant_pool.add_float(*float)),
                TokenType::Boolean(bool) if is_null => result = Expr::Boolean(*bool),
                TokenType::Punc('[') if is_null => {
                    self.ci += 1;
                    result = self.parse_array()?;
                },
                TokenType::Punc('{') if is_null => {
                    self.ci += 1;
                    result = self.parse_dict()?;
                },
                TokenType::Punc('(') if is_null => {
                    self.ci += 1;
                    result = self.parse_parenthesis()?;
                },
                TokenType::Arithmetic(char) => {
                    self.ci += 1;
                    let next = self.parse_value("dserror(13): Improper arithmetic equation.")?;
                    match char {
                        '+' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Add, Box::new(next)),
                        '-' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Subtract, Box::new(next)),
                        '*' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Multiply, Box::new(next)),
                        '/' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Divide, Box::new(next)),
                        '^' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Power, Box::new(next)),
                        '%' => result = Expr::BinaryOperation(Box::new(result), BinaryOperator::Rem, Box::new(next)),
                        _ => result = Expr::Null
                    };
                },
                TokenType::LogicalOperator(op) => {
                    match op {
                        LogicalOperator::Or => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::Or,
                                Box::new(self.parse_value("dserror(28): Expected value after `or` logical operator.")?)
                            );
                        },
                        LogicalOperator::And => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::And,
                                Box::new(self.parse_value("dserror(28): Expected value after `and` logical operator.")?)
                            );
                        },
                        LogicalOperator::Invert if is_null => {
                            self.ci += 1;
                            result = Expr::Not(
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::Equal => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::Equal,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::NotEqual => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::NotEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::GreaterThan => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::GreaterThan,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::GreaterThanOrEqual => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::GreaterThanOrEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::LessThan => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::LessThan,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        LogicalOperator::LessThanOrEqual => {
                            self.ci += 1;
                            result = Expr::BinaryOperation(
                                Box::new(result), 
                                BinaryOperator::LessThanOrEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?)
                            );
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
                    }
                },
                TokenType::Punc('?') if has_parsed => {
                    self.ci += 1;
                    let truthy = self.parse_value("dserror(33): Improper ternary operator.")?;
                    self.ci += 1;
                    if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(':')) {
                        return Err(self.create_error(token.pos, "dserror(33): Improper ternary operator."));
                    }

                    self.ci += 1;
                    let falsy = self.parse_value("dserror(33): Improper ternary operator.")?;
                    result = Expr::Ternary(Box::new(result), Box::new(truthy), Box::new(falsy));
                },
                TokenType::Keyword(keyword) => {
                    match keyword {
                        Keyword::Await if is_null => {
                            self.ci += 1;
                            result = Expr::Await(Box::new(self.parse_value("dserror(32): Expected a value to await.")?));
                        },
                        Keyword::Func if is_null => {
                            self.ci += 1;
                            result = Expr::Func(self.parse_function(0, false)?);
                        },
                        Keyword::Async if is_null => {
                            self.ci += 1;
                            if !matches_token!(some self.tokens.get(self.ci), TokenType::Keyword(Keyword::Func)) {
                                return Err(self.create_error(token.pos, "dserror(31): Expected `func` keyword after `async` keyword."));
                            }

                            self.ci += 1;
                            result = Expr::Func(self.parse_function(0, true)?);
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
                    }
                },
                TokenType::Null => result = Expr::Null,
                | TokenType::Punc(';') 
                | TokenType::Punc(',') 
                | TokenType::Punc(')') 
                | TokenType::Punc(']') 
                | TokenType::Punc('}') 
                | TokenType::Punc(':') if has_parsed => { 
                    self.ci -= 1; 
                    return Ok(result);
                },
                _ => {
                    println!("{:?}\n{:?} {:?}", self.constant_pool, token, result);
                    return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."));
                }
            }

            has_parsed = true;
            self.ci += 1;
        }

        if !has_parsed { 
            return Err(self.create_error(self.tokens[self.ci - 1].pos, err)); 
        }

        self.ci -= 1;
        Ok(result)
    }

    pub fn parse_function(&mut self, name: u32, is_async: bool) -> Result<FunctionExpr, ASTError> {
        let parameters = self.parse_function_params()?;
        let stmts = self.parse_sub_body()?;
        Ok(FunctionExpr {
            name,
            parameters,
            stmts,
            is_async
        })
    }

    pub fn parse_keyword_statement(&mut self, keyword: &Keyword, pos: Position) -> Result<Stmt, ASTError> {
        macro_rules! consume_token {
            ($err:expr, $pattern:pat) => {{
                match self.tokens.get(self.ci) {
                    Some(Token { val: $pattern, .. }) => self.ci += 1,
                    _ => return Err(self.create_error(self.tokens[self.ci - 2].pos, $err))
                }
            }};
        }

        macro_rules! get_word_as_index {
            ($err:expr) => {{
                self.ci += 1;
                match self.tokens.get(self.ci) {
                    Some(Token { val: TokenType::Word(name), .. }) => {
                        self.ci += 1;
                        self.constant_pool.add_string(name)
                    },
                    _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, $err))
                }
            }};
        }

        match keyword {
            Keyword::Var => {
                let index = get_word_as_index!("dserror(3): Expected a declaration name but found nothing.");
                consume_token!("dserror(4): Expected an assignment operator but found nothing.", TokenType::AssignmentOperator(AssignmentOperator::Assign));

                Ok(Stmt {
                    val: StmtType::Store(index, self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?, false),
                    start_index: pos.start
                })
            },
            Keyword::Const => {
                let index = get_word_as_index!("dserror(3): Expected a declaration name but found nothing.");
                consume_token!("dserror(4): Expected an assignment operator but found nothing.", TokenType::AssignmentOperator(AssignmentOperator::Assign));
                
                Ok(Stmt {
                    val: StmtType::Store(index, self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?, true),
                    start_index: pos.start
                })
            },
            Keyword::Return => {
                self.ci += 1;
                Ok(Stmt {
                    val: StmtType::Return(
                        if is_token!(self, TokenType::Punc(';')) {
                            Expr::Null
                        } else {
                            self.parse_value("dserror(22): Illegal return Stmt.")?
                        }
                    ),
                    start_index: pos.start
                })
            },
            Keyword::If => self.parse_condition_chain(pos),
            Keyword::Func => {
                let index = get_word_as_index!("dserror(24): Missing function name.");
                let function = self.parse_function(index, false)?;
                Ok(Stmt {
                    val: StmtType::Expr(Expr::Func(function)),
                    start_index: pos.start
                })
            },
            Keyword::Async => {
                self.ci += 1;
                if !matches_token!(some self.tokens.get(self.ci), TokenType::Keyword(Keyword::Func)) {
                    return Err(self.create_error(pos, "dserror(31): Expected `func` keyword after `async` keyword."));
                }

                let index = get_word_as_index!("dserror(24): Missing function name.");
                let function = self.parse_function(index, true)?;

                Ok(Stmt {
                    val: StmtType::Expr(Expr::Func(function)),
                    start_index: pos.start
                })
            },
            Keyword::While => self.parse_while_loop(pos),
            Keyword::Break => Ok(Stmt { val: StmtType::Break, start_index: pos.start }),
            Keyword::Continue => Ok(Stmt { val: StmtType::Continue, start_index: pos.start }),
            Keyword::Await => {
                self.ci += 1;
                Ok(Stmt {
                    val: StmtType::Expr(
                        Expr::Await(
                            Box::new(self.parse_value("dserror(32): Expected a value to await.")?)
                        )
                    ),
                    start_index: pos.start
                })
            },
            _ => Ok(Stmt::default())
        }
    }

    pub fn parse_expr_as_object(&mut self, mut ident: Expr) -> Result<Expr, ASTError> {
        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match token.val {
                TokenType::Punc('.') => {
                    self.ci += 1;
                    ident = Expr::Attribute(
                        Box::new(ident),
                        Box::new(
                            Expr::String(
                                match self.tokens.get(self.ci) {
                                    Some(Token { val: TokenType::Word(name), .. }) => self.constant_pool.add_string(name),
                                    _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(2): Found an improper attribute declaration."))
                                }
                            )
                        )
                    );
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let attribute = self.parse_value("dserror(15): Improper indexing for the object.")?;
                    self.ci += 1;
                    if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(']')) {
                        return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object."));
                    }

                    ident = Expr::Attribute(Box::new(ident), Box::new(attribute));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    ident = Expr::Call(Box::new(ident), self.parse_call_params()?);
                },
                _ => {
                    self.ci -= 1;
                    return Ok(ident);
                }
            }

            self.ci += 1;
        }

        Ok(ident)
    }

    pub fn parse_call_params(&mut self) -> Result<Vec<(Expr, bool)>, ASTError> {
        let mut params = Vec::new();
        let mut can_use_comma = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(')') => {
                    return if params.len() > 255 { 
                        Err(self.create_error(token.pos, "dserror(38) A function can have upto 255 parameters only."))
                    } else { Ok(params) }
                },
                TokenType::Punc(',') if can_use_comma => can_use_comma = false,
                TokenType::Punc('.') => {
                    match self.tokens.get(self.ci..self.ci+3) {
                        Some([
                            Token { val: TokenType::Punc('.'), .. },
                            Token { val: TokenType::Punc('.'), .. },
                            Token { val: TokenType::Punc('.'), .. },
                        ]) => {
                            self.ci += 3;
                            params.push((self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?, true));
                            can_use_comma = true;
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
                    }
                },
                _ => {
                    let param = self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?;
                    params.push((param, false));
                    can_use_comma = true;
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
    }

    pub fn parse_array(&mut self) -> Result<Expr, ASTError> {
        let mut items: Vec<Expr> = Vec::new();
        let mut can_use_comma = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc(']') => {
                    self.ci += 1;
                    return self.parse_expr_as_object(Expr::Array(items))
                },
                TokenType::Punc(',') if can_use_comma => can_use_comma = false,
                _ => {
                    items.push(self.parse_value("dserror(10): Detected an unclosed bracket.")?);
                    can_use_comma = true;
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(10): Detected an unclosed bracket."))
    }

    pub fn parse_dict(&mut self) -> Result<Expr, ASTError> {
        let mut attributes = Vec::new();
        let mut can_use_comma = false;

        macro_rules! parse_key_value {
            ($id:expr, $key:expr, $pos:expr, $is_word:expr) => {{
                self.ci += 1;
                match self.tokens.get(self.ci) {
                    Some(Token { val: TokenType::Punc(':'), .. }) => {
                        self.ci += 1;
                        self.parse_value("dserror(20): Dict must have a value or have a direct representation.")?
                    },
                    _ => {
                        if $is_word {
                            self.ci -= 1;
                            Expr::Word($id)
                        } else {
                            return Err(self.create_error($pos, "dserror(19): Expected `:` character to represent the value for the key."));
                        }
                    }
                }
            }};
        }

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('}') => {
                    self.ci += 1;
                    return self.parse_expr_as_object(Expr::Dict(attributes));
                },
                TokenType::String(key) => {
                    if key.len() == 0 { 
                        return Err(self.create_error(token.pos, "dserror(18): Dict keys strings must not be empty."));
                    }

                    let id = self.constant_pool.add_string(&key);
                    attributes.push((id, parse_key_value!(id, key, token.pos, false)));
                    can_use_comma = true;
                },
                TokenType::Word(key) => {
                    let id = self.constant_pool.add_string(&key);
                    attributes.push((id, parse_key_value!(id, key, token.pos, true)));
                    can_use_comma = true;
                },
                TokenType::Punc(',') if can_use_comma => can_use_comma = false,
                _ => return Err(self.create_error(token.pos, "dserror(17): Dict keys must be a string or a word representation."))
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(17): Dict keys must be a string or a word representation."))
    }

    pub fn parse_parenthesis(&mut self) -> Result<Expr, ASTError> {
        let val = Expr::Group(Box::new(self.parse_value("dserror(21): Expected value in a bracket.")?));
        self.ci += 1;

        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(')')) {
            return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
        }

        self.ci += 1;
        self.parse_expr_as_object(val)
    }

    pub fn parse_word_statement(&mut self, name: &String, pos: Position) -> Result<Stmt, ASTError> {
        let constant_id = self.constant_pool.add_string(name);
        let mut result = Expr::Word(constant_id);

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('.') => {
                    self.ci += 1;
                    result = Expr::Attribute(
                        Box::new(result),
                        Box::new(
                            Expr::String(
                                match self.tokens.get(self.ci) {
                                    Some(Token { val: TokenType::Word(name), .. }) => self.constant_pool.add_string(name),
                                    _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(2): Found an improper attribute declaration."))
                                }
                            )
                        )
                    );
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let attribute = self.parse_value("dserror(15): Improper indexing for the object.")?;
                    self.ci += 1;
                    if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(']')) {
                        return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object."));
                    }

                    result = Expr::Attribute(Box::new(result), Box::new(attribute));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    result = Expr::Call(Box::new(result), self.parse_call_params()?);
                },
                TokenType::AssignmentOperator(op) => {
                    self.ci += 1;
                    let stmt_type = StmtType::Assign(result, op.clone(), self.parse_value("dserror(7): Expected an value before termination of the Stmt.")?);
                    return Ok(Stmt { val: stmt_type, start_index: pos.start });
                },
                TokenType::Punc(';') => break,
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }
                
        Ok(Stmt { val: StmtType::Expr(result), start_index: pos.start })
    }

    pub fn parse_sub_body(&mut self) -> Result<Vec<Stmt>, ASTError> {
        let mut stmts = Vec::new();
        self.ci += 1;

        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('{')) {
            return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(30): Expected a start of a body."));
        }

        self.ci += 1;
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('}') => return Ok(stmts),
                TokenType::Keyword(keyword) => {
                    let stmt = self.parse_keyword_statement(keyword, token.pos)?;
                    stmts.push(stmt);
                },
                TokenType::Word(name) => {
                    self.ci += 1;
                    let stmt = self.parse_word_statement(name, token.pos)?;
                    stmts.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let primary_statement = Stmt {
                        val: StmtType::Expr(self.parse_value("")?),
                        start_index: token.pos.start
                    };

                    stmts.push(primary_statement);
                }
            }
            
            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(35): Unexpected end of body."))
    }

    pub fn parse_function_params(&mut self) -> Result<Vec<FuncParam>, ASTError> {
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('(')) {
            return Err(self.create_error(self.tokens[self.ci].pos, "dserror(23): Missing parameters initialization for function."));
        }

        self.ci += 1;
        let mut params = Vec::new();
        let mut can_use_comma = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(',') if can_use_comma => can_use_comma = false,
                TokenType::Punc(')') => {
                    return if params.len() >= 255 {
                        Err(self.create_error(token.pos, "dserror(38) A function can have upto 254 parameters only."))
                    } else {
                        Ok(params)
                    }
                },
                TokenType::Punc('.') => {
                    return match self.tokens.get(self.ci..self.ci+5) {
                        Some([
                            Token { val: TokenType::Punc('.'), .. },
                            Token { val: TokenType::Punc('.'), .. },
                            Token { val: TokenType::Punc('.'), .. },
                            Token { val: TokenType::Word(name), .. },
                            Token { val: TokenType::Punc(')'), .. }
                        ]) => {
                            params.push((self.constant_pool.add_string(name), true));
                            self.ci += 3;
                            Ok(params)
                        },
                        _ => Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
                    }
                },
                TokenType::Word(name) => {
                    params.push((self.constant_pool.add_string(name), false));
                    can_use_comma = true;
                },
                _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected Expr."))
            }

            self.ci += 1;
        }

        return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(23): Missing parameters initialization for function."));
    }

    pub fn parse_while_loop(&mut self, pos: Position) -> Result<Stmt, ASTError> {
        self.ci += 1;
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('(')) {
            return Err(self.create_error(self.tokens[self.ci].pos, "dserror(26): Expected an open bracket."));
        }
        
        self.ci += 1;
        let mut condition = self.parse_parenthesis()?;
        condition = if let Expr::Group(group) = condition { 
            *group 
        } else { condition };

        Ok(Stmt { 
            val: StmtType::While(condition, self.parse_sub_body()?), 
            start_index: pos.start
        })
    }

    pub fn parse_condition_chain(&mut self, pos: Position) -> Result<Stmt, ASTError> {
        macro_rules! parse_partial_condition {
            () => {{
                self.ci += 1;
                if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('(')) {
                    return Err(self.create_error(pos, "Expected ( token but not found."))
                }

                self.ci += 1;
                let value = self.parse_value("dserror(21): Expected value in a bracket.")?;
                self.ci += 1;

                if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(')')) {
                    return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
                }
        
                (value, self.parse_sub_body()?)
            }};
        }

        let main_branch = parse_partial_condition!();
        let mut conditions = vec![];
        self.ci += 1;
        
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(Keyword::Elif) => conditions.push(parse_partial_condition!()),
                TokenType::Keyword(Keyword::Else) => {
                    return Ok(Stmt { val: StmtType::Condition(main_branch, conditions, Some(self.parse_sub_body()?)), start_index: token.pos.start });
                },
                _ => {
                    self.ci -= 1;
                    return Ok(Stmt { val: StmtType::Condition(main_branch, conditions, None), start_index: token.pos.start });
                }
            }

            self.ci += 1;
        }

        Ok(Stmt { val: StmtType::Condition(main_branch, conditions, None), start_index: pos.start })
    }

}