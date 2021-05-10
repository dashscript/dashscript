use dashscript_lexer::{Token, TokenType, Keyword, AssignmentOperator, LogicalOperator, Position};
use crate::{AST, ASTError, Identifier, Statement, StatementType, Condition, FuncParam};

macro_rules! is_token {
    ($ast:expr, $pattern:pat) => {{
        match $ast.tokens.get($ast.ci) {
            Some(Token { val: $pattern, .. }) => true,
            _ => false
        }
    }};
}

#[macro_export]
macro_rules! matches_token {
    (some $token:expr, $pattern:pat) => {
        match $token {
            Some(Token { val: $pattern, .. }) => true,
            _ => false
        }
    };

    ($token:expr, $pattern:pat) => {
        match $token {
            Token { val: $pattern, .. } => true,
            _ => false
        }
    };
}

impl AST {

    pub fn parse_value(&mut self, err: &str) -> Result<Identifier, ASTError> {
        let mut result = Identifier::default();
        let mut has_parsed = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();
            let is_null = match result { 
                Identifier::Null => true, 
                _ => false 
            };

            match &token.val {
                TokenType::Word(name) if is_null => {
                    self.ci += 1;
                    let index = self.constant_pool.add_string(name);
                    result = self.parse_identifier_as_object(Identifier::Word(index))?;
                },
                TokenType::String(string) if is_null => {
                    self.ci += 1;
                    let index = self.constant_pool.add_string(string);
                    result = self.parse_identifier_as_object(Identifier::String(index))?
                },
                TokenType::Number(num) if is_null => result = Identifier::Number(self.constant_pool.add_number(*num)),
                TokenType::Boolean(bool) if is_null => result = Identifier::Boolean(*bool),
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
                        '+' => result = Identifier::Add(Box::new(result), Box::new(next)),
                        '-' => result = Identifier::Subtract(Box::new(result), Box::new(next)),
                        '*' => result = Identifier::Multiply(Box::new(result), Box::new(next)),
                        '/' => result = Identifier::Divide(Box::new(result), Box::new(next)),
                        '^' => result = Identifier::Pow(Box::new(result), Box::new(next)),
                        _ => ()
                    };
                },
                TokenType::LogicalOperator(op) => {
                    match op {
                        LogicalOperator::Or => {
                            self.ci += 1;
                            result = Identifier::Or(
                                Box::new(result), 
                                Box::new(self.parse_value("dserror(28): Expected value after `or` logical operator.")?)
                            );
                        },
                        LogicalOperator::And => {
                            self.ci += 1;
                            result = Identifier::And(
                                Box::new(result), 
                                Box::new(self.parse_value("dserror(28): Expected value after `and` logical operator.")?)
                            );
                        },
                        LogicalOperator::Invert if is_null => {
                            self.ci += 1;
                            result = Identifier::Invert(
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::Equal => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::Equal,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::NotEqual => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::NotEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::GreaterThan => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::GreaterThan,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::GreaterThanOrEqual => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::GreaterThanOrEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::LessThan => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::LessThan,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        LogicalOperator::LessThanOrEqual => {
                            self.ci += 1;
                            result = Identifier::Condition(
                                Box::new(result), Condition::LessThanOrEqual,
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement.")?)
                            );
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
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
                    result = Identifier::Ternary(Box::new(result), Box::new(truthy), Box::new(falsy));
                },
                TokenType::Keyword(keyword) => {
                    match keyword {
                        Keyword::Await if is_null => {
                            self.ci += 1;
                            result = Identifier::Await(Box::new(self.parse_value("dserror(32): Expected a value to await.")?));
                        },
                        Keyword::Func if is_null => {
                            self.ci += 1;
                            result = Identifier::Func(
                                0,
                                self.parse_function_params()?,
                                self.parse_sub_body()?
                            );
                        },
                        Keyword::Async if is_null => {
                            self.ci += 1;
                            if !matches_token!(some self.tokens.get(self.ci), TokenType::Keyword(Keyword::Func)) {
                                return Err(self.create_error(token.pos, "dserror(31): Expected `func` keyword after `async` keyword."));
                            }

                            self.ci += 1;
                            result = Identifier::AsyncFunc(
                                0,
                                self.parse_function_params()?,
                                self.parse_sub_body()?
                            );
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                TokenType::Null => result = Identifier::Null,
                TokenType::Punc(';') | TokenType::Punc(',') | TokenType::Punc(')') | TokenType::Punc(']') | TokenType::Punc('}') | TokenType::Punc(':') if has_parsed => { 
                    self.ci -= 1; 
                    return Ok(result);
                },
                _ => {
                    println!("{:?}\n{:?} {:?}", self.constant_pool, token, result);
                    return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."));
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

    pub fn parse_keyword_statement(&mut self, keyword: &Keyword, pos: Position) -> Result<Statement, ASTError> {
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
                Ok(Statement {
                    val: StatementType::Var(index, self.parse_value("dserror(7): Expected an value before termination of the statement.")?, false),
                    start_index: pos.start
                })
            },
            Keyword::Const => {
                let index = get_word_as_index!("dserror(3): Expected a declaration name but found nothing.");
                consume_token!("dserror(4): Expected an assignment operator but found nothing.", TokenType::AssignmentOperator(AssignmentOperator::Assign));
                Ok(Statement {
                    val: StatementType::Var(index, self.parse_value("dserror(7): Expected an value before termination of the statement.")?, true),
                    start_index: pos.start
                })
            },
            Keyword::Return => {
                self.ci += 1;
                Ok(Statement {
                    val: StatementType::Return(
                        if is_token!(self, TokenType::Punc(';')) {
                            Identifier::Null
                        } else {
                            self.parse_value("dserror(22): Illegal return statement.")?
                        }
                    ),
                    start_index: pos.start
                })
            },
            Keyword::If => self.parse_condition_chain(pos),
            Keyword::Func => {
                Ok(Statement {
                    val: StatementType::Primary(
                        Identifier::Func(
                            get_word_as_index!("dserror(24): Missing function name."),
                            self.parse_function_params()?,
                            self.parse_sub_body()?
                        )
                    ),
                    start_index: pos.start
                })
            },
            Keyword::Async => {
                self.ci += 1;
                if !matches_token!(some self.tokens.get(self.ci), TokenType::Keyword(Keyword::Func)) {
                    return Err(self.create_error(pos, "dserror(31): Expected `func` keyword after `async` keyword."));
                }

                Ok(Statement {
                    val: StatementType::Primary(
                        Identifier::AsyncFunc(
                            get_word_as_index!("dserror(24): Missing function name."),
                            self.parse_function_params()?,
                            self.parse_sub_body()?
                        )
                    ),
                    start_index: pos.start
                })
            },
            Keyword::While => self.parse_while_loop(pos),
            Keyword::Break => Ok(Statement { val: StatementType::Break, start_index: pos.start }),
            Keyword::Continue => Ok(Statement { val: StatementType::Continue, start_index: pos.start }),
            Keyword::Await => {
                self.ci += 1;
                Ok(Statement {
                    val: StatementType::Primary(
                        Identifier::Await(
                            Box::new(self.parse_value("dserror(32): Expected a value to await.")?)
                        )
                    ),
                    start_index: pos.start
                })
            },
            _ => Ok(Statement::default())
        }
    }

    pub fn parse_identifier_as_object(&mut self, mut ident: Identifier) -> Result<Identifier, ASTError> {
        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match token.val {
                TokenType::Punc('.') => {
                    self.ci += 1;
                    ident = Identifier::Attribute(
                        Box::new(ident),
                        Box::new(
                            Identifier::String(
                                match self.tokens.get(self.ci) {
                                    Some(Token { val: TokenType::Word(name), .. }) => {
                                        self.ci += 1;
                                        self.constant_pool.add_string(name)
                                    },
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

                    ident = Identifier::Attribute(Box::new(ident), Box::new(attribute));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    ident = Identifier::Call(Box::new(ident), self.parse_call_params()?);
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

    pub fn parse_call_params(&mut self) -> Result<Vec<(Identifier, bool)>, ASTError> {
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
                            params.push((self.parse_value("dserror(7): Expected an value before termination of the statement.")?, true));
                            can_use_comma = true;
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                _ => {
                    let param = self.parse_value("dserror(7): Expected an value before termination of the statement.")?;
                    params.push((param, false));
                    can_use_comma = true;
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
    }

    pub fn parse_array(&mut self) -> Result<Identifier, ASTError> {
        let mut items: Vec<Identifier> = Vec::new();
        let mut can_use_comma = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc(']') => {
                    self.ci += 1;
                    return self.parse_identifier_as_object(Identifier::Array(items))
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

    pub fn parse_dict(&mut self) -> Result<Identifier, ASTError> {
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
                            Identifier::Word($id)
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
                    return self.parse_identifier_as_object(Identifier::Dict(attributes));
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

    pub fn parse_parenthesis(&mut self) -> Result<Identifier, ASTError> {
        let val = Identifier::Group(Box::new(self.parse_value("dserror(21): Expected value in a bracket.")?));
        self.ci += 1;
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc(')')) {
            return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
        }

        self.ci += 1;
        self.parse_identifier_as_object(val)
    }

    pub fn parse_word_statement(&mut self, name: &String, pos: Position) -> Result<Statement, ASTError> {
        let mut result = Identifier::Word(self.constant_pool.add_string(name));
        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('.') => {
                    self.ci += 1;
                    result = Identifier::Attribute(
                        Box::new(result),
                        Box::new(
                            Identifier::String(
                                match self.tokens.get(self.ci) {
                                    Some(Token { val: TokenType::Word(name), .. }) => {
                                        self.ci += 1;
                                        self.constant_pool.add_string(name)
                                    },
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

                    result = Identifier::Attribute(Box::new(result), Box::new(attribute));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    result = Identifier::Call(Box::new(result), self.parse_call_params()?);
                },
                TokenType::AssignmentOperator(op) => {
                    self.ci += 1;
                    let stmt_type = StatementType::Assign(result, op.clone(), self.parse_value("dserror(7): Expected an value before termination of the statement.")?);
                    return Ok(Statement { val: stmt_type, start_index: pos.start });
                },
                TokenType::Punc(';') => break,
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }
                
        Ok(Statement { val: StatementType::Primary(result), start_index: pos.start })
    }

    pub fn parse_sub_body(&mut self) -> Result<Vec<Statement>, ASTError> {
        let mut statements: Vec<Statement> = Vec::new();
        self.ci += 1;
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('{')) {
            return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(30): Expected a start of a body."));
        }

        self.ci += 1;
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('}') => return Ok(statements),
                TokenType::Keyword(keyword) => {
                    let stmt = self.parse_keyword_statement(keyword, token.pos)?;
                    statements.push(stmt);
                },
                TokenType::Word(name) => {
                    self.ci += 1;
                    let stmt = self.parse_word_statement(name, token.pos)?;
                    statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let primary_stmt = Statement {
                        val: StatementType::Primary(self.parse_value("")?),
                        start_index: token.pos.start
                    };

                    statements.push(primary_stmt);
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
                        _ => Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                TokenType::Word(name) => {
                    params.push((self.constant_pool.add_string(name), false));
                    can_use_comma = true;
                },
                _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
            }

            self.ci += 1;
        }

        return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(23): Missing parameters initialization for function."));
    }

    pub fn parse_while_loop(&mut self, pos: Position) -> Result<Statement, ASTError> {
        self.ci += 1;
        if !matches_token!(some self.tokens.get(self.ci), TokenType::Punc('(')) {
            return Err(self.create_error(self.tokens[self.ci].pos, "dserror(26): Expected an open bracket."));
        }
        
        self.ci += 1;
        let mut condition = self.parse_parenthesis()?;
        condition = if let Identifier::Group(group) = condition { 
            *group 
        } else { condition };

        Ok(Statement { 
            val: StatementType::While(condition, self.parse_sub_body()?), 
            start_index: pos.start
        })
    }

    pub fn parse_condition_chain(&mut self, pos: Position) -> Result<Statement, ASTError> {
        macro_rules! parse_partial_condition {
            () => {{
                self.ci += 1;
                match self.tokens.get(self.ci) {
                    Some(Token { val: TokenType::Punc('('), .. }) => (),
                    _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(26): Expected an open bracket."))
                }

                self.ci += 1;
                // TODO(Scientific-Guy): Instead of getting the parenthesis and dereferring it, make a conditon based parenthessis parser.
                let condition = self.parse_parenthesis()?;
                (if let Identifier::Group(group) = condition { 
                    *group 
                } else { condition }, self.parse_sub_body()?)
            }};
        }

        let mut conditions: Vec<(Identifier, Vec<Statement>)> = vec![parse_partial_condition!()];
        self.ci += 1;
        
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(Keyword::Elif) => conditions.push(parse_partial_condition!()),
                TokenType::Keyword(Keyword::Else) => {
                    return Ok(Statement { val: StatementType::Condition(conditions, Some(self.parse_sub_body()?)), start_index: token.pos.start });
                },
                _ => {
                    self.ci -= 1;
                    return Ok(Statement { val: StatementType::Condition(conditions, None), start_index: token.pos.start });
                }
            }

            self.ci += 1;
        }

        Ok(Statement { val: StatementType::Condition(conditions, None), start_index: pos.start })
    }

}