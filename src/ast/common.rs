use crate::lexer::parser::{ Position, TokenType, AssignmentOp, Token };
use super::main::{ AST, ASTError };
use super::types::{ Identifier, Statement, StatementType, FuncParam };

impl AST {

    pub fn get_keyword_statement(&mut self, name: String, pos: Position) -> Result<Statement, ASTError> {
        match name.as_str() {
            "var" => self.get_declaration_statement(pos, false),
            "const" => self.get_declaration_statement(pos, true),

            // TODO(Scientific-Guy): Make a better return statement. (Least likely).
            "return" => Ok(Statement {
                val: StatementType::Return({
                    self.ci += 1;
                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::Punc(';'),
                            pos: _
                        }) => Identifier::Null,
                        _ => self.get_value("dserror(22): Illegal return statement.")?
                    }
                }),
                pos
            }),

            // TODO(Scientific-Guy): Make a better await handeler but i feel its not necessary.
            "await" => {
                self.ci += 1;
                Ok(Statement {
                    val: StatementType::Primary(Identifier::Await(
                        Box::new(self.get_value("dserror(32): Expected a value to await.")?)
                    )),
                    pos
                })
            },

            "func" => self.get_function_statement(pos),
            "async" => self.get_async_function_statement(pos),
            "if" => self.get_condition_chain(pos),
            "break" => Ok(Statement { val: StatementType::Break, pos }),
            "while" => self.get_while_loop(pos),
            "continue" => Ok(Statement { val: StatementType::Continue, pos }),
            "class" => self.get_class_statement(pos),
            "for" => self.get_for_loop(pos),
            _ => Err(self.create_error(pos, "dserror(14): Unexpected Identifier"))
        }
    }

    pub fn get_declaration_statement(&mut self, pos: Position, is_const: bool) -> Result<Statement, ASTError> {
        Ok(Statement {
            val: StatementType::Var(
                self.next_word_as_str("dserror(3): Expected a declaration name but found nothing.")?,
                {
                    self.ci += 1;
                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::AssignmentOperator(AssignmentOp::Assign),
                            pos: _
                        }) => self.ci += 1,
                        _ => return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(4): Expected an assignment operator but found nothing."))
                    }

                    self.get_value("dserror(7): Expected an value before termination of the statement.")?
                },
                is_const
            ),
            pos
        })
    }
    
    pub fn get_parenthesis(&mut self) -> Result<Identifier, ASTError> {
        self.ci += 1;
        let mut val = self.get_value("dserror(21): Expected value in a bracket.")?;
        val = Identifier::Group(Box::new(val));

        self.ci += 2;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc(')'),
                pos: _
            }) => (),
            _ => return Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
        }

        self.ci += 1;
        self.get_identifier_as_word(val)
    }

    pub fn get_array(&mut self) -> Result<Identifier, ASTError> {
        let mut items: Vec<Identifier> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc(']') => {
                    self.ci += 1;
                    return self.get_identifier_as_word(Identifier::Array(items))
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                _ => {
                    items.push(self.get_value("dserror(10): Detected an unclosed bracket.")?);
                    sep_used = true;
                    self.ci += 1;
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(10): Detected an unclosed bracket."))
    }

    pub fn get_partial_condition(&mut self) -> Result<(Identifier, Vec<Statement>), ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('('),
                pos: _
            }) => (),
            _ => return Err(self.create_error(self.current_token().pos, "dserror(26): Expected an open bracket."))
        }

        // TODO(Scientific-Guy): Instead of getting the parenthesis and dereferring it, make a conditon based parenthessis parser.
        let mut cond = self.get_parenthesis()?;
        cond = if let Identifier::Group(group) = cond { *group } else { cond };
        Ok((cond, self.get_sub_body()?))
    }

    pub fn get_condition_chain(&mut self, pos: Position) -> Result<Statement, ASTError> {
        let mut conditions: Vec<(Identifier, Vec<Statement>)> = vec![self.get_partial_condition()?];
        self.ci += 1;
        
        // Code warning:
        // self.ci += 1; at the of the loop.
        // self.ci -= 1; at the start of the loop.
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(word) => {
                    match word.as_str() {
                        "elif" => conditions.push(self.get_partial_condition()?),
                        "else" => return Ok(Statement {
                            val: StatementType::Condition(conditions, Some(self.get_sub_body()?)),
                            pos
                        }),
                        _ => {
                            self.ci -= 1;
                            break
                        }
                    }
                },
                _ => {
                    self.ci -= 1;
                    break
                }
            }

            self.ci += 1;
        }

        Ok(Statement { val: StatementType::Condition(conditions, None), pos })
    }

    pub fn get_function_params(&mut self) -> Result<Vec<FuncParam>, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('('),
                pos: _
            }) => self.ci += 1,
            _ => return Err(self.create_error(self.tokens[self.ci].pos, "dserror(23): Missing parameters initialization for function."))
        }
        
        let mut params: Vec<FuncParam> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(',') if sep_used => sep_used = false,
                TokenType::Punc(')') => {
                    return if params.len() >= 255 {
                        Err(self.create_error(token.pos, "dserror(38) A function can have upto 254 parameters only."))
                    } else {
                        self.ci -= 1;
                        Ok(params)
                    }
                },
                TokenType::Punc('.') => {
                    return match self.tokens.get(self.ci..self.ci+5) {
                        Some([
                            Token { val: TokenType::Punc('.'), pos: _ },
                            Token { val: TokenType::Punc('.'), pos: _ },
                            Token { val: TokenType::Punc('.'), pos: _ },
                            Token { val: TokenType::Word(name), pos: _ },
                            Token { val: TokenType::Punc(')'), pos: _ }
                        ]) => {
                            params.push((name.clone(), true));
                            // Could be an error
                            self.ci += 3;
                            Ok(params)
                        },
                        _ => Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                TokenType::Word(name) => {
                    params.push((name.clone(), false));
                    sep_used = true;
                },
                _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
            }

            self.ci += 1;
        }

        return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(23): Missing parameters initialization for function."));
    }

    pub fn get_function_body(&mut self) -> Result<Vec<Statement>, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('{'),
                pos: _
            }) => {
                self.ci += 1;
                let mut statements = Vec::new();

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
            },
            _ => Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(25): Missing body."))
        }
    }

    pub fn get_function_statement(&mut self, pos: Position) -> Result<Statement, ASTError> {
        Ok(Statement {
            val: StatementType::Primary(
                Identifier::Func(
                    self.next_word_as_str("dserror(24): Missing function name.")?,
                    self.get_function_params()?,
                    {
                        self.ci += 1;
                        self.get_function_body()?
                    }
                )
            ),
            pos
        })
    }

    pub fn get_async_function_statement(&mut self, pos: Position) -> Result<Statement, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Keyword(keyword),
                pos: _
            }) if keyword.as_str() == "func" => (),
            _ => return Err(self.create_error(pos, "dserror(31): Expected `func` keyword after `async` keyword."))
        }

        Ok(Statement {
            val: StatementType::Primary(
                Identifier::AsyncFunc(
                    self.next_word_as_str("dserror(24): Missing function name.")?,
                    self.get_function_params()?,
                    {
                        self.ci += 1;
                        self.get_function_body()?
                    }
                )
            ),
            pos
        })
    }

    pub fn get_call_params(&mut self) -> Result<Vec<(Identifier, bool)>, ASTError> {
        let mut params = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(')') => {
                    return if params.len() >= 255 { 
                        Err(self.create_error(token.pos, "dserror(38) A function can have upto 254 parameters only."))
                    } else { Ok(params) }
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                TokenType::Punc('.') => {
                    match self.tokens.get(self.ci..self.ci+3) {
                        Some([
                            Token { val: TokenType::Punc('.'), pos: _ },
                            Token { val: TokenType::Punc('.'), pos: _ },
                            Token { val: TokenType::Punc('.'), pos: _ },
                        ]) => {
                            self.ci += 3;
                            params.push((self.get_value("dserror(7): Expected an value before termination of the statement.")?, true));
                            self.ci += 1;
                            sep_used = true;
                        },
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                _ => {
                    params.push((self.get_value("dserror(7): Expected an value before termination of the statement.")?, false));
                    self.ci += 1;
                    sep_used = true;
                }
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket."))
    }

    pub fn get_dict(&mut self) -> Result<Identifier, ASTError> {
        let mut attributes: Vec<(String, Identifier)> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('}') => {
                    self.ci += 1;
                    return self.get_identifier_as_word(Identifier::Dict(attributes));
                },
                TokenType::String(key) => {
                    #[allow(mutable_borrow_reservation_conflict)]
                    attributes.push((key.clone(), self.get_dict_key_value(key.clone(), token.pos, false)?));
                    sep_used = true;
                    self.ci += 1;
                },
                TokenType::Word(key) => {
                    #[allow(mutable_borrow_reservation_conflict)]
                    attributes.push((key.clone(), self.get_dict_key_value(key.clone(), token.pos, true)?));
                    sep_used = true;
                    self.ci += 1;
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                _ => return Err(self.create_error(token.pos, "dserror(17): Dict keys must be a string or a word representation."))
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(17): Dict keys must be a string or a word representation."))
    }

    pub fn get_dict_key_value(&mut self, key: String, pos: Position, is_word: bool) -> Result<Identifier, ASTError> {
        if key.len() == 0 { 
            return Err(self.create_error(pos, "dserror(18): Dict keys strings must not be empty."));
        }

        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc(':'),
                pos: _
            }) => (),
            _ => {
                if is_word {
                    self.ci -= 1;
                    return Ok(Identifier::Word(key));
                }

                return Err(self.create_error(pos, "dserror(19): Expected `:` character to represent the value for the key."));
            }
        }

        self.ci += 1;
        self.get_value("dserror(20): Dict must have a value or have a direct representation.")
    }

    pub fn next_word_as_str(&mut self, err: &str) -> Result<String, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Word(str),
                pos: _
            }) => Ok(str.clone()),
            _ => Err(self.create_error(self.tokens[self.ci - 2].pos, err))
        }
    }

}