use crate::lexer::parser::{ Position, TokenType, AssignmentOp };
use super::{
    main::AST,
    types::{
        Identifier,
        Statement,
        StatementType
    }
};

impl AST {

    pub fn get_keyword_statement(&mut self, name: String, pos: Position) -> Statement {
        match name.as_str() {
            "var" => self.get_decl_statement(pos, false),
            "const" => self.get_decl_statement(pos, true),
            "return" => Statement {
                val: StatementType::Return(match self.next_token("dserror(22): Illegal return statement.").val {
                    TokenType::Punc(';') => Identifier::Null,
                    _ => self.parse_value("dserror(22): Illegal return statement.")
                }),
                pos
            },
            "func" => self.parse_function(pos),
            "if" => self.parse_condition_chain(pos),
            "async" => self.parse_async_function(pos),
            "await" => Statement {
                val: StatementType::Primary(self.get_await_keyword()),
                pos
            },
            "break" => Statement {
                val: StatementType::Break,
                pos
            },
            "while" => self.parse_while_loop(pos),
            "continue" => Statement {
                val: StatementType::Continue,
                pos
            },
            "class" => self.parse_class(pos),
            "for" => self.parse_for_loop(pos),
            _ => {
                self.throw_error(pos, "dserror(14): Unexpected Identifier");
                Statement::default()
            }
        }
    }

    pub fn get_decl_statement(&mut self, pos: Position, is_const: bool) -> Statement {
        Statement {
            val: StatementType::Var(
                self.next_word_as_str("dserror(3): Expected a declaration name but found nothing."),
                {
                    let token = self.next_token("dserror(4): Expected an assignment operator but found nothing.");
                    self.ci += 1;
                    match &token.val {
                        TokenType::AssignmentOperator(AssignmentOp::Assign) => (),
                        _ => self.throw_error(self.tokens[self.ci - 1].pos, "dserror(4): Expected an assignment operator but found nothing.")
                    };

                    self.parse_value("dserror(7): Expected an value before termination of the statement.")
                },
                is_const
            ),
            pos
        }
    }

    pub fn get_await_keyword(&mut self) -> Identifier {
        self.ci += 1;
        Identifier::Await(
            Box::new(self.parse_value("dserror(32): Expected a value to await."))
        )
    }

    pub fn next_assignment_operator(&mut self, err: &str) -> AssignmentOp {
        let token = self.next_token(err);
        self.ci += 1;
        match &token.val {
            TokenType::AssignmentOperator(op) => op.clone(),
            _ => {
                self.throw_error(self.tokens[self.ci - 1].pos, err);
                AssignmentOp::Assign
            }
        }
    }

    
    pub fn parse_parenthesis(&mut self) -> Identifier {
        self.ci += 1;
        let mut val = self.parse_value("dserror(21): Expected value in a bracket.");
        val = Identifier::Group(Box::new(val));
        self.ci += 1;
        let close = self.next_token("dserror(10): Detected an unclosed bracket.");

        match &close.val {
            TokenType::Punc(')') => (),
            _ => self.throw_error(close.pos, "dserror(10): Detected an unclosed bracket.")
        }

        self.ci += 1;
        self.parse_word_as_identifier(val)
    }

    pub fn parse_array(&mut self) -> Identifier {
        let mut items: Vec<Identifier> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc(']') => {
                    self.ci += 1;
                    return self.parse_word_as_identifier(Identifier::Array(items))
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                _ => {
                    items.push(self.parse_value("dserror(10): Detected an unclosed bracket."));
                    sep_used = true;
                    self.ci += 1;
                }
            }

            self.ci += 1;
        }

        self.throw_error(self.tokens[self.ci - 1].pos, "dserror(10): Detected an unclosed bracket.");
        Identifier::default()
    }

    pub fn parse_partial_condition(&mut self) -> (Identifier, Vec<Statement>) {
        match self.next_token("dserror(26): Expected an open bracket.").val {
            TokenType::Punc('(') => (),
            _ => self.throw_error(self.current_token().pos, "dserror(26): Expected an open bracket.")
        }
        
        let mut cond = self.parse_parenthesis();
        cond = if let Identifier::Group(group) = cond { *group } else { cond };
        self.ci -= 1;
        let body = self.parse_sub_body();
        (cond, body)
    }

    pub fn parse_condition_chain(&mut self, pos: Position) -> Statement {
        let mut conditions: Vec<(Identifier, Vec<Statement>)> = vec![self.parse_partial_condition()];
        
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Keyword(word) => match word.as_str() {
                    "elif" => {
                        self.ci -= 1;
                        conditions.push(self.parse_partial_condition());
                    },
                    "else" => {
                        self.ci -= 1;
                        let stmt = Statement {
                            val: StatementType::Condition(conditions, Some(self.parse_sub_body())),
                            pos
                        };

                        return stmt;
                    },
                    _ => {
                        self.ci -= 1;
                        break;
                    }
                },
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }

        Statement {
            val: StatementType::Condition(conditions, None),
            pos
        }
    }

    pub fn parse_function_params(&mut self) -> Vec<String> {
        match self.next_token("dserror(23): Missing parameters initialization for function.").val {
            TokenType::Punc('(') => self.ci += 1,
            _ => self.throw_error(self.tokens[self.ci].pos, "dserror(23): Missing parameters initialization for function.")
        }
        
        let mut params: Vec<String> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(',') if sep_used => (),
                TokenType::Punc(')') => {
                    if params.len() >= 255 { self.throw_error(token.pos, "dserror(38) A function can have upto 254 parameters only.") }
                    self.ci -= 1;
                    return params
                },
                TokenType::Word(name) => params.push(name.to_string()),
                _ => {
                    self.throw_error(token.pos, "dserror(14): Unexpected identifier.");
                    sep_used = true
                }
            }

            self.ci += 1;
        }

        self.throw_error(self.tokens[self.ci - 1].pos, "dserror(23): Missing parameters initialization for function.");
        params
    }

    pub fn parse_function(&mut self, pos: Position) -> Statement {
        Statement {
            val: StatementType::Primary(
                Identifier::Func(
                    self.next_word_as_str("dserror(24): Missing function name."),
                    self.parse_function_params(),
                    self.parse_sub_body()
                )
            ),
            pos
        }
    }

    pub fn parse_async_function(&mut self, pos: Position) -> Statement {
        match self.next_token("dserror(31): Expected `func` keyword after `async` keyword.").val {
            TokenType::Keyword(keyword) => if keyword != "func".to_string() {
                self.throw_error(pos, "dserror(31): Expected `func` keyword after `async` keyword.")
            },
            _ => self.throw_error(pos, "dserror(31): Expected `func` keyword after `async` keyword.")
        }

        Statement {
            val: StatementType::Primary(
                Identifier::AsyncFunc(
                    self.next_word_as_str("dserror(24): Missing function name."),
                    self.parse_function_params(),
                    self.parse_sub_body()
                )
            ),
            pos
        }
    }

    pub fn parse_anonymous_function(&mut self) -> Identifier {
        Identifier::Func(
            "anonymous".to_string(),
            self.parse_function_params(),
            self.parse_sub_body()
        )
    }

    pub fn parse_anonymous_async_function(&mut self, pos: Position) -> Identifier {
        match self.next_token("dserror(31): Expected `func` keyword after `async` keyword.").val {
            TokenType::Keyword(keyword) => if keyword != "func".to_string() {
                self.throw_error(pos, "dserror(31): Expected `func` keyword after `async` keyword.")
            },
            _ => self.throw_error(pos, "dserror(31): Expected `func` keyword after `async` keyword.")
        }

        Identifier::AsyncFunc(
            "anonymous".to_string(),
            self.parse_function_params(),
            self.parse_sub_body()
        )
    }

    pub fn parse_call_params(&mut self) -> Vec<Identifier> {
        let mut params: Vec<Identifier> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(')') => {
                    if params.len() >= 255 { self.throw_error(token.pos, "dserror(38) A function can have upto 254 parameters only.") }
                    return params
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                _ => {
                    params.push(self.parse_value("dserror(7): Expected an value before termination of the statement."));
                    self.ci += 1;
                    sep_used = true;
                }
            }

            self.ci += 1;
        }

        self.throw_error(self.tokens[self.ci - 2].pos, "dserror(10): Detected an unclosed bracket.");
        params
    }

    pub fn parse_dict(&mut self) -> Identifier {
        let mut attributes: Vec<(String, Identifier)> = Vec::new();
        let mut sep_used = false;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('}') => {
                    self.ci += 1;
                    return self.parse_word_as_identifier(Identifier::Dict(attributes))
                },
                TokenType::String(key) => {
                    #[allow(mutable_borrow_reservation_conflict)]
                    attributes.push((key.clone(), self.parse_dict_key_value(key.clone(), token.pos, false)));
                    sep_used = true;
                    self.ci += 1;
                },
                TokenType::Word(key) => {
                    #[allow(mutable_borrow_reservation_conflict)]
                    attributes.push((key.clone(), self.parse_dict_key_value(key.clone(), token.pos, true)));
                    sep_used = true;
                    self.ci += 1;
                },
                TokenType::Punc(',') if sep_used => sep_used = false,
                _ => {
                    println!("{:#?}", token);
                    self.throw_error(token.pos, "dserror(17): Dict keys must be a string or a word representation.");
                }
            }

            self.ci += 1;
        }

        Identifier::default()
    }

    pub fn parse_dict_key_value(&mut self, key: String, pos: Position, is_word: bool) -> Identifier {
        if key.len() == 0 { 
            self.throw_error(pos, "dserror(18): Dict keys strings must not be empty.") 
        }

        let sep_token = self.next_token("dserror(19): Expected `:` character to represent the value for the key.");
        match &sep_token.val {
            TokenType::Punc(':') => (),
            _ => {
                if is_word {
                    self.ci -= 1;
                    return Identifier::Word(key);
                }

                self.throw_error(pos, "dserror(19): Expected `:` character to represent the value for the key.")
            }
        }

        self.ci += 1;
        self.parse_value("dserror(20): Dict must have a value or have a direct representation.")
    }

}