use crate::lexer::parser::{ TokenType, Position };
use super::{
    main::AST,
    types::{
        Statement,
        StatementType,
        Identifier
    }
};

impl AST {

    pub fn parse_value(&mut self, err: &str) -> Identifier {
        let mut res = Identifier::default();
        let mut has_parsed = false;

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();
            let res_is_null = res.is_null();

            match &token.val {
                TokenType::String(str) if res_is_null => {
                    self.ci += 1;
                    res = self.parse_word_as_identifier(Identifier::String(str.clone()));
                },
                TokenType::Word(word) if res_is_null => {
                    self.ci += 1;
                    res = self.parse_word_as_identifier(Identifier::Word(word.to_string()));
                },
                TokenType::Number(num) if res_is_null => res = Identifier::Number(num.clone()),
                TokenType::Punc('[') if res_is_null => {
                    self.ci += 1;
                    res = self.parse_array();
                },
                TokenType::Punc('{') if res_is_null => {
                    self.ci += 1;
                    res = self.parse_dict();
                },
                TokenType::Punc('(') if res_is_null => {
                    res = self.parse_parenthesis();
                },
                TokenType::Keyword(keyword) if res_is_null => match keyword.as_str() {
                    "func" => {
                        res = self.parse_anonymous_function();
                        self.ci -= 1;
                    },
                    "async" => {
                        res = self.parse_anonymous_async_function(token.pos);
                        self.ci -= 1;
                    },
                    "await" => res = self.get_await_keyword(),
                    _ => self.throw_error(token.pos, "dserror(27): Illegal keyword to use as a value.")
                },
                TokenType::Arithmetic(char) => {
                    self.ci += 1;
                    let next = self.parse_value("dserror(13): Improper arithmetic equation.");
                    self.ci += 1;
                    match char.as_str() {
                        "+" => res = Identifier::Add(Box::new(res), Box::new(next)),
                        "-" => res = Identifier::Subtract(Box::new(res), Box::new(next)),
                        "*" => res = Identifier::Multiply(Box::new(res), Box::new(next)),
                        "/" => res = Identifier::Divide(Box::new(res), Box::new(next)),
                        "^" => res = Identifier::Pow(Box::new(res), Box::new(next)),
                        _ => ()
                    }
                },
                TokenType::LogicalOperator(op) => {
                    match op.as_str() {
                        "||" => {
                            self.ci += 1;
                            res = Identifier::Or(
                                Box::new(res), 
                                Box::new(self.parse_value("dserror(28): Expected value after `or` logical operator."))
                            );

                            self.ci += 1;
                        },
                        "&&" => {
                            self.ci += 1;
                            res = Identifier::And(
                                Box::new(res), 
                                Box::new(self.parse_value("dserror(28): Expected value after `and` logical operator."))
                            );

                            self.ci += 1;
                        },
                        "!" if res_is_null => {
                            self.ci += 1;
                            res = Identifier::Invert(
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement."))
                            );

                            self.ci += 1;
                        },
                        "==" | "!=" | "<=" | ">=" | "<" | ">" => {
                            self.ci += 1;
                            res = Identifier::Condition(
                                Box::new(res),
                                op.to_string(),
                                Box::new(self.parse_value("dserror(7): Expected an value before termination of the statement."))
                            );

                            self.ci += 1;
                        },
                        _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                    }
                },
                TokenType::Punc(';') => break,
                TokenType::Punc(',') | TokenType::Punc(')') | TokenType::Punc(']') | TokenType::Punc('}') | TokenType::Punc(':') if has_parsed => { self.ci -= 1; break },
                TokenType::Null => res = Identifier::Null,
                TokenType::Boolean(bool) => res = Identifier::Boolean(*bool),
                TokenType::Punc('?') if has_parsed => {
                    self.ci += 1;
                    let truthy = self.parse_value("dserror(33): Improper ternary operator.");
                    self.ci += 1;
                    match self.next_token("dserror(33): Improper ternary operator.").val {
                        TokenType::Punc(':') => (),
                        _ => self.throw_error(token.pos, "dserror(33): Improper ternary operator.")
                    }

                    self.ci += 1;
                    let falsy = self.parse_value("dserror(33): Improper ternary operator.");
                    res = Identifier::Ternary(
                        Box::new(res),
                        Box::new(truthy),
                        Box::new(falsy)
                    )
                },
                _ => {
                    println!("{:#?} {:#?}", token, res);
                    self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                }
            }

            has_parsed = true;
            self.ci += 1;
        }

        if !has_parsed { self.throw_error(self.tokens[self.ci - 1].pos, err); }
        self.ci -= 1;
        res
    }

    pub fn parse_value_as_stmt(&mut self) -> Statement {
        Statement {
            val: StatementType::Primary(self.parse_value("dserror(14): Unexpected identifier")),
            pos: self.tokens[if self.ci >= self.len { self.ci - 1 } else { self.ci }].pos
        }
    }

    pub fn parse_word_as_identifier(&mut self, word: Identifier) -> Identifier {
        let mut res = word;

        while self.ci < self.len {
            let token = &self.tokens[self.ci];

            match &token.val {
                TokenType::Punc('.') => {
                    let wrd = self.next_word_as_str("dserror(2): Found an improper attribute declaration.");
                    res = Identifier::Attribute(Box::new(res), Box::new(Identifier::String(wrd)));
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let index = self.parse_value("dserror(15): Improper indexing for the object.");
                    self.ci += 1;
                    
                    match self.next_token("dserror(15): Improper indexing for the object.").val{
                        TokenType::Punc(']') => (),
                        _ => self.throw_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object.")
                    }

                    res = Identifier::Attribute(
                        Box::new(res), 
                        Box::new(index)
                    );
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    let params = self.parse_call_params();
                    res = Identifier::Call(Box::new(res), params);
                },
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }

        res
    }

    pub fn parse_sub_body(&mut self) -> Vec<Statement> {
        self.ci += 1;
        self.next_sub_body_open();
        let mut statements: Vec<Statement> = Vec::new();

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('}') => return statements,
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos);
                    statements.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_word_statement(word.to_string(), token.pos);
                    statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.parse_value_as_stmt();
                    statements.push(stmt);
                }
            }

            self.ci += 1;
        }

        statements
    }

    pub fn get_word_statement(&mut self, name: String, pos: Position) -> Statement {
        let mut res = Identifier::Word(name);

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('.') => {
                    let wrd = self.next_word_as_str("dserror(2): Found an improper attribute declaration.");
                    res = Identifier::Attribute(Box::new(res), Box::new(Identifier::String(wrd)));
                },
                TokenType::Punc('(') => {
                    self.ci += 1;
                    let params = self.parse_call_params();
                    res = Identifier::Call(Box::new(res), params);
                },
                TokenType::Punc('[') => {
                    self.ci += 1;
                    let index = self.parse_value("dserror(15): Improper indexing for the object.");
                    self.ci += 1;

                    match self.next_token("dserror(15): Improper indexing for the object.").val{
                        TokenType::Punc(']') => (),
                        _ => self.throw_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object.")
                    }

                    res = Identifier::Attribute(
                        Box::new(res), 
                        Box::new(index)
                    );
                },
                TokenType::AssignmentOperator(op) => {
                    self.ci += 1;
                    let stmt = Statement {
                        val: StatementType::Assign(res, op.clone(), self.parse_value("dserror(7): Expected an value before termination of the statement.")),
                        pos
                    };

                    self.ci += 1;
                    return stmt;
                },
                TokenType::Punc(';') => break,
                _ => {
                    self.ci -= 1;
                    break;
                }
            }

            self.ci += 1;
        }
                
        Statement {
            val: StatementType::Primary(res),
            pos
        }
    }

    pub fn next_sub_body_open(&mut self) {
        let token = self.next_token("dserror(25): Missing body");
        match &token.val {
            TokenType::Punc('{') => {
                self.ci += 1;
                return
            },
            _ => self.throw_error(token.pos, "dserror(25): Missing body.")
        }
    }

    pub fn next_word_as_str(&mut self, err: &str) -> String {
        self.ci += 1;
        if self.ci == self.len {
            self.throw_error(self.tokens[self.ci - 1].pos, err);
        }

        let token = self.tokens[self.ci].clone();
        match &token.val {
            TokenType::Word(s) => s.clone(),
            _ => {
                self.throw_error(self.tokens[self.ci - 1].pos, err);
                String::new()
            }
        }
    }

    pub fn next_string_as_str(&mut self, err: &str) -> String {
        let token = self.next_token(err);
        match &token.val {
            TokenType::String(s) => s.clone(),
            _ => {
                self.throw_error(self.tokens[self.ci - 1].pos, err);
                String::new()
            }
        }
    }

}