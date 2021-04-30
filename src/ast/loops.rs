use super::main::{ AST, ASTError };
use super::types::{ Statement, StatementType, Identifier, ForStmt };
use crate::lexer::parser::{ Token, TokenType, Position };

impl AST {

    pub fn get_while_loop(&mut self, pos: Position) -> Result<Statement, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('('),
                pos: _
            }) => (),
            _ => return Err(self.create_error(self.current_token().pos, "dserror(26): Expected an open bracket."))
        }
        
        let mut cond = self.get_parenthesis()?;
        cond = if let Identifier::Group(group) = cond { *group } else { cond };
        Ok(Statement { val: StatementType::While(cond, self.get_sub_body()?), pos })
    }

    pub fn get_for_loop(&mut self, pos: Position) -> Result<Statement, ASTError> {
        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::Punc('('),
                pos: _
            }) => self.ci += 1,
            _ => return Err(self.create_error(self.current_token().pos, "dserror(26): Expected an open bracket."))
        }
        
        let mut stmts = Vec::new();
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(')') => break,
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos)?;
                    stmts.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_for_word_statement(word.to_string(), token.pos)?;
                    stmts.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.get_value_as_stmt()?;
                    stmts.push(stmt);
                    self.ci -= 1;
                }
            }

            self.ci += 1;
        }

        let body = self.get_sub_body()?;
        self.ci -= 1;
        Ok(Statement {
            val: match stmts.len() {
                3 => StatementType::For(ForStmt::Stmts(stmts, body)),
                1 => {
                    let stmt = stmts[0].clone();
                    match &stmt.val {
                        StatementType::Primary(Identifier::In(name, ident)) => {
                            match *name.clone() {
                                Identifier::Word(name) => StatementType::For(ForStmt::In(name.to_string(), *ident.clone(), body)),
                                _ => return Err(self.create_error(pos, "dserror(36): Expected a word for `in` statement here."))
                            }
                        },
                        _ => return Err(self.create_error(pos, "dserror(37): Improper for statement."))
                    }
                },
                _ => return Err(self.create_error(pos, "dserror(37): Improper for statement."))
            },
            pos
        })
    }

    // TODO(Scientific-Guy): Find a better way to parse this.
    pub fn get_for_word_statement(&mut self, name: String, pos: Position) -> Result<Statement, ASTError> {
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
                    self.ci += 2;

                    match self.tokens.get(self.ci) {
                        Some(Token {
                            val: TokenType::Punc(']'),
                            pos: _
                        }) => (),
                        _ => return Err(self.create_error(self.tokens[self.ci - 1].pos, "dserror(15): Improper indexing for the object."))
                    }

                    res = Identifier::Attribute(Box::new(res), Box::new(index));
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
                TokenType::Keyword(keyword) => {
                    match keyword.as_str() {
                        "in" => {
                            self.ci += 1;
                            let value = self.get_value("dserror(7): Expected an value before termination of the statement.")?;
                            self.ci += 1;
                            return Ok(Statement {
                                val: StatementType::Primary(Identifier::In(Box::new(res), Box::new(value))),
                                pos
                            })
                        },
                        _ => {
                            self.ci -= 1;
                            break;
                        }
                    }
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
                        "!" => {
                            self.ci += 1;
                            res = Identifier::Invert(Box::new(self.get_value("dserror(7): Expected an value before termination of the statement.")?));
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