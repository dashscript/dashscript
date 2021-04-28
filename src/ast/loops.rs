use super::{
    main::AST,
    types::{
        Statement,
        StatementType,
        Identifier,
        ForStmt
    }
};

use crate::lexer::parser::{
    TokenType,
    Position
};

impl AST {

    pub fn parse_while_loop(&mut self, pos: Position) -> Statement {
        match self.next_token("dserror(26): Expected an open bracket.").val {
            TokenType::Punc('(') => (),
            _ => self.throw_error(self.current_token().pos, "dserror(26): Expected an open bracket.")
        }
        
        let mut cond = self.parse_parenthesis();
        cond = if let Identifier::Group(group) = cond { *group } else { cond };

        Statement {
            val: StatementType::While(cond, self.parse_sub_body()),
            pos
        }
    }

    pub fn parse_for_loop(&mut self, pos: Position) -> Statement {
        match self.next_token("dserror(26): Expected an open bracket.").val {
            TokenType::Punc('(') => (),
            _ => self.throw_error(self.current_token().pos, "dserror(26): Expected an open bracket.")
        }
        
        let stmts = self.parse_for_statement();
        self.ci -= 1;
        let body = self.parse_sub_body();
        self.ci -= 1;

        Statement {
            val: match stmts.len() {
                3 => StatementType::For(ForStmt::Stmts(stmts, body)),
                1 => {
                    let stmt = stmts[0].clone();
                    match &stmt.val {
                        StatementType::Primary(Identifier::In(name, ident)) => {
                            match *name.clone() {
                                Identifier::Word(name) => {
                                    StatementType::For(ForStmt::In(name.to_string(), *ident.clone(), body))
                                },
                                _ => {
                                    self.throw_error(pos, "dserror(36): Expected a word for `in` statement here.");
                                    StatementType::default()
                                }
                            }
                        },
                        _ => {
                            self.throw_error(pos, "dserror(37): Improper for statement.");
                            StatementType::default()
                        }
                    }
                },
                _ => {
                    self.throw_error(pos, "dserror(37): Improper for statement.");
                    StatementType::default()
                }
            },
            pos
        }
    }

    pub fn parse_for_statement(&mut self) -> Vec<Statement> {
        self.ci -= 1;
        let token = self.next_token("dserror(37): Improper for statement.");
        match &token.val {
            TokenType::Punc('(') => self.ci += 1,
            _ => self.throw_error(self.tokens[self.ci].pos, "dserror(37): Improper for statement.")
        }

        let mut statements: Vec<Statement> = Vec::new();
        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc(')') => return statements,
                TokenType::Keyword(key) => {
                    let stmt = self.get_keyword_statement(key.to_string(), token.pos);
                    statements.push(stmt);
                },
                TokenType::Word(word) => {
                    self.ci += 1;
                    let stmt = self.get_for_word_statement(word.to_string(), token.pos);
                    statements.push(stmt);
                },
                TokenType::Punc(';') => (),
                _ => {
                    let stmt = self.parse_value_as_stmt();
                    statements.push(stmt);
                    self.ci -= 1;
                }
            }

            self.ci += 1;
        }

        statements
    }

    pub fn get_for_word_statement(&mut self, name: String, pos: Position) -> Statement {
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
                TokenType::Keyword(keyword) => {
                    match keyword.as_str() {
                        "in" => {
                            self.ci += 1;
                            let value = self.parse_value("dserror(7): Expected an value before termination of the statement.");
                            self.ci += 1;
                            return Statement {
                                val: StatementType::Primary(
                                    Identifier::In(
                                        Box::new(res), 
                                        Box::new(value)
                                    )
                                ),
                                pos
                            }
                        },
                        _ => {
                            self.ci -= 1;
                            break;
                        }
                    }
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
                        "!" => {
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

}