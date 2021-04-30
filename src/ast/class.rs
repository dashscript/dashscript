use super::main::{ AST, ASTError };
use super::types::{ Statement, StatementType, ClassMethod, ClassProp };
use crate::lexer::parser::{ Token, TokenType, Position, AssignmentOp };

impl AST {

    // TODO(Scientific-Guy): Make a better ast wrapper for classes.
    pub fn get_class_statement(&mut self, pos: Position) -> Result<Statement, ASTError> {
        let name = self.next_word_as_str("dserror(34): Expected a name for the class.")?;
        let mut extends: Option<String> = None;

        self.ci += 1;
        match self.tokens.get(self.ci) {
            Some(Token {
                val: TokenType::LogicalOperator(op),
                pos: _
            }) if op.as_str() == "<=" => {
                extends = Some(self.next_word_as_str("dserror(34): Expected a name for the extended class.")?);
                self.ci += 1;
                match self.tokens.get(self.ci) {
                    Some(Token {
                        val: TokenType::Punc('{'),
                        pos: _
                    }) => (),
                    _ => return Err(self.create_error(pos, "dserror(30): Expected a start of a body."))
                }
            },
            Some(Token {
                val: TokenType::Punc('{'),
                pos: _
            }) => (),
            _ => return Err(self.create_error(pos, "dserror(30): Expected a start of a body."))
        }

        self.ci += 1;
        let (props, methods) = self.get_class_body()?;
        Ok(Statement {
            val: StatementType::Class {
                name,
                extends,
                props,
                methods
            },
            pos
        })
    }

    pub fn get_class_body(&mut self) -> Result<(Vec<ClassProp>, Vec<ClassMethod>), ASTError> {
        let mut props: Vec<ClassProp> = Vec::new();
        let mut methods: Vec<ClassMethod>  = Vec::new();

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();

            match &token.val {
                TokenType::Punc('}') => {
                    self.ci += 1;
                    return Ok((props, methods));
                },
                TokenType::Keyword(keyword) => {
                    match keyword.as_str() {
                        "static" => {
                            let next = self.next_token("dserror(14): Unexpected identifier.")?;
                            match &next.val {
                                TokenType::Word(word) => match self.next_token("dserror(14): Unexpected identifier.")?.val {
                                    TokenType::Punc('(') => methods.push(self.get_class_method(word, true)?),
                                    TokenType::AssignmentOperator(AssignmentOp::Assign) => props.push({
                                        self.ci += 1;
                                        (word.to_string(), self.get_value("dserror(7): Expected an value before termination of the statement.")?, true)
                                    }),
                                    _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                                },
                                TokenType::Keyword(keyword) => {
                                    match keyword.as_str() {
                                        "async" => methods.push(self.get_async_class_method(token.pos, true)?),
                                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                                    }
                                },
                                _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                            }
                        },
                        "async" => methods.push(self.get_async_class_method(token.pos, false)?),
                        _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                    }
                },
                TokenType::Word(word) => match self.next_token("dserror(14): Unexpected identifier.")?.val {
                    TokenType::Punc('(') => methods.push(self.get_class_method(word, false)?),
                    TokenType::AssignmentOperator(AssignmentOp::Assign) => props.push({
                        self.ci += 1;
                        (word.to_string(), self.get_value("dserror(7): Expected an value before termination of the statement.")?, false)
                    }),
                    _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
                },
                TokenType::Punc(';') => (),
                _ => return Err(self.create_error(token.pos, "dserror(14): Unexpected identifier."))
            }

            self.ci += 1;
        }

        Err(self.create_error(self.tokens[self.ci - 2].pos, "dserror(35): Unexpected end of body."))
    }

    pub fn get_class_method(&mut self, word: &String, is_static: bool) -> Result<ClassMethod, ASTError> {
        self.ci -= 1;
        let params = self.get_function_params()?;
        self.ci -= 1;
        let body = self.get_function_body()?;
        self.ci -= 1;
        Ok((word.to_string(), params, body, false, is_static))
    }

    pub fn get_async_class_method(&mut self, pos: Position, is_static: bool) -> Result<ClassMethod, ASTError> {
        self.ci += 1;
        // TODO(Scientific-Guy): Find a way to get the token range without cloning it.
        let self_clone = self.clone();

        if let Some([Token {
            val: TokenType::Word(name),
            pos: _
        }, Token {
            val: TokenType::Punc('('),
            pos: _
        }]) = self_clone.tokens.get(self.ci..self.ci + 2) {
            self.ci += 1;
                let params = self.get_function_params()?;
                self.ci -= 1;
                let body = self.get_function_body()?;
                self.ci -= 1;
                Ok((name.to_string(), params, body, true, is_static))
        } else {
            Err(self.create_error(pos, "dserror(14): Unexpected identifier."))
        }
    }
}