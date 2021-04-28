use super::{
    main::AST,
    types::{
        Statement,
        StatementType,
        ClassMethod,
        ClassProp
    }
};

use crate::lexer::parser::{
    TokenType,
    Position,
    AssignmentOp
};

impl AST {

    pub fn parse_class(&mut self, pos: Position) -> Statement {
        let name = self.next_word_as_str("dserror(34): Expected a name for the class.");
        let next_token = self.next_token("dserror(30): Expected a start of a body.");
        let mut extends: Option<String> = None;

        match next_token.val {
            TokenType::LogicalOperator(op) => {
                if op == "<=".to_string() {
                    extends = Some(self.next_word_as_str("dserror(34): Expected a name for the extended class."));
                    match self.next_token("dserror(30): Expected a start of a body.").val {
                        TokenType::Punc('{') => (),
                        _ => self.throw_error(pos, "dserror(30): Expected a start of a body.")
                    }
                } else {
                    self.throw_error(pos, "dserror(30): Expected a start of a body.")
                }
            },
            TokenType::Punc('{') => (),
            _ => self.throw_error(pos, "dserror(30): Expected a start of a body.")
        }

        self.ci += 1;
        let (props, methods) = self.parse_class_body();
        Statement {
            val: StatementType::Class {
                name,
                extends,
                props,
                methods
            },
            pos
        }
    }

    pub fn parse_class_body(&mut self) -> (Vec<ClassProp>, Vec<ClassMethod>) {
        let mut props: Vec<ClassProp> = Vec::new();
        let mut methods: Vec<ClassMethod>  = Vec::new();

        while self.ci < self.len {
            let token = self.tokens[self.ci].clone();
            println!("{:#?}", token);

            match &token.val {
                TokenType::Punc('}') => {
                    self.ci += 1;
                    return (props, methods);
                },
                TokenType::Keyword(keyword) => {
                    match keyword.as_str() {
                        "static" => {
                            let next = self.next_token("dserror(14): Unexpected identifier.");
                            match &next.val {
                                TokenType::Word(word) => match self.next_token("dserror(14): Unexpected identifier.").val {
                                    TokenType::Punc('(') => methods.push(self.parse_class_method(word, false, true)),
                                    TokenType::AssignmentOperator(AssignmentOp::Assign) => props.push(self.parse_class_prop(word, true)),
                                    _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                                },
                                TokenType::Keyword(keyword) => {
                                    match keyword.as_str() {
                                        "async" => methods.push(self.parse_async_class_method(token.pos, true)),
                                        _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                                    }
                                },
                                _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                            }
                        },
                        "async" => methods.push(self.parse_async_class_method(token.pos, false)),
                        _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                    }
                },
                TokenType::Word(word) => match self.next_token("dserror(14): Unexpected identifier.").val {
                    TokenType::Punc('(') => methods.push(self.parse_class_method(word, false, false)),
                    TokenType::AssignmentOperator(AssignmentOp::Assign) => props.push(self.parse_class_prop(word, false)),
                    _ => self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                },
                TokenType::Punc(';') => (),
                _ => {
                    println!("ERR => {:#?}", token);
                    self.throw_error(token.pos, "dserror(14): Unexpected identifier.")
                }
            }

            self.ci += 1;
        }

        self.throw_error(self.tokens[self.ci - 2].pos, "dserror(35): Unexpected end of body.");
        (props, methods)
    }

    pub fn parse_class_prop(&mut self, word: &String, is_static: bool) -> ClassProp {
        self.ci += 1;
        (
            word.to_string(),
            self.parse_value("dserror(7): Expected an value before termination of the statement."),
            is_static
        )
    }

    pub fn parse_class_method(&mut self, word: &String, is_static: bool, is_async: bool) -> ClassMethod {
        self.ci -= 1;
        let params = self.parse_function_params();
        self.ci -= 1;
        let body = self.parse_sub_body();
        self.ci -= 1;
        (
            word.to_string(),
            params, 
            body,
            is_async,
            is_static,
        )
    }

    pub fn parse_async_class_method(&mut self, pos: Position, is_static: bool) -> ClassMethod {
        let next = self.next_token("dserror(14): Unexpected identifier.");
        
        match &next.val {
            TokenType::Word(name) => {
                match self.next_token("dserror(14): Unexpected identifier.").val {
                    TokenType::Punc('(') => {
                        let method = self.parse_class_method(name, true, is_static);
                        method
                    },
                    _ => {
                        self.throw_error(pos, "dserror(14): Unexpected identifier.");
                        ClassMethod::default()
                    }
                }
            },
            _ => {
                self.throw_error(pos, "dserror(14): Unexpected identifier.");
                ClassMethod::default()
            }
        }
    }

}