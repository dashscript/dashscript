use std::fmt::{self, Display, Formatter};
use crate::{
    Token, TokenKind, Position, Lexer, Stmt, ConstantPool, Expr, Keyword,
    LexerErrorKind, AssignOp, BinOp
};

macro_rules! unexpected_token {
    ($self:expr, $error:expr, $token:expr) => {{
        match $token {
            Token { kind: TokenKind::Error(error), position } => $self.error(position, ASTErrorKind::LexerError(error)),
            Token { position, .. } => $self.error(position, $error),
        }
    }};
}

#[derive(Debug, Clone, Default)]
pub struct AST {
    pub lexer: Lexer,
    constant_pool: ConstantPool,
    statements: Vec<Stmt>,
    current: Token,
    errors: Vec<ASTError>,
    imports: Vec<u32>,
    had_error: bool
}

#[derive(Debug, Clone, Default)]
pub struct ASTBuild {
    pub constant_pool: ConstantPool,
    pub statements: Vec<Stmt>,
    pub imports: Vec<u32>,
    pub body: String
}

impl AST {

    pub fn new(filename: &String, body: &String) -> Self {
        AST { 
            lexer: Lexer::new(filename, body),
            ..Default::default()
        }
    }

    pub fn compile(filename: &String, body: &String) -> Result<ASTBuild, Vec<ASTError>> {
        let mut ast = Self::new(filename, body);
        ast.parse();

        if ast.had_error {
            Err(ast.errors)
        } else {
            Ok(ast.into_build())
        }
    }

    pub fn parse(&mut self) {
        let mut token = self.lexer.next().unwrap_or_default();
        self.current = token.clone();

        loop {
            // There might be a chance which can cause many errors
            // due to the original first one. This is used to prevent
            // the overflow of duplicate chained errors.
            if self.had_error {
                return;
            }

            match token.kind {
                TokenKind::Keyword(keyword) => {
                    let index = token.position.start as usize;
                    let keyword = match keyword {
                        Keyword::Let => self.keyword_assign(index, false),
                        Keyword::Const => self.keyword_assign(index, true),
                        Keyword::Import => self.keyword_import(index),
                        Keyword::Func => self.keyword_function(index, false),
                        Keyword::Try => self.keyword_try(index),
                        Keyword::Return => self.keyword_return(index),
                        Keyword::While => self.keyword_while(index),
                        Keyword::For => self.keyword_for(index),
                        Keyword::Break => Stmt { expr: Expr::Break, index },
                        Keyword::Continue => Stmt { expr: Expr::Continue, index },
                        Keyword::If => {
                            let statement = self.keyword_if(index);
                            self.statements.push(statement);
                            token = self.current.clone();
                            continue;
                        },
                        _ => Stmt::default()
                    };

                    self.statements.push(keyword);
                },
                TokenKind::Word(name) => {
                    let mut expr = Expr::Word(self.constant_pool.add_string(name));
                    expr = self.expression_with_prefix(expr, true);
                    self.statements.push(Stmt { expr, index: token.position.start as usize });
                    match self.current.kind {
                        TokenKind::Semicolon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current)
                    }
                },
                TokenKind::Semicolon => (),
                TokenKind::Error(error) => self.error(token.position, ASTErrorKind::LexerError(error)),
                _ => {
                    let index = token.position.start as usize;
                    let expr = self.expression_with_token(token, ASTErrorKind::UnexpectedExpr);
                    self.statements.push(Stmt { expr, index });

                    match &self.current.kind {
                        TokenKind::Semicolon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current)
                    }
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

        if self.had_error {
            return;
        }
    }
    
    pub fn error(&mut self, position: Position, kind: ASTErrorKind) {
        let (line, col) = get_line_col(&self.lexer.body, position.start as usize);
        self.had_error = true;
        self.errors.push(ASTError {
            line, 
            col, 
            kind,
            filename: self.lexer.filename.clone(),
            body: self.lexer.body[position.start as usize..position.end as usize].to_string()
        });
    }

    pub fn keyword_return(&mut self, index: usize) -> Stmt {
        let next = self.next_token();
        let expr = match next.kind {
            TokenKind::Semicolon => Expr::Return(Box::new(Expr::Null)),
            _ => {
                let expr = self.expression_with_token(next, ASTErrorKind::UnexpectedExpr);
                if let TokenKind::Semicolon = self.current.kind {
                    return Stmt { expr: Expr::Return(Box::new(expr)), index };
                }
                
                self.error(self.current.position, ASTErrorKind::ExpectedSemicolon);
                Expr::Null
            }
        };

        Stmt { expr, index }
    }

    pub fn keyword_import(&mut self, index: usize) -> Stmt {
        let name_token = self.next_token();
        let name = self.constant_pool.add_string(match name_token.kind {
            TokenKind::String(name) => name,
            TokenKind::Word(name) => name,
            _ => {
                unexpected_token!(self, ASTErrorKind::ImproperImport, self.current);
                return Stmt::default();
            }
        });

        self.imports.push(name);
        match self.next_token().kind {
            TokenKind::Semicolon => {
                Stmt { expr: Expr::Import { module: name, as_: None }, index }
            },
            TokenKind::Keyword(Keyword::As) => {
                match self.next_token().kind {
                    TokenKind::Word(as_name) => {
                        Stmt {
                            expr: Expr::Import {
                                module: name,
                                as_: Some(self.constant_pool.add_string(as_name))
                            }, index
                        }
                    },
                    _ => {
                        unexpected_token!(self, ASTErrorKind::ImproperImport, self.current);
                        Stmt::default()
                    }
                }
            },
            _ => {
                unexpected_token!(self, ASTErrorKind::ImproperImport, self.current);
                Stmt::default()
            }
        }
    }

    pub fn keyword_assign(&mut self, index: usize, constant: bool) -> Stmt {
        let name = match self.next_token().kind {
            TokenKind::Word(name) => name,
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                return Stmt::default();
            }
        };

        match self.next_token().kind {
            TokenKind::Assign => (),
            TokenKind::Semicolon if !constant => {
                return Stmt {
                    expr: Expr::Store(self.constant_pool.add_string(name), Box::new(Expr::Null), false), 
                    index
                }
            },
            _ => unexpected_token!(self, ASTErrorKind::ExpectedAssignmentOperator, self.current)
        };

        let value = self.expression(ASTErrorKind::ExpectedValue);
        if !matches!(self.current.kind, TokenKind::Semicolon) {
            unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current);
        }

        Stmt {
            expr: Expr::Store(self.constant_pool.add_string(name), Box::new(value), constant), 
            index
        }
    }

    pub fn keyword_try(&mut self, index: usize) -> Stmt {
        match self.next_token().kind {
            TokenKind::CurlyBraceOpen => (),
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                return Stmt::default();
            }
        };

        let try_inner = self.expression_block();
        match (self.next_token().kind, self.next_token().kind) {
            (TokenKind::Keyword(Keyword::Expect), TokenKind::CurlyBraceOpen) => (),
            _ => {
                unexpected_token!(self, ASTErrorKind::ImproperTryStatement, self.current);
                return Stmt::default();
            }
        };

        let expect_inner = self.expression_block();
        Stmt { expr: Expr::Try { try_inner, expect_inner }, index }
    }

    pub fn keyword_if(&mut self, index: usize) -> Stmt {
        let mut branches = Vec::new();
        let mut else_branch = None;

        let condition = self.expression(ASTErrorKind::ExpectedValue);
        let inner = match self.current.kind {
            TokenKind::CurlyBraceOpen => self.expression_block(),
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                return Stmt { expr: Expr::If { branches, else_branch }, index };
            }
        };

        branches.push((condition, inner));

        loop {
            match self.lexer.next() {
                Some(token) => {
                    self.current = token;

                    match self.current.kind {
                        TokenKind::Keyword(Keyword::Elif) => {
                            let condition = self.expression(ASTErrorKind::ExpectedValue);
                            let inner = match self.current.kind {
                                TokenKind::CurlyBraceOpen => self.expression_block(),
                                _ => {
                                    unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                                    return Stmt { expr: Expr::If { branches, else_branch }, index };
                                }
                            };

                            branches.push((condition, inner));
                        },
                        TokenKind::Keyword(Keyword::Else) => {
                            else_branch = Some(match self.next_token().kind {
                                TokenKind::CurlyBraceOpen => self.expression_block(),
                                _ => {
                                    unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                                    return Stmt { expr: Expr::If { branches, else_branch }, index };
                                }
                            });
                        },
                        _ => return Stmt { expr: Expr::If { branches, else_branch }, index }
                    }
                },
                None => break self.current = Token { kind: TokenKind::Semicolon, ..Default::default() }
            }
        }

        Stmt { expr: Expr::If { branches, else_branch }, index }
    }

    pub fn keyword_function(&mut self, index: usize, is_async: bool) -> Stmt {
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

        match self.next_token().kind {
            TokenKind::CurlyBraceOpen => {
                Stmt {
                    expr: Expr::Function {
                        name,
                        parameters,
                        inner: self.expression_block(),
                        is_async
                    },
                    index
                }
            },
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                Stmt::default()
            }
        }
    }

    pub fn keyword_while(&mut self, index: usize) -> Stmt {
        let condition = self.expression(ASTErrorKind::ExpectedValue);
        match self.current.kind {
            TokenKind::CurlyBraceOpen => (),
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                return Stmt { expr: Expr::While(Box::new(condition), Vec::new()), index };
            }
        };

        let inner = self.expression_block();
        Stmt { expr: Expr::While(Box::new(condition), inner), index }
    }

    pub fn keyword_for(&mut self, index: usize) -> Stmt {
        macro_rules! unexpected {
            ($kind:ident) => {{
                unexpected_token!(self, ASTErrorKind::$kind, self.current);
                return Stmt::default();
            }};
        }

        let name = match self.next_token().kind {
            TokenKind::Word(name) => self.constant_pool.add_string(name),
            _ => unexpected!(ExpectedIdent)
        };

        match self.next_token().kind {
            TokenKind::Keyword(Keyword::In) => (),
            _ => unexpected!(ExpectedIn)
        };

        let in_ = self.expression(ASTErrorKind::ExpectedSemicolon);
        match self.current.kind {
            TokenKind::CurlyBraceOpen => (),
            _ => unexpected!(ExpectedBlock)
        };

        let inner = self.expression_block();
        Stmt { expr: Expr::For { name, in_: Box::new(in_), inner }, index }
    }

    pub fn expression(&mut self, kind: ASTErrorKind) -> Expr {
        if let Some(token) = self.lexer.next() {
            self.current = token.clone();

            let prefix = match token.kind {
                TokenKind::String(string) => Expr::String(self.constant_pool.add_string(string)),
                TokenKind::Word(string) => Expr::Word(self.constant_pool.add_string(string)),
                TokenKind::Int(int) => Expr::Int(self.constant_pool.add_int(int)),
                TokenKind::Float(float) => Expr::Float(self.constant_pool.add_float(float)),
                TokenKind::True => Expr::Boolean(true),
                TokenKind::False => Expr::Boolean(false),
                TokenKind::Null => Expr::Null,
                TokenKind::Not => return Expr::Not(Box::new(self.expression(ASTErrorKind::UnexpectedNotOp))),
                TokenKind::SqBraceOpen => Expr::Array(self.expression_array()),
                TokenKind::CurlyBraceOpen => Expr::Dict(self.expression_dict()),
                TokenKind::ParenOpen => {
                    let expr = self.expression(ASTErrorKind::UnclosedParen);
                    if !matches!(self.current.kind, TokenKind::ParenClose) {
                        unexpected_token!(self, ASTErrorKind::UnclosedParen, self.current);
                        return Expr::Null;
                    }

                    expr
                },
                TokenKind::Keyword(Keyword::Func) => self.expression_function(false),
                _ => {
                    self.error(token.position, kind);
                    Expr::Null
                }
            };

            return self.expression_with_prefix(prefix, false);
        }

        self.error(self.current.position, kind);
        Expr::Null
    }

    pub fn expression_with_token(&mut self, token: Token, kind: ASTErrorKind) -> Expr {
        let expr = match token.kind {
            TokenKind::String(string) => Expr::String(self.constant_pool.add_string(string)),
            TokenKind::Word(string) => Expr::Word(self.constant_pool.add_string(string)),
            TokenKind::Int(int) => Expr::Int(self.constant_pool.add_int(int)),
            TokenKind::Float(float) => Expr::Float(self.constant_pool.add_float(float)),
            TokenKind::True => Expr::Boolean(true),
            TokenKind::False => Expr::Boolean(false),
            TokenKind::Null => Expr::Null,
            TokenKind::Not => return Expr::Not(Box::new(self.expression(ASTErrorKind::UnexpectedNotOp))),
            TokenKind::SqBraceOpen => Expr::Array(self.expression_array()),
            TokenKind::CurlyBraceOpen => Expr::Dict(self.expression_dict()),
            TokenKind::ParenOpen => {
                let expr = self.expression(ASTErrorKind::UnclosedParen);
                if !matches!(self.current.kind, TokenKind::ParenClose) {
                    unexpected_token!(self, ASTErrorKind::UnclosedParen, self.current);
                    return Expr::Null;
                }

                expr
            },
            TokenKind::Keyword(Keyword::Func) => self.expression_function(false),
            _ => {
                unexpected_token!(self, kind, token);
                Expr::Null
            }
        };

        self.expression_with_prefix(expr, false)
    }

    pub fn expression_with_prefix(&mut self, mut expr: Expr, can_assign: bool) -> Expr {
        self.current = match self.lexer.next() {
            Some(token) => token,
            None => return expr
        };

        macro_rules! bin_op {
            ($op:ident) => {{
                expr = Expr::BinaryOperation {
                    lhs: Box::new(expr),
                    rhs: Box::new(self.expression(ASTErrorKind::UnexpectedExpr)),
                    op: BinOp::$op
                };

                continue;
            }};
        }

        loop {
            match self.current.kind {
                TokenKind::Dot => {
                    let token = self.next_token();
                    let attr = match token.kind {
                        TokenKind::Word(word) => Expr::String(self.constant_pool.add_string(word)),
                        TokenKind::Int(int) => Expr::Int(self.constant_pool.add_int(int)),
                        _ => {
                            unexpected_token!(self, ASTErrorKind::ExpectedPropertyExpr, token);
                            Expr::Null
                        }
                    };

                    expr = Expr::Attribute(Box::new(expr), Box::new(attr));
                },
                TokenKind::SqBraceOpen => {
                    expr = Expr::Attribute(Box::new(expr), Box::new(self.expression(ASTErrorKind::ImproperPropertyIndexing)));
                    match self.current.kind {
                        TokenKind::SqBraceClose => (),
                        _ => unexpected_token!(self, ASTErrorKind::ImproperPropertyIndexing, self.current)
                    }
                },
                TokenKind::ParenOpen => expr = Expr::Call(Box::new(expr), self.expression_call()),
                TokenKind::Add => bin_op!(Add),
                TokenKind::Sub => bin_op!(Subtract),
                TokenKind::Mul => bin_op!(Multiply),
                TokenKind::Div => bin_op!(Divide),
                TokenKind::Pow => bin_op!(Power),
                TokenKind::Rem => bin_op!(Rem),
                TokenKind::And => bin_op!(And),
                TokenKind::BitAnd => bin_op!(BitAnd),
                TokenKind::BitOr => bin_op!(BitOr),
                TokenKind::BitXor => bin_op!(BitXor),
                TokenKind::Shr => bin_op!(Shr),
                TokenKind::Shl => bin_op!(Shl),
                TokenKind::Or => bin_op!(Or),
                TokenKind::Equal => bin_op!(Equal),
                TokenKind::NotEqual => bin_op!(NotEqual),
                TokenKind::GreaterThan => bin_op!(GreaterThan),
                TokenKind::GreaterThanOrEqual => bin_op!(GreaterThanOrEqual),
                TokenKind::LessThan => bin_op!(LessThan),
                TokenKind::LessThanOrEqual => bin_op!(LessThanOrEqual),
                TokenKind::Question => {
                    let truthy = self.expression(ASTErrorKind::ImproperTernaryOperator);
                    match self.current.kind {
                        TokenKind::Colon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ImproperTernaryOperator, self.current)
                    }

                    let falsy = self.expression(ASTErrorKind::ImproperTernaryOperator);
                    expr = Expr::Ternary(Box::new(expr), Box::new(truthy), Box::new(falsy));
                    continue;
                },
                TokenKind::Assign if can_assign => {
                    return Expr::Assign {
                        target: Box::new(expr),
                        op: AssignOp::Assign,
                        value: Box::new(self.expression(ASTErrorKind::ExpectedValue))
                    }
                },
                TokenKind::AssignAdd if can_assign => {
                    return Expr::Assign {
                        target: Box::new(expr),
                        op: AssignOp::Add,
                        value: Box::new(self.expression(ASTErrorKind::ExpectedValue))
                    }
                },
                TokenKind::AssignSub if can_assign => {
                    return Expr::Assign {
                        target: Box::new(expr),
                        op: AssignOp::Sub,
                        value: Box::new(self.expression(ASTErrorKind::ExpectedValue))
                    }
                },
                _ => return expr
            }

            self.current = match self.lexer.next() {
                Some(token) => token,
                None => return expr
            };
        }
    }

    pub fn expression_function(&mut self, is_async: bool) -> Expr {
        let parameters = match self.next_token().kind {
            TokenKind::ParenOpen => self.expression_function_params(),
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedIdent, self.current);
                return Expr::Null;
            }
        };

        match self.next_token().kind {
            TokenKind::CurlyBraceOpen => {
                Expr::Function {
                    name: 0,
                    parameters,
                    inner: self.expression_block(),
                    is_async
                }
            },
            _ => {
                unexpected_token!(self, ASTErrorKind::ExpectedBlock, self.current);
                Expr::Null
            }
        }
    }

    pub fn expression_call(&mut self) -> Vec<Expr> {
        let mut params = Vec::new();

        while let Some(token) = self.lexer.next() {
            self.current = token;

            match self.current.kind {
                TokenKind::ParenClose => return params,
                _ => {
                    params.push(self.expression_with_token(self.current.clone(), ASTErrorKind::UnexpectedExpr));
                    match self.current.kind {
                        TokenKind::ParenClose => return params,
                        TokenKind::Comma => (),
                        _ => {
                            unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current);
                            return params;
                        }
                    }
                }
            }
        }

        self.error(self.current.position, ASTErrorKind::UnclosedParen);
        params
    }

    pub fn expression_array(&mut self) -> Vec<Expr> {
        let mut items = Vec::new();

        while let Some(token) = self.lexer.next() {
            self.current = token;

            match self.current.kind {
                TokenKind::SqBraceClose => return items,
                _ => {
                    items.push(self.expression_with_token(self.current.clone(), ASTErrorKind::UnexpectedExpr));
                    match self.current.kind {
                        TokenKind::SqBraceClose => return items,
                        TokenKind::Comma => (),
                        _ => {
                            unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current);
                            return items;
                        }
                    }
                }
            }
        }

        self.error(self.current.position, ASTErrorKind::UnclosedParen);
        items
    }

    pub fn expression_dict(&mut self) -> Vec<(u32, Expr)> {
        let mut items = Vec::new();

        while let Some(token) = self.lexer.next() {
            self.current = token;

            match self.current.kind.clone() {
                TokenKind::CurlyBraceClose => return items,
                TokenKind::String(string) => {
                    match self.next_token().kind {
                        TokenKind::Colon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedColon, self.current)
                    }

                    let key = self.constant_pool.add_string(string);
                    let value = self.expression(ASTErrorKind::ExpectedValue);
                    items.push((key, value));

                    match self.current.kind {
                        TokenKind::CurlyBraceClose => return items,
                        TokenKind::Comma => (),
                        _ => unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current)
                    };
                },
                TokenKind::Word(word) => {
                    let key = self.constant_pool.add_string(word);
                    match self.next_token().kind {
                        TokenKind::Colon => (),
                        TokenKind::Comma => items.push((key, Expr::Word(key))),
                        TokenKind::CurlyBraceClose => {
                            items.push((key, Expr::Word(key)));
                            return items;
                        },
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedColon, self.current)
                    }

                    let value = self.expression(ASTErrorKind::ExpectedValue);
                    items.push((key, value));

                    match self.current.kind {
                        TokenKind::CurlyBraceClose => return items,
                        TokenKind::Comma => (),
                        _ => unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current)
                    };
                },
                _ => unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current)
            }
        }

        self.error(self.current.position, ASTErrorKind::UnclosedParen);
        items
    }

    pub fn expression_block(&mut self) -> Vec<Stmt> {
        let mut statements = Vec::new();
        let mut token = self.lexer.next().unwrap();

        loop {
            // There might be a chance which can cause many errors
            // due to the original first one. This is used to prevent
            // the overflow of duplicate chained errors.
            if self.had_error {
                return statements;
            }

            match token.kind {
                TokenKind::Keyword(keyword) => {
                    let index = token.position.start as usize;
                    let keyword = match keyword {
                        Keyword::Let => self.keyword_assign(index, false),
                        Keyword::Const => self.keyword_assign(index, true),
                        Keyword::Import => self.keyword_import(index),
                        Keyword::Func => self.keyword_function(index, false),
                        Keyword::Try => self.keyword_try(index),
                        Keyword::Return => self.keyword_return(index),
                        Keyword::While => self.keyword_while(index),
                        Keyword::For => self.keyword_for(index),
                        Keyword::Break => Stmt { expr: Expr::Break, index },
                        Keyword::Continue => Stmt { expr: Expr::Continue, index },
                        Keyword::If => {
                            let statement = self.keyword_if(index);
                            statements.push(statement);
                            token = self.current.clone();
                            continue;
                        },
                        _ => Stmt::default()
                    };

                    statements.push(keyword);
                },
                TokenKind::Word(name) => {
                    let mut expr = Expr::Word(self.constant_pool.add_string(name));
                    expr = self.expression_with_prefix(expr, true);
                    statements.push(Stmt { expr, index: token.position.start as usize });
                    match self.current.kind {
                        TokenKind::Semicolon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current)
                    }
                },
                TokenKind::Semicolon => (),
                TokenKind::CurlyBraceClose => return statements,
                TokenKind::Error(error) => self.error(token.position, ASTErrorKind::LexerError(error)),
                _ => {
                    let index = token.position.start as usize;
                    let expr = self.expression_with_token(token, ASTErrorKind::UnexpectedExpr);
                    statements.push(Stmt { expr, index });

                    match &self.current.kind {
                        TokenKind::Semicolon => (),
                        _ => unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current)
                    }
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
        statements
    }

    pub fn expression_function_params(&mut self) -> Vec<u32> {
        let mut params = Vec::new();

        while let Some(token) = self.lexer.next() {
            self.current = token;

            match self.current.kind.clone() {
                TokenKind::ParenClose => return params,
                TokenKind::Word(name) => {
                    params.push(self.constant_pool.add_string(name));
                    match self.next_token().kind {
                        TokenKind::ParenClose => return params,
                        TokenKind::Comma => (),
                        _ => {
                            unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current);
                            return params;
                        }
                    }
                },
                _ => unexpected_token!(self, ASTErrorKind::UnexpectedExpr, self.current)
            }
        }

        self.error(self.current.position, ASTErrorKind::UnclosedParen);
        params
    }

    pub fn validate_current_semicolon(&mut self) {
        match &self.current.kind {
            TokenKind::Semicolon => (),
            _ => unexpected_token!(self, ASTErrorKind::ExpectedSemicolon, self.current)
        }
    }

    pub fn next_token(&mut self) -> Token {
        match self.lexer.next() {
            Some(token) => {
                self.current = token.clone();
                token
            },
            None => {
                self.error(self.current.position, ASTErrorKind::UnexpectedEof);
                Token::default()
            }
        }
    }

    pub fn into_build(self) -> ASTBuild {
        ASTBuild {
            statements: self.statements,
            imports: self.imports,
            constant_pool: self.constant_pool,
            body: self.lexer.body
        }
    }

}

#[derive(Debug, Clone, Copy)]
pub enum ASTErrorKind {
    LexerError(LexerErrorKind),
    UnexpectedNotOp,
    UnexpectedEof,
    UnexpectedExpr,
    UselessAwait,
    UnclosedParen,
    ImproperPropertyIndexing,
    ImproperTernaryOperator,
    ImproperImport,
    ImproperTryStatement,
    ExpectedValue,
    ExpectedColon,
    ExpectedIdent,
    ExpectedAssignmentOperator,
    ExpectedBlock,
    ExpectedPropertyExpr,
    ExpectedSemicolon,
    ExpectedIn
}

#[derive(Debug, Clone)]
pub struct ASTError {
    line: usize,
    col: usize,
    filename: String,
    body: String,
    kind: ASTErrorKind
}

impl Display for ASTError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self.kind {
            ASTErrorKind::LexerError(error) => write!(f, "{}", error.to_string()),
            ASTErrorKind::UnexpectedNotOp => write!(f, "Never expected a \"!\" (not) operator."),
            ASTErrorKind::UnexpectedEof => write!(f, "Never expected an eof."),
            ASTErrorKind::UnexpectedExpr => write!(f, "Never expected an expression."),
            ASTErrorKind::UselessAwait => write!(f, "Statement \"await\" which is useless to exist."),
            ASTErrorKind::UnclosedParen => write!(f, "Found an unclosed bracket."),
            ASTErrorKind::ImproperPropertyIndexing => write!(f, "Property indexing is improper. Proper: \"object['key']\""),
            ASTErrorKind::ImproperTernaryOperator => write!(f, "Ternary operator is improper. Proper: \"target ? truthy : falsy\""),
            ASTErrorKind::ImproperImport => write!(f, "Import statement has an improper pattern of syntax."),
            ASTErrorKind::ImproperTryStatement => write!(f, "Improper try statement. Proper: \"try {{}} expect {{}}\""),
            ASTErrorKind::ExpectedValue => write!(f, "Expected a value here."),
            ASTErrorKind::ExpectedColon => write!(f, "Expected a \":\" (colon)."),
            ASTErrorKind::ExpectedIdent => write!(f, "Expected an identifier here."),
            ASTErrorKind::ExpectedAssignmentOperator => write!(f, "Expected any one of \"=\", \"+=\" or \"-=\" (assignment operators)."),
            ASTErrorKind::ExpectedBlock => write!(f, "Expected a \"{{\" (block) here."),
            ASTErrorKind::ExpectedPropertyExpr => write!(f, "Expected identifier after property delaration. Proper: \"object.key\"."),
            ASTErrorKind::ExpectedSemicolon => write!(f, "Expected a \";\" (semicolon) here."),
            ASTErrorKind::ExpectedIn => write!(f, "Expected \"in\" keyword.")
        }
    }
}

impl ASTError {
    pub fn kind(&self) -> ASTErrorKind { self.kind }
    pub fn body(&self) -> String { self.body.clone() }
    pub fn position(&self) -> (String, usize, usize) {
        (self.filename.clone(), self.line, self.col)
    }
}

fn get_line_col(body: &String, start: usize) -> (usize, usize) {
    let lines: Vec<&str> = body.split("\n").collect();
    let mut i = 0;
    for line in 0..lines.len() {
        i += lines[line].len() + 1;
        if i > start {
            return (line+1, i - start);
        }
    }

    (lines.len(), 0)
}