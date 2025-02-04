use std::iter::Peekable;

use crate::alt::{ast::*, lexer::Lexer, token::Token};

use super::ast::{
    expression::{Call, Function, If, Infix, Prefix},
    statement::{Block, Let, Return},
};

#[cfg(test)]
mod test;

#[derive(PartialEq, PartialOrd)]
enum PrecedenceLevel {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // >,<
    Sum = 3,         // +,-
    Product = 4,     // *,/
    Prefix = 5,      // -,+,!
    Call = 6,        // add(1,2)
}

impl PrecedenceLevel {
    fn from_token(token: &Token) -> Self {
        match token {
            Token::Eq => PrecedenceLevel::Equals,
            Token::NotEq => PrecedenceLevel::Equals,
            Token::Lt => PrecedenceLevel::LessGreater,
            Token::Gt => PrecedenceLevel::LessGreater,
            Token::Plus => PrecedenceLevel::Sum,
            Token::Minus => PrecedenceLevel::Sum,
            Token::Slash => PrecedenceLevel::Product,
            Token::Asterisk => PrecedenceLevel::Product,
            Token::Lparen => PrecedenceLevel::Call,
            _ => PrecedenceLevel::Lowest,
        }
    }
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    error: String,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            error: "".to_string(),
        }
    }

    pub fn parse_program(&mut self) -> Result<Program, String> {
        let mut program = Program { statements: vec![] };

        while let Some(stmt) = self.parse_statement() {
            program.statements.push(stmt);
        }

        if self.error.is_empty() {
            Ok(program)
        } else {
            Err(format!("error parsing the program: {}", self.error))
        }
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        let to_call = match self.lexer.peek() {
            Some(Token::Return) => Self::parse_return_statement,
            Some(Token::Let) => Self::parse_let_statement,
            Some(Token::Lbrace) => Self::parse_block_as_statement,
            Some(_) => Self::parse_expression_statement,
            None => return None,
        };

        match to_call(self) {
            Ok(statement) => return Some(statement),
            Err(msg) => {
                self.error = msg;
                return None;
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        self.lexer.next(); // skip the return
        let ret_expr = self.parse_expression(PrecedenceLevel::Lowest)?;
        self.skip_semicolon();
        Ok(Statement::Return(Return { ret_expr }))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        self.lexer.next(); // skip the let
        let name = self
            .lexer
            .next()
            .ok_or("no tokens after `let'".to_string())
            .and_then(|token| {
                if let Token::Identifier(idf) = token {
                    Ok(idf)
                } else {
                    Err(format!(
                        "error parsing let statement: expected identifier, got {token:?}"
                    ))
                }
            })?;

        self.lexer
            .next()
            .ok_or(format!("no tokens after `let {name}', expected `='"))
            .and_then(|token| {
                if let Token::Assign = token {
                    let value = self
                        .parse_expression(PrecedenceLevel::Lowest)
                        .map_err(|e| format!("wrong expression for the let statement: {e}"))?;
                    self.skip_semicolon();
                    Ok(Statement::Let(Let { name, value }))
                } else {
                    Err(format!(
                        "error parsing let statement: expected `=', got {token}"
                    ))
                }
            })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        /* the whole expression has the lowest precedence */
        self.parse_expression(PrecedenceLevel::Lowest)
            .map_err(|e| format!("error parsing expression: {e}"))
            .map(|expr| {
                self.skip_semicolon();
                Statement::Expression(expr)
            })
    }

    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Result<Expression, String> {
        let mut left_expr = match self.lexer.next() {
            Some(Token::Identifier(idf)) => Ok(Expression::Identifier(idf)),
            Some(Token::Int(literal)) => literal
                .parse()
                .map_err(|e| (format!("cannot parse {literal} as integer(i64): {e:?}")))
                .map(|value| Expression::Integer(value)),
            Some(operator @ Token::Bang) | Some(operator @ Token::Minus) => self
                .parse_expression(PrecedenceLevel::Prefix)
                .map(|operand| {
                    Expression::Prefix(Prefix {
                        operator,
                        operand: Box::new(operand),
                    })
                }),
            Some(Token::True) => Ok(Expression::Boolean(true)),
            Some(Token::False) => Ok(Expression::Boolean(false)),
            Some(Token::String(val)) => Ok(Expression::String(val)),
            Some(Token::Lparen) => self
                .parse_groupped_expression()
                .map_err(|e| format!("(groupped expression): {e}")),
            Some(Token::If) => self
                .parse_if_expression()
                .map_err(|e| format!("(if expression): {e}")),
            Some(Token::Function) => self
                .parse_fn_expression()
                .map_err(|e| format!("(fn expression): {e}")),
            Some(Token::Semicolon) => Ok(Expression::Empty),
            Some(etc) => Err(format!("no prefix parsing fn for token kind {etc:?}")),
            None => Err(format!("no token to parse an expression")),
        }?;

        while let Some(token) = self.lexer.peek() {
            let infix_fn = match token {
                Token::Lparen => Self::parse_fn_call,
                Token::Plus
                | Token::Minus
                | Token::Asterisk
                | Token::Slash
                | Token::Eq
                | Token::NotEq
                | Token::Lt
                | Token::Gt => Self::parse_infix_expression,
                Token::Semicolon | Token::Rparen | Token::Comma => return Ok(left_expr),
                token => {
                    return Err(format!(
                        "unknown infix operator: {token}, left expr: {left_expr:?}",
                    ))
                }
            };
            if precedence >= PrecedenceLevel::from_token(&token) {
                return Ok(left_expr);
            }
            // there's a function to apply
            left_expr =
                infix_fn(self, left_expr).map_err(|e| format!("(infix expression): {e}"))?;
        }

        // reached the end of program
        Ok(left_expr)
    }

    fn parse_groupped_expression(&mut self) -> Result<Expression, String> {
        let expression = self
            .parse_expression(PrecedenceLevel::Lowest)
            .map_err(|e| format!("inner expression parsing failed: {e}"))?;
        match self.lexer.next() {
            Some(Token::Rparen) => Ok(expression),
            etc => Err(format!("expected `)', got {etc:?}")),
        }
    }

    fn parse_if_expression(&mut self) -> Result<Expression, String> {
        let cond = self
            .lexer
            .next()
            .ok_or(format!("no token after `if' to parse"))
            .and_then(|token| {
                if Token::Lparen == token {
                    Ok(self
                        .parse_groupped_expression()
                        .map_err(|e| format!("cannot parse condition of the if expression: {e}"))?)
                } else {
                    Err(format!("expected ( for if condition, got {token:?}"))
                }
            })?;

        match self.lexer.next() {
            Some(Token::Lbrace) => (),
            etc => return Err(format!("invalid syntax: expected `{{', got {etc:?}")),
        }

        let cons = self
            .parse_block_statement()
            .map_err(|e| format!("error parsing consequence: {e}"))?;

        let alt = if let Some(Token::Else) = self.lexer.peek() {
            self.lexer.next(); // skip the else
            match self.lexer.next() {
                Some(Token::Lbrace) => Some(
                    self.parse_block_statement()
                        .map_err(|e| format!("else's block: {e}"))?,
                ),
                token => return Err(format!("expected `else {{', got `else {token:?}'")),
            }
        } else {
            // there's no `else'
            None
        };

        Ok(Expression::If(If {
            condition: Box::new(cond),
            consequence: cons,
            alternative: alt,
        }))
    }

    fn parse_fn_expression(&mut self) -> Result<Expression, String> {
        let parameters = self
            .lexer
            .next()
            .ok_or(format!("no token after `fn'",))
            .and_then(|token| {
                if token == Token::Lparen {
                    self.parse_fn_params()
                } else {
                    Err("no `(' after `fn'".to_string())
                }
            })?;

        let body = self
            .lexer
            .next()
            .ok_or(format!(
                "no tokens after fn's parameters fn({})",
                parameters.join(",")
            ))
            .and_then(|token| {
                if token == Token::Lbrace {
                    self.parse_block_statement()
                } else {
                    Err(format!("no `{{' after fn({})", parameters.join(",")))
                }
            })?;
        Ok(Expression::Fn(Function { body, parameters }))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<String>, String> {
        let mut params = vec![];

        if self.lexer.peek() == Some(&Token::Rparen) {
            self.lexer.next();
            return Ok(params);
        }

        while let Some(Token::Identifier(value)) = self.lexer.next() {
            params.push(value);
            match self.lexer.next() {
                Some(Token::Comma) => continue,
                Some(Token::Rparen) => return Ok(params),
                token => {
                    return Err(format!(
                        "wrong parameters separator: expected `,' or `)', got {token:?}"
                    ))
                }
            }
        }

        Err(format!(
            "incorrect parameters declaration: missing `,' or `)'"
        ))
    }

    fn parse_block_as_statement(&mut self) -> Result<Statement, String> {
        self.lexer.next();
        Ok(Statement::Block(self.parse_block_statement()?))
    }

    fn parse_block_statement(&mut self) -> Result<Block, String> {
        let mut block = Block { statements: vec![] };

        if self.lexer.peek() == Some(&Token::Rbrace) {
            self.lexer.next();
            return Ok(block);
        }

        while let Some(stmt) = self.parse_statement() {
            block.statements.push(stmt);
            if let Some(Token::Rbrace) = self.lexer.peek() {
                self.lexer.next(); // skip the }
                return Ok(block);
            }
        }

        if self.error.is_empty() {
            Err(format!("block is not closed with `}}'"))
        } else {
            Err(self.error.drain(..).collect())
        }
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let operator = self
            .lexer
            .next()
            .ok_or(format!("no token avaiable for an operator after {left:?}"))?;

        let precedence = PrecedenceLevel::from_token(&operator);
        self.parse_expression(precedence).map(|right| {
            Expression::Infix(Infix {
                operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn parse_fn_call(&mut self, function: Expression) -> Result<Expression, String> {
        Ok(Expression::Call(Call {
            function: Box::new(function),
            arguments: self.parse_fn_call_args()?,
        }))
    }

    fn parse_fn_call_args(&mut self) -> Result<Vec<Expression>, String> {
        let mut args = vec![];

        match self.lexer.next() {
            Some(Token::Lparen) => {
                if let Some(Token::Rparen) = self.lexer.peek() {
                    self.lexer.next();
                    return Ok(args);
                }
                loop {
                    let expr = self
                        .parse_expression(PrecedenceLevel::Lowest)
                        .map_err(|e| {
                            format!("error parsing fn call args: {e} (already parsed: {args:?})")
                        })?;
                    args.push(expr);

                    match self.lexer.next() {
                        Some(Token::Comma) => (),               // usual separator
                        Some(Token::Rparen) => return Ok(args), // end of args
                        Some(etc) => return Err(format!("wrong fn call args separator: {etc:?}")),
                        None => return Err(format!("found fn call args {args:?} but no `)' seen")),
                    };
                }
            }
            etc => Err(format!("missing `(' after function name, found {etc:?}")),
        }
    }

    pub fn skip_semicolon(&mut self) {
        if self
            .lexer
            .peek()
            .is_some_and(|val| *val == Token::Semicolon)
        {
            self.lexer.next();
        }
    }
}
