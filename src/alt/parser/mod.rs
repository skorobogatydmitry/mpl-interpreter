use std::iter::Peekable;

use crate::alt::{ast::*, lexer::Lexer, token::Token};

use super::ast::{
    expression::{Boolean, Call, Function, Identifier, If, Infix, Integer, Prefix},
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
    pub errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Self {
            lexer: lexer.peekable(),
            errors: vec![],
        }
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while let Some(stmt) = self.parse_statement() {
            program.statements.push(stmt);
        }

        Some(program)
    }

    // TODO: review why there could be None and replace with Result if applicable
    fn parse_statement(&mut self) -> Option<Statement> {
        let to_call = match self.lexer.peek() {
            Some(Token::Return) => {
                self.lexer.next(); // skip the return
                Self::parse_return_statement
            }
            Some(Token::Let) => {
                self.lexer.next(); // skip the let
                Self::parse_let_statement
            }
            Some(_) => Self::parse_expression_statement,
            None => return None,
        };

        match to_call(self) {
            Ok(statement) => return Some(statement),
            Err(msg) => {
                self.errors.push(msg);
                return None;
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<Statement, String> {
        let ret_expr = self.parse_expression(PrecedenceLevel::Lowest)?;
        self.skip_semicolon();
        Ok(Statement::Return(Return {
            ret_expr: Some(ret_expr),
        }))
    }

    fn parse_let_statement(&mut self) -> Result<Statement, String> {
        let name = self
            .lexer
            .next()
            .ok_or("no tokens after `let'".to_string())
            .and_then(|token| {
                if let Token::Identifier(idf) = token {
                    Ok(Identifier { value: idf })
                } else {
                    Err(format!(
                        "incorrect let statement: expected identifier, got {token:?}"
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
                        .map_err(|e| format!("error parsing let statement: {e}"))?;
                    self.skip_semicolon();
                    Ok(Statement::Let(Let {
                        name,
                        value: Some(value),
                    }))
                } else {
                    Err(format!("expected `=', got {token}"))
                }
            })
    }

    fn parse_expression_statement(&mut self) -> Result<Statement, String> {
        /* the whole expression has the lowest precedence */
        self.parse_expression(PrecedenceLevel::Lowest)
            .map_err(|e| format!("cannot parse expression: {e}"))
            .map(|expr| {
                self.skip_semicolon();
                Statement::Expression(statement::ExpressionStatement {
                    expression: Some(expr),
                })
            })
    }

    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Result<Expression, String> {
        let mut left_expr = match self.lexer.next() {
            Some(Token::Identifier(idf)) => Ok(Expression::Identifier(Identifier { value: idf })),
            Some(Token::Int(literal)) => self
                .parse_integer_literal(literal)
                .ok_or(format!("error parsing integer literal: TODO")),
            Some(operator @ Token::Bang) | Some(operator @ Token::Minus) => self
                .parse_expression(PrecedenceLevel::Prefix)
                .map(|operand| {
                    Expression::Prefix(Prefix {
                        operator,
                        operand: Box::new(operand),
                    })
                }),
            Some(Token::True) => Ok(Expression::Boolean(Boolean { value: true })),
            Some(Token::False) => Ok(Expression::Boolean(Boolean { value: false })),
            Some(Token::Lparen) => self
                .parse_groupped_expression()
                .map_err(|e| format!("error parsing a groupped expression: {e}")),
            Some(Token::If) => self
                .parse_if_expression()
                .ok_or(format!("error parsing an if expression: TODO")),
            Some(Token::Function) => self
                .parse_fn_expression()
                .ok_or(format!("error parsing a fn expression: TODO")),
            Some(Token::Semicolon) => Ok(Expression::Empty),
            Some(etc) => Err(format!("no prefix parsing fn for token kind {etc:?}")),
            None => Err(format!("no token to parse an expression")),
        }?;

        while let Some(token) = self.lexer.peek() {
            if precedence >= PrecedenceLevel::from_token(&token) {
                return Ok(left_expr);
            }
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
                _ => return Ok(left_expr),
            };
            // there's a function to apply
            left_expr = infix_fn(self, left_expr)
                .map_err(|e| format!("unable to parse an infix expression: {e}"))?;
        }

        // reached the end of program
        Ok(left_expr)
    }

    fn parse_integer_literal(&mut self, literal: String) -> Option<Expression> {
        match literal.parse() {
            Ok(val) => Some(Expression::Integer(Integer { value: val })),
            Err(e) => {
                self.errors
                    .push(format!("cannot parse {} as i64: {:?}", literal, e));
                None
            }
        }
    }

    fn parse_groupped_expression(&mut self) -> Result<Expression, String> {
        let expression = self.parse_expression(PrecedenceLevel::Lowest);
        match self.lexer.next() {
            Some(Token::Rparen) => {
                expression.map_err(|e| format!("inner expression parsing failed: {e}"))
            }
            etc => Err(format!("expected `)`, got {etc:?}")),
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let cond = if let Some(next_token) = self.lexer.next() {
            if next_token != Token::Lparen {
                self.errors
                    .push(format!("expected ( for if condition, got {next_token}",));
                return None;
            }
            if let Ok(cond) = self.parse_groupped_expression() {
                cond
            } else {
                self.errors
                    .push(format!("cannot parse condition of the if expression"));
                return None;
            }
        } else {
            self.errors.push(format!("no token for the if condition"));
            return None;
        };

        match self.lexer.next() {
            Some(Token::Lbrace) => (),
            etc => {
                self.errors
                    .push(format!("invalid syntax: expected `{{', got {etc:?}"));
                return None;
            }
        }

        let cons = match self.parse_block_statement() {
            Ok(block) => block,
            Err(_) => return None,
        };

        let alt = if let Some(Token::Else) = self.lexer.peek() {
            self.lexer.next(); // skip the else
            if let Some(Token::Lbrace) = self.lexer.next() {
                match self.parse_block_statement() {
                    Ok(block) => Some(block),
                    Err(_) => return None,
                }
            } else {
                self.errors.push(format!(
                    "invalid syntax: expected `else {{', got `else {:?}'",
                    self.lexer.peek()
                ));
                return None;
            }
        } else {
            // there's no else
            None
        };

        Some(Expression::If(If {
            condition: Box::new(cond),
            consequence: cons,
            alternative: alt,
        }))
    }

    fn parse_fn_expression(&mut self) -> Option<Expression> {
        let parameters = match self.lexer.next() {
            Some(Token::Lparen) => {
                if let Ok(params) = self.parse_fn_params() {
                    params
                } else {
                    // there's an error, bubble-up
                    return None;
                }
            }
            etc => {
                // `fn(`
                //    ^
                self.errors
                    .push(format!("invalid syntax: expected `fn(', got `fn {etc:?}'",));
                return None;
            }
        };

        let body = match self.lexer.next() {
            Some(Token::Lbrace) => match self.parse_block_statement() {
                Ok(block) => block,
                Err(_) => return None,
            },
            etc => {
                // `fn(...) {`
                //          ^
                self.errors.push(format!(
                    "invalid syntax: expected `{{' after fn parameters, got {etc:?}",
                ));
                return None;
            }
        };
        Some(Expression::Fn(Function { body, parameters }))
    }

    fn parse_fn_params(&mut self) -> Result<Vec<Identifier>, ()> {
        let mut params = vec![];

        if self.lexer.peek() == Some(&Token::Rparen) {
            self.lexer.next();
            return Ok(params);
        }

        while let Some(Token::Identifier(value)) = self.lexer.next() {
            params.push(Identifier { value });
            match self.lexer.next() {
                Some(Token::Comma) => continue,
                Some(Token::Rparen) => return Ok(params),
                token => {
                    self.errors
                        .push(format!("expected `,' or `)', got {token:?}"));
                    return Err(());
                }
            }
        }

        self.errors
            .push(format!("incorrect parameters decl: missing `,' or `)'"));
        Err(())
    }

    fn parse_block_statement(&mut self) -> Result<Block, ()> {
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

        self.errors
            .push(format!("no }} found - block is not closed"));
        Err(())
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Result<Expression, String> {
        let operator = self
            .lexer
            .next()
            .ok_or(format!("no token available for an operator after {left:?}"))?;

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

        while let Some(token) = self.lexer.next() {
            match token {
                Token::Rparen => return Ok(args),
                Token::Lparen => {
                    if !args.is_empty() {
                        return Err(format!("wrong function call args separator: {token:?}"));
                    }
                }
                Token::Comma => (), // usual separator
                etc => return Err(format!("wrong function call args separator: {etc:?}")),
            }

            let expr = self
                .parse_expression(PrecedenceLevel::Lowest)
                .map_err(|e| {
                    format!("error parsing a fn call args: {e}, already parsed: {args:?}")
                })?;
            args.push(expr);
        }

        Err(format!(
            "found args {args:?} but no `)' reached parsing a fn call args"
        ))
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
