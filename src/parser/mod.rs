use std::collections::HashMap;

#[cfg(test)]
mod test;

use crate::{
    ast::*,
    lexer::Lexer,
    token::{Token, TokenKind},
};

/// type alias for functions to parse expressions with prefix operator like -x
type PrefixParseFn = fn(parser: &mut Parser) -> Option<Expression>;
/// type alias for functions to parse expressions with infix operator like x + 5
type InfixParseFn = fn(parser: &mut Parser, expr: Expression) -> Option<Expression>;

#[derive(PartialEq, PartialOrd)]
enum PrecedenceLevel {
    Lowest = 0,
    Equals = 1,      // ==
    LessGreater = 2, // >,<
    Sum = 3,         // +,-
    Product = 4,     // *,/
    Prefix = 5,      // -,+,!
    Call = 6,        // add(1,2)
    Index = 7,       // some[1]
}

impl PrecedenceLevel {
    fn from_token(kind: &TokenKind) -> Self {
        match kind {
            TokenKind::Eq => PrecedenceLevel::Equals,
            TokenKind::NotEq => PrecedenceLevel::Equals,
            TokenKind::Lt => PrecedenceLevel::LessGreater,
            TokenKind::Gt => PrecedenceLevel::LessGreater,
            TokenKind::Plus => PrecedenceLevel::Sum,
            TokenKind::Minus => PrecedenceLevel::Sum,
            TokenKind::Slash => PrecedenceLevel::Product,
            TokenKind::Asterisk => PrecedenceLevel::Product,
            TokenKind::Lparen => PrecedenceLevel::Call,
            TokenKind::Lbracket => PrecedenceLevel::Index,
            _ => PrecedenceLevel::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    cur_token: Token,
    peek_token: Token,
    pub errors: Vec<String>,
    prefix_parse_fns: HashMap<TokenKind, PrefixParseFn>,
    infix_parse_fns: HashMap<TokenKind, InfixParseFn>,
}

impl Parser {
    pub fn new(mut lexer: Lexer) -> Self {
        let cur_t = lexer.next_token();
        let peek_t = lexer.next_token();
        Self {
            lexer,
            cur_token: cur_t,
            peek_token: peek_t,
            errors: vec![],
            prefix_parse_fns: HashMap::from([
                (
                    TokenKind::Identifier,
                    Self::parse_identifier as PrefixParseFn,
                ),
                (TokenKind::Int, Self::parse_integer_literal),
                (TokenKind::Bang, Self::parse_prefix_expression),
                (TokenKind::Minus, Self::parse_prefix_expression),
                (TokenKind::True, Self::parse_boolean),
                (TokenKind::False, Self::parse_boolean),
                (TokenKind::Lparen, Self::parse_groupped_expression),
                (TokenKind::If, Self::parse_if_expression),
                (TokenKind::Function, Self::parse_fn_expression),
                (TokenKind::String, Self::parse_string_literal),
                (TokenKind::Lbracket, Self::parse_array_literal),
                (TokenKind::Lbrace, Self::parse_hash_literal),
            ]),
            infix_parse_fns: HashMap::from([
                (
                    TokenKind::Plus,
                    Self::parse_infix_expression as InfixParseFn,
                ),
                (TokenKind::Minus, Self::parse_infix_expression),
                (TokenKind::Asterisk, Self::parse_infix_expression),
                (TokenKind::Slash, Self::parse_infix_expression),
                (TokenKind::Eq, Self::parse_infix_expression),
                (TokenKind::NotEq, Self::parse_infix_expression),
                (TokenKind::Lt, Self::parse_infix_expression),
                (TokenKind::Gt, Self::parse_infix_expression),
                (TokenKind::Lparen, Self::parse_fn_call), // ( is considered to be an infix operator between fn name and its params
                (TokenKind::Lbracket, Self::parse_index_expression),
            ]),
        }
    }

    fn next_token(&mut self) {
        self.cur_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();
    }

    pub fn parse_program(&mut self) -> Option<Program> {
        let mut program = Program { statements: vec![] };

        while self.cur_token.kind != TokenKind::EOF {
            if let Some(statement) = self.parse_statement() {
                program.statements.push(statement);
            }

            self.next_token();
        }

        Some(program)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.cur_token.kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        self.next_token();
        let ret_val = self.parse_expression(PrecedenceLevel::Lowest);
        if self.peek_token.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Some(Statement::Return(ReturnStatement { token, ret_val }))
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        if self.expect_peek(TokenKind::Identifier) {
            let name = Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            };

            if self.expect_peek(TokenKind::Assign) {
                self.next_token();

                let value = self.parse_expression(PrecedenceLevel::Lowest);

                if self.peek_token.kind == TokenKind::Semicolon {
                    self.next_token();
                }

                Some(Statement::Let(LetStatement { token, name, value }))
            } else {
                None
            }
        } else {
            None
        }
    }

    fn expect_peek(&mut self, kind: TokenKind) -> bool {
        if self.peek_token.kind == kind {
            self.next_token();
            true
        } else {
            self.reg_error(kind);
            false
        }
    }

    fn reg_error(&mut self, kind: TokenKind) {
        self.errors.push(format!(
            "expected next token to be {}, got {}",
            kind, self.peek_token.kind
        ));
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let token = self.cur_token.clone();
        let expression = self.parse_expression(
            PrecedenceLevel::Lowest, // the whole expression has the lowest precedence
        );
        // allow expressions to end with `;` e.g. some + 5; let x = 10;
        if self.peek_token.kind == TokenKind::Semicolon {
            self.next_token();
        }
        Some(Statement::Expression(ExpressionStatement {
            token,
            expression,
        }))
    }

    fn parse_expression(&mut self, precedence: PrecedenceLevel) -> Option<Expression> {
        let prefix_fn = self.prefix_parse_fns.get(&self.cur_token.kind);
        if let Some(prefix_fn) = prefix_fn {
            let mut left_expr = prefix_fn(self);

            while self.peek_token.kind != TokenKind::Semicolon
                && precedence < PrecedenceLevel::from_token(&self.peek_token.kind)
            {
                if let Some(infix_fn) = self.infix_parse_fns.get(&self.peek_token.kind) {
                    left_expr = infix_fn(
                        self,
                        left_expr.expect(&format!(
                            "no left expression for {:?}",
                            self.peek_token.kind
                        )),
                    )
                }
            }
            left_expr
        } else {
            self.errors.push(format!(
                "no prefix parsing fn for token kind {}",
                self.cur_token.kind
            ));
            None
        }
    }

    fn parse_identifier(&mut self) -> Option<Expression> {
        Some(Expression::Identifier(Identifier {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_integer_literal(&mut self) -> Option<Expression> {
        match self.cur_token.literal.parse() {
            Ok(val) => Some(Expression::Integer(IntegerLiteral {
                token: self.cur_token.clone(),
                value: val,
            })),
            Err(e) => {
                self.errors.push(format!(
                    "cannot parse {} as i64, got {:?}",
                    self.cur_token.literal, e
                ));
                None
            }
        }
    }

    fn parse_prefix_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();
        self.next_token();
        self.parse_expression(PrecedenceLevel::Prefix)
            .map(|operand| {
                Expression::Prefix(PrefixExpression {
                    token,
                    operator,
                    operand: Box::new(operand),
                })
            })
    }

    fn parse_boolean(&mut self) -> Option<Expression> {
        Some(Expression::Boolean(Boolean {
            token: self.cur_token.clone(),
            value: match self.cur_token.kind {
                TokenKind::False => false,
                TokenKind::True => true,
                _ => panic!(
                    "current token is not true / false, but {}",
                    self.cur_token.kind
                ),
            },
        }))
    }

    fn parse_groupped_expression(&mut self) -> Option<Expression> {
        self.next_token(); // move to the token next, away from the (
        let expression = self.parse_expression(PrecedenceLevel::Lowest);

        if self.expect_peek(TokenKind::Rparen) {
            expression
        } else {
            None // invalid syntax
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();

        if !self.expect_peek(TokenKind::Lparen) {
            // all if's are expected to have () around condition
            return None;
        }
        self.next_token(); // make ( a current token
        let cond = self
            .parse_expression(PrecedenceLevel::Lowest)
            .expect("can't parse condition");

        if !self.expect_peek(TokenKind::Rparen) {
            // all if's conditions ends with )
            return None;
        }
        if !self.expect_peek(TokenKind::Lbrace) {
            // there should be {
            return None;
        }

        let cons = self.parse_block_statement();
        let alt = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            if !self.expect_peek(TokenKind::Lbrace) {
                // there should be { after else
                return None;
            }
            Some(self.parse_block_statement())
        } else {
            None
        };

        Some(Expression::If(IfExpression {
            token,
            condition: Box::new(cond),
            consequence: cons,
            alternative: alt,
        }))
    }

    fn parse_fn_expression(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        if !self.expect_peek(TokenKind::Lparen) {
            // `fn(`
            //    ^
            return None;
        }
        let parameters = if let Some(params) = self.parse_fn_params() {
            params
        } else {
            return None; // cannot parse parameters
        };
        if !self.expect_peek(TokenKind::Lbrace) {
            // `fn(params) {`
            //             ^
            return None;
        }
        let body = self.parse_block_statement();
        Some(Expression::Fn(Function {
            token,
            body,
            parameters,
        }))
    }

    fn parse_string_literal(&mut self) -> Option<Expression> {
        Some(Expression::StringExp(StringLiteral {
            token: self.cur_token.clone(),
            value: self.cur_token.literal.clone(),
        }))
    }

    fn parse_array_literal(&mut self) -> Option<Expression> {
        Some(Expression::Array(Array {
            token: self.cur_token.clone(),
            elements: self.parse_expressions_list(TokenKind::Rbracket),
        }))
    }

    fn parse_hash_literal(&mut self) -> Option<Expression> {
        let token = self.cur_token.clone();
        let mut h = vec![];

        while self.peek_token.kind != TokenKind::Rbrace {
            self.next_token();
            let key = self
                .parse_expression(PrecedenceLevel::Lowest)
                .expect("error parsing key expression");
            if !self.expect_peek(TokenKind::Colon) {
                return None;
            }
            self.next_token();
            let val = self
                .parse_expression(PrecedenceLevel::Lowest)
                .expect(&format!(
                    "error parsing value expression: {:?}",
                    self.errors
                ));
            h.push((key, val));

            if self.peek_token.kind != TokenKind::Rbrace && !self.expect_peek(TokenKind::Comma) {
                return None;
            }
        }

        if !self.expect_peek(TokenKind::Rbrace) {
            return None;
        }

        Some(Expression::Hash(Hash { token, h }))
    }

    fn parse_fn_params(&mut self) -> Option<Vec<Identifier>> {
        if self.peek_token.kind == TokenKind::Rparen {
            self.next_token();
            Some(vec![])
        } else {
            self.next_token();

            let mut idfs = vec![Identifier {
                token: self.cur_token.clone(),
                value: self.cur_token.literal.clone(),
            }];

            while self.peek_token.kind == TokenKind::Comma {
                self.next_token(); // comma
                self.next_token(); // identifier
                idfs.push(Identifier {
                    token: self.cur_token.clone(),
                    value: self.cur_token.literal.clone(),
                });
            }

            if self.expect_peek(TokenKind::Rparen) {
                Some(idfs)
            } else {
                Some(vec![])
            }
        }
    }

    fn parse_block_statement(&mut self) -> BlockStatement {
        let mut block = BlockStatement {
            token: self.cur_token.clone(),
            statements: vec![],
        };
        self.next_token();

        while self.cur_token.kind != TokenKind::Rbrace && self.cur_token.kind != TokenKind::EOF {
            match self.parse_statement() {
                Some(stmt) => block.statements.push(stmt),
                None => (),
            }
            self.next_token();
        }

        block
    }

    fn parse_infix_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token(); // prev function is supposed to leave cursor at the left expression's last token
        let token = self.cur_token.clone();
        let operator = self.cur_token.literal.clone();

        let precedence = PrecedenceLevel::from_token(&self.cur_token.kind);
        self.next_token(); // go to the right operand
        self.parse_expression(precedence).map(|right| {
            Expression::Infix(InfixExpression {
                token,
                operator,
                left: Box::new(left),
                right: Box::new(right),
            })
        })
    }

    fn parse_fn_call(&mut self, function: Expression) -> Option<Expression> {
        self.next_token();

        Some(Expression::Call(Call {
            token: self.cur_token.clone(),
            function: Box::new(function),
            arguments: self.parse_expressions_list(TokenKind::Rparen),
        }))
    }

    fn parse_index_expression(&mut self, left: Expression) -> Option<Expression> {
        self.next_token(); // move to the [
        let token = self.cur_token.clone();
        self.next_token(); // move to the start of expression

        let exp = Index {
            token,
            operand: Box::new(left),
            index: Box::new(
                self.parse_expression(PrecedenceLevel::Lowest)
                    .expect("error parsing index expression"),
            ),
        };

        if !self.expect_peek(TokenKind::Rbracket) {
            return None;
        }

        Some(Expression::Index(exp))
    }

    fn parse_expressions_list(&mut self, end: TokenKind) -> Vec<Expression> {
        let mut args = vec![];
        if self.peek_token.kind == end {
            self.next_token();
            return args;
        }

        self.next_token();
        args.push(
            self.parse_expression(PrecedenceLevel::Lowest)
                .expect("unable to parse args"),
        );
        while self.peek_token.kind == TokenKind::Comma {
            self.next_token();
            self.next_token();
            args.push(
                self.parse_expression(PrecedenceLevel::Lowest)
                    .expect("unable to parse args"),
            );
        }

        if !self.expect_peek(end) {
            return vec![];
        }

        args
    }
}
