use std::fmt::Display;

use crate::alt::token::Token;

use super::statement;
use super::Expression;

// TODO: place it right in the expression enum
// identifier metadata
#[derive(Debug, PartialEq)]
pub struct Identifier {
    pub value: String, // actual identifier's name
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

// TODO: place it right in the expression enum
#[derive(Debug, PartialEq)]
pub struct Integer {
    pub value: i64,
}

// there are only 2 operators: ! and -, so it could be e.g. -5 or !false
#[derive(Debug, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub operand: Box<Expression>,
}

// there are several operators, common format is <expr> <operator> <expr>
#[derive(Debug, PartialEq)]
pub struct Infix {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

// TODO: place it right in the expression enum
#[derive(Debug, PartialEq)]
pub struct Boolean {
    pub value: bool,
}

#[derive(Debug, PartialEq)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: statement::Block,
    pub alternative: Option<statement::Block>,
}

/// function definition expression
/// # Examples
/// fn(z,y) { z / y }  
/// fn() { print_some() }
#[derive(Debug, PartialEq)]
pub struct Function {
    pub parameters: Vec<Identifier>,
    pub body: statement::Block,
}

/// function call expression
/// # examples
/// add(1,2,3);  
/// fn(x,y,z) {x + y + z}(3,2,1)
#[derive(Debug, PartialEq)]
pub struct Call {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}
