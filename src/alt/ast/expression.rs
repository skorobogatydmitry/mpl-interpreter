use crate::alt::token::Token;

use super::statement;
use super::Expression;

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
    pub parameters: Vec<String>,
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
