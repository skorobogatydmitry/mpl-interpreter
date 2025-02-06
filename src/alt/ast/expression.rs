use std::fmt::Display;

use crate::alt::token::Token;

use super::statement;
use super::Expression;

// there are only 2 operators: ! and -, so it could be e.g. -5 or !false
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prefix {
    pub operator: Token,
    pub operand: Box<Expression>,
}

// there are several operators, common format is <expr> <operator> <expr>
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Infix {
    pub operator: Token,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct If {
    pub condition: Box<Expression>,
    pub consequence: statement::Block,
    pub alternative: Option<statement::Block>,
}

/// function definition expression
/// # Examples
/// fn(z,y) { z / y }  
/// fn() { print_some() }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Function {
    pub parameters: Vec<String>,
    pub body: statement::Block,
}

/// function call expression
/// # examples
/// add(1,2,3);  
/// fn(x,y,z) {x + y + z}(3,2,1)
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Call {
    pub function: Box<Expression>, // could be an identifier as well as an inline function
    pub arguments: Vec<Expression>,
}

/// indexing expression
/// # examples
/// some[1]
/// [1,2,3][1+2]
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Index {
    pub operand: Box<Expression>,
    pub index: Box<Expression>,
}

impl Display for If {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut result = format!("if ({}) {}", self.condition, self.consequence);
        if let Some(alt) = self.alternative.as_ref() {
            result.push_str(&format!(" else {}", alt));
        }
        write!(f, "{}", result)
    }
}

impl Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}) {}",
            self.parameters
                .iter()
                .map(|a| format!("{}", a))
                .collect::<Vec<String>>()
                .join(", "),
            self.body
        )
    }
}

impl Display for Call {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}({})",
            self.function,
            self.arguments
                .iter()
                .map(|a| format!("{}", a))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Display for Index {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}[{}])", self.operand, self.index)
    }
}
