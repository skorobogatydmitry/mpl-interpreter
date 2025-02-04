//! statement is the main building block of a program

use super::Expression;
use super::Statement;

/// let statement descriptor  
/// let <identifier> = <expression>;  
#[derive(Debug, PartialEq)]
pub struct Let {
    pub name: String, // identifier name: in `let x = 10;` it's `x`
    pub value: Option<Expression>,
}

/// `return <expression>;` or just `return;`
#[derive(Debug, PartialEq)]
pub struct Return {
    pub ret_expr: Expression,
}

/// { 5; 6; 7; add(1,2,x); }
#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}
