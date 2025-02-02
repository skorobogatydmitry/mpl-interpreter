//! statement is the main building block of a program

use super::expression::*;
use super::Expression;
use super::Statement;

/// let statement descriptor  
/// let <identifier> = <expression>;  
#[derive(Debug, PartialEq)]
pub struct Let {
    pub name: Identifier, // identifier name: in `let x = 10;` it's `x`
    pub value: Option<Expression>,
}

/// `return <expression>;` or just `return;`
#[derive(Debug, PartialEq)]
pub struct Return {
    pub ret_expr: Option<Expression>,
}

/// wrapper to allow `x + 5` to be a statement
#[derive(Debug, PartialEq)]
pub struct ExpressionStatement {
    pub expression: Option<Expression>,
}

/// { 5; 6; 7; add(1,2,x); }
#[derive(Debug, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>,
}
