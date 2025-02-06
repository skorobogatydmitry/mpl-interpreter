//! statement is the main building block of a program

use std::fmt::Display;

use super::Expression;
use super::Statement;

/// let statement descriptor  
/// let <identifier> = <expression>;  
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Let {
    pub name: String, // identifier name: in `let x = 10;` it's `x`
    pub value: Expression,
}

/// `return <expression>;` or just `return;`
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Return {
    pub ret_expr: Expression,
}

/// { 5; 6; 7; add(1,2,x); }
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {}", self.name, self.value)
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {}", self.ret_expr)
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{{\n  {}\n}}",
            self.statements
                .iter()
                .map(|stmt| format!("{}", stmt)) // TODO: add indent
                .collect::<Vec<String>>()
                .join("\n  ")
        )
    }
}
