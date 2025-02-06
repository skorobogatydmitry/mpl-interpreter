use std::{collections::BTreeMap, fmt::Display};

pub mod expression;
pub mod statement;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Statement {
    Let(statement::Let),
    Return(statement::Return),
    Expression(Expression),
    Block(statement::Block),
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Expression {
    Identifier(String), // actual identifier's name
    Prefix(expression::Prefix),
    Infix(expression::Infix),
    Integer(i64),
    Boolean(bool),
    String(String),
    Array(Vec<Expression>),
    Index(expression::Index),
    Hash(BTreeMap<Expression, Expression>),
    Pair((Box<Expression>, Box<Expression>)),
    If(expression::If),
    Fn(expression::Function),
    Call(expression::Call),
    Empty, // just a `;', is needed e.g. for return ;
}

#[derive(Debug, PartialEq, Eq)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expression::Identifier(data) => write!(f, "{}", data),
            Expression::Prefix(data) => write!(f, "{}{}", data.operand, data.operator),
            Expression::Infix(data) => write!(f, "{} {} {}", data.left, data.operator, data.right),
            Expression::Integer(data) => write!(f, "{}", data),
            Expression::Boolean(data) => write!(f, "{}", data),
            Expression::String(data) => write!(f, "{}", data),
            Expression::Array(data) => write!(
                f,
                "[{}]",
                data.iter()
                    .map(|el| format!("{}", el))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Index(data) => write!(f, "{}", data),
            Expression::Hash(data) => write!(
                f,
                "{{{}}}",
                data.iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Expression::Pair((k, v)) => write!(f, "{}: {}", k, v),
            Expression::If(data) => write!(f, "{}", data),
            Expression::Fn(data) => write!(f, "{}", data),
            Expression::Call(data) => write!(f, "{}", data),
            Expression::Empty => write!(f, ""),
        }
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Statement::Let(val) => write!(f, "{};", val),
            Statement::Return(val) => write!(f, "{};", val),
            Statement::Expression(val) => write!(f, "{};", val),
            Statement::Block(val) => write!(f, "{};", val),
        }
    }
}
