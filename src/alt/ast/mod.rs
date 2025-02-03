pub mod expression;
pub mod statement;

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let(statement::Let),
    Return(statement::Return),
    Expression(Expression),
    Block(statement::Block),
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Identifier(String), // actual identifier's name
    Prefix(expression::Prefix),
    Infix(expression::Infix),
    Integer(i64),
    Boolean(bool),
    String(String),
    If(expression::If),
    Fn(expression::Function),
    Call(expression::Call),
    Empty, // just a `;', is needed e.g. for return ;
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
