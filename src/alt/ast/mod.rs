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
    Integer(i64),
    Prefix(expression::Prefix),
    Infix(expression::Infix),
    Boolean(bool),
    If(expression::If),
    Fn(expression::Function),
    Call(expression::Call),
    Empty, // just a `;', is needed e.g. for return ;
}

#[derive(Debug, PartialEq)]
pub struct Program {
    pub statements: Vec<Statement>,
}
