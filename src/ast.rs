use crate::token::Token;

/// abstract AST building block
pub trait Node {
    fn token_literal(&self) -> String;
    fn print(&self) -> String;
}

#[derive(Debug, Clone)]
pub enum Statement {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Identifier(Identifier),
    Integer(IntegerLiteral),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    Boolean(Boolean),
    If(IfExpression),
    Fn(Function),
    Call(Call),
}

pub struct Program {
    pub statements: Vec<Statement>,
}

/// let statement descriptor  
/// let <identifier> = <expression>;  
#[derive(Debug, Clone)]
pub struct LetStatement {
    pub token: Token,     // garbage for debugging
    pub name: Identifier, // identifier name: in `let x = 10;` it's `x`
    pub value: Option<Expression>,
}

// identifier metadata
#[derive(Debug, Clone)]
pub struct Identifier {
    pub token: Token,  // garbage for debugging
    pub value: String, // actual identifier's name
}

/// return <expression>;
#[derive(Debug, Default, Clone)]
pub struct ReturnStatement {
    pub token: Token,
    pub ret_val: Option<Expression>,
}

/// wrapper to allow `x + 5` to be a statement
#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub token: Token,
    pub expression: Option<Expression>,
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub token: Token,
    pub value: i64,
}

// there are only 2 operators: ! and -, so it could be e.g. -5 or !false
#[derive(Debug, Clone)]
pub struct PrefixExpression {
    pub token: Token,
    pub operator: String,
    pub operand: Box<Expression>,
}

// there are several operators, common format is <expr> <operator> <expr>
#[derive(Debug, Clone)]
pub struct InfixExpression {
    pub token: Token,
    pub operator: String,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

#[derive(Debug, Clone)]
pub struct Boolean {
    pub token: Token,
    pub value: bool,
}

#[derive(Debug, Clone)]
pub struct IfExpression {
    pub token: Token,
    pub condition: Box<Expression>,
    pub consequence: BlockStatement,
    pub alternative: Option<BlockStatement>,
}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub token: Token,
    pub statements: Vec<Statement>,
}

/// function definition expression
/// # Examples
/// fn(z,y) { z / y }  
/// fn() { print_some() }
#[derive(Debug, Clone)]
pub struct Function {
    pub token: Token,
    pub parameters: Vec<Identifier>,
    pub body: BlockStatement,
}

/// function call expression
/// # examples
/// add(1,2,3);  
/// fn(x,y,z) {x + y + z}(3,2,1)
#[derive(Debug, Clone)]
pub struct Call {
    pub token: Token,
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match self {
            Self::Let(stmt) => stmt.token_literal(),
            Self::Return(stmt) => stmt.token_literal(),
            Self::Expression(stmt) => stmt.token_literal(),
            Self::Block(stmt) => stmt.token_literal(),
        }
    }
    fn print(&self) -> String {
        match self {
            Self::Let(stmt) => stmt.print(),
            Self::Return(stmt) => stmt.print(),
            Self::Expression(stmt) => stmt.print(),
            Self::Block(stmt) => stmt.print(),
        }
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        match self {
            Self::Identifier(data) => data.token_literal(),
            Self::Integer(data) => data.token_literal(),
            Self::Prefix(data) => data.token_literal(),
            Self::Infix(data) => data.token_literal(),
            Self::Boolean(data) => data.token_literal(),
            Self::If(data) => data.token_literal(),
            Self::Fn(data) => data.token_literal(),
            Self::Call(data) => data.token_literal(),
        }
    }
    fn print(&self) -> String {
        match self {
            Self::Identifier(data) => data.print(),
            Self::Integer(data) => data.print(),
            Self::Prefix(data) => data.print(),
            Self::Infix(data) => data.print(),
            Self::Boolean(data) => data.print(),
            Self::If(data) => data.print(),
            Self::Fn(data) => data.print(),
            Self::Call(data) => data.print(),
        }
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        if !self.statements.is_empty() {
            match &self.statements[0] {
                Statement::Let(stmt) => stmt.token_literal(),
                Statement::Return(stmt) => stmt.token_literal(),
                Statement::Expression(stmt) => stmt.token_literal(),
                Statement::Block(stmt) => stmt.token_literal(),
            }
        } else {
            "".to_string()
        }
    }
    fn print(&self) -> String {
        let mut res = "".to_string();
        for stm in self.statements.as_slice() {
            res.push_str(stm.print().as_str())
        }
        res
    }
}

impl Node for LetStatement {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn print(&self) -> String {
        let mut res = "".to_string();
        res.push_str(self.token_literal().as_str());
        res.push_str(" ");
        res.push_str(self.name.print().as_str());
        res.push_str(" = ");
        if let Some(expr) = &self.value {
            res.push_str(expr.print().as_str());
        }
        res.push_str(";");

        res
    }
}

impl Node for Identifier {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn print(&self) -> String {
        self.value.clone()
    }
}

impl Node for ReturnStatement {
    fn print(&self) -> String {
        format!(
            "{}{};",
            self.token_literal(),
            self.ret_val
                .as_ref()
                .map(|val| format!(" {}", val.print()))
                .unwrap_or_default()
        )
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for ExpressionStatement {
    fn print(&self) -> String {
        self.expression
            .as_ref()
            .map(|exp| exp.print())
            .unwrap_or_default()
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for IntegerLiteral {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }

    fn print(&self) -> String {
        self.token_literal()
    }
}

impl Node for PrefixExpression {
    fn print(&self) -> String {
        format!("({}{})", self.operator, self.operand.print())
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for InfixExpression {
    fn print(&self) -> String {
        format!(
            "({} {} {})",
            self.left.print(),
            self.operator,
            self.right.print()
        )
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for Boolean {
    fn print(&self) -> String {
        self.token_literal()
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for IfExpression {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn print(&self) -> String {
        let mut res = String::new();
        res.push_str(format!("if{} {}", self.condition.print(), self.consequence.print()).as_str());
        if let Some(alt) = &self.alternative {
            res.push_str(format!(" {}", alt.print()).as_str());
        }
        res
    }
}

impl Node for BlockStatement {
    fn print(&self) -> String {
        let mut res = String::new();

        for stmt in &self.statements {
            res.push_str(stmt.print().as_str());
        }
        res
    }
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
}

impl Node for Function {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn print(&self) -> String {
        format!(
            "{}({}) {}",
            self.token_literal(),
            self.parameters
                .iter()
                .map(|p| p.print())
                .collect::<Vec<String>>()
                .join(", "),
            self.body.print().as_str()
        )
    }
}

impl Node for Call {
    fn token_literal(&self) -> String {
        self.token.literal.clone()
    }
    fn print(&self) -> String {
        format!(
            "{}({})",
            self.function.print(),
            self.arguments
                .iter()
                .map(|arg| arg.print())
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_print() {
        let program = Program {
            statements: vec![Statement::Let(LetStatement {
                token: Token {
                    kind: crate::token::TokenKind::Let,
                    literal: "let".to_string(),
                },
                name: Identifier {
                    token: Token {
                        kind: crate::token::TokenKind::Identifier,
                        literal: "myVar".to_string(),
                    },
                    value: "myVar".to_string(),
                },
                value: Some(Expression::Identifier(Identifier {
                    token: Token {
                        kind: crate::token::TokenKind::Identifier,
                        literal: "anotherVar".to_string(),
                    },
                    value: "anotherVar".to_string(),
                })),
            })],
        };

        assert_eq!("let myVar = anotherVar;", program.print());
    }
}
