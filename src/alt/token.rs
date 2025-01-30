use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Token {
    Illegal(String),
    EOF,

    Identifier(String),
    Int(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    Lt,
    Gt,
    Eq,
    NotEq,

    Comma,
    Semicolon,

    Lparen,
    Rparen,
    Lbrace,
    Rbrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl Token {
    pub fn from_identifier(identifier: String) -> Self {
        match &identifier[..] {
            "fn" => Self::Function,
            "let" => Self::Let,
            "if" => Self::If,
            "true" => Self::True,
            "false" => Self::False,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => Self::Identifier(identifier.to_string()),
        }
    }
}
