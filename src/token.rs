use std::fmt::Display;

#[derive(Debug, PartialEq, Default, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub literal: String,
}

#[derive(Debug, PartialEq, Default, Clone, Hash, Eq)]
pub enum TokenKind {
    #[default]
    Illegal,
    EOF,

    Identifier,
    Int,

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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

impl TokenKind {
    pub fn from_identifier(identifier: &str) -> Self {
        match identifier {
            "fn" => Self::Function,
            "let" => Self::Let,
            "if" => Self::If,
            "true" => Self::True,
            "false" => Self::False,
            "else" => Self::Else,
            "return" => Self::Return,
            _ => Self::Identifier,
        }
    }
}

impl Token {
    pub fn new(literal: String, kind: TokenKind) -> Self {
        Self { literal, kind }
    }
}
