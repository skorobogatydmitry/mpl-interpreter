use std::fmt::Display;

#[derive(Debug, PartialEq, Clone, Hash, Eq)]
pub enum Token {
    Illegal(String),

    Identifier(String),
    Int(String),
    String(String),

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
        match self {
            Token::Illegal(raw) => write!(f, "illegal token ({})", raw),
            Token::Identifier(idf) => write!(f, "{}", idf),
            Token::Int(int) => write!(f, "{}", int),
            Token::String(val) => write!(f, "{}", val),
            Token::Assign => write!(f, "="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Bang => write!(f, "!"),
            Token::Asterisk => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Lt => write!(f, "<"),
            Token::Gt => write!(f, ">"),
            Token::Eq => write!(f, "=="),
            Token::NotEq => write!(f, "!="),
            Token::Comma => write!(f, ","),
            Token::Semicolon => write!(f, ";"),
            Token::Lparen => write!(f, "("),
            Token::Rparen => write!(f, ")"),
            Token::Lbrace => write!(f, "{{"),
            Token::Rbrace => write!(f, "}}"),
            Token::Function => write!(f, "fn"),
            Token::Let => write!(f, "let"),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if"),
            Token::Else => write!(f, "else"),
            Token::Return => write!(f, "return"),
        }
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
