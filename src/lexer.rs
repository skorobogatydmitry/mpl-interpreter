use crate::token::{Token, TokenKind};

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
    ch: char,
}

impl Lexer {
    pub fn new(input: &str) -> Self {
        Self {
            input: input.chars().collect(),
            pos: 0,
            read_pos: 1,
            ch: input.chars().next().unwrap_or('\0'),
        }
    }

    pub fn next_token(&mut self) -> Token {
        // skip whitespaces
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
        let mut literal = self.ch.to_string();
        let token = match self.ch {
            '=' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new("==".to_string(), TokenKind::Eq)
                } else {
                    Token::new(literal, TokenKind::Assign)
                }
            }
            ';' => Token::new(literal, TokenKind::Semicolon),
            '(' => Token::new(literal, TokenKind::Lparen),
            ')' => Token::new(literal, TokenKind::Rparen),
            ',' => Token::new(literal, TokenKind::Comma),
            '+' => Token::new(literal, TokenKind::Plus),
            '{' => Token::new(literal, TokenKind::Lbrace),
            '}' => Token::new(literal, TokenKind::Rbrace),
            '-' => Token::new(literal, TokenKind::Minus),
            '!' => {
                if self.peek_char() == '=' {
                    self.read_char();
                    Token::new("!=".to_string(), TokenKind::NotEq)
                } else {
                    Token::new(literal, TokenKind::Bang)
                }
            }
            '/' => Token::new(literal, TokenKind::Slash),
            '*' => Token::new(literal, TokenKind::Asterisk),
            '<' => Token::new(literal, TokenKind::Lt),
            '>' => Token::new(literal, TokenKind::Gt),
            '\0' => Token::new("".to_string(), TokenKind::EOF),
            _ => {
                let mut kind = TokenKind::Illegal;
                if self.is_on_valid_id_char() {
                    literal = self.read_identifier();
                    kind = TokenKind::from_identifier(&literal[..]);
                } else if self.ch.is_numeric() {
                    kind = TokenKind::Int;
                    literal = self.read_number();
                }
                return Token { literal, kind };
            }
        };
        self.read_char();
        token
    }

    fn read_number(&mut self) -> String {
        let mut res = String::new();
        while self.ch.is_numeric() {
            res.push(self.ch);
            self.read_char();
        }

        res
    }

    /// whether the current character is a valid part of an identifier
    fn is_on_valid_id_char(&self) -> bool {
        self.ch.is_alphabetic() || self.ch == '_'
    }

    fn read_identifier(&mut self) -> String {
        let mut res = String::new();
        while self.is_on_valid_id_char() {
            res.push(self.ch);
            self.read_char();
        }

        res
    }

    fn read_char(&mut self) {
        if self.read_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.read_pos];
        }
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_pos]
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::token::*;

    #[test]
    fn next_token() {
        let sample_input = r#"
        let five = 5;
        let ten = 10;
        let add = fn(x,y) {
          x + y;
        };

        let result = add(five, ten);

        !-/*5;
        5 < 10 > 5;

        if (5<10) {
          return true;
        } else {
          return false;
        }
        
        10 == 10;
        10 != 9;
        "#;
        let expected = vec![
            Token::new("let".to_string(), TokenKind::Let),
            Token::new("five".to_string(), TokenKind::Identifier),
            Token::new("=".to_string(), TokenKind::Assign),
            Token::new("5".to_string(), TokenKind::Int),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("let".to_string(), TokenKind::Let), // 5
            Token::new("ten".to_string(), TokenKind::Identifier),
            Token::new("=".to_string(), TokenKind::Assign),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("let".to_string(), TokenKind::Let), // 10
            Token::new("add".to_string(), TokenKind::Identifier),
            Token::new("=".to_string(), TokenKind::Assign),
            Token::new("fn".to_string(), TokenKind::Function),
            Token::new("(".to_string(), TokenKind::Lparen),
            Token::new("x".to_string(), TokenKind::Identifier), // 15
            Token::new(",".to_string(), TokenKind::Comma),
            Token::new("y".to_string(), TokenKind::Identifier),
            Token::new(")".to_string(), TokenKind::Rparen),
            Token::new("{".to_string(), TokenKind::Lbrace),
            Token::new("x".to_string(), TokenKind::Identifier), // 20
            Token::new("+".to_string(), TokenKind::Plus),
            Token::new("y".to_string(), TokenKind::Identifier),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("}".to_string(), TokenKind::Rbrace),
            Token::new(";".to_string(), TokenKind::Semicolon), // 25
            Token::new("let".to_string(), TokenKind::Let),
            Token::new("result".to_string(), TokenKind::Identifier),
            Token::new("=".to_string(), TokenKind::Assign),
            Token::new("add".to_string(), TokenKind::Identifier),
            Token::new("(".to_string(), TokenKind::Lparen), // 30
            Token::new("five".to_string(), TokenKind::Identifier),
            Token::new(",".to_string(), TokenKind::Comma),
            Token::new("ten".to_string(), TokenKind::Identifier),
            Token::new(")".to_string(), TokenKind::Rparen),
            Token::new(";".to_string(), TokenKind::Semicolon), // 35
            Token::new("!".to_string(), TokenKind::Bang),
            Token::new("-".to_string(), TokenKind::Minus),
            Token::new("/".to_string(), TokenKind::Slash),
            Token::new("*".to_string(), TokenKind::Asterisk),
            Token::new("5".to_string(), TokenKind::Int), // 40
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("5".to_string(), TokenKind::Int),
            Token::new("<".to_string(), TokenKind::Lt),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new(">".to_string(), TokenKind::Gt), // 45
            Token::new("5".to_string(), TokenKind::Int),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("if".to_string(), TokenKind::If),
            Token::new("(".to_string(), TokenKind::Lparen),
            Token::new("5".to_string(), TokenKind::Int),
            Token::new("<".to_string(), TokenKind::Lt),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new(")".to_string(), TokenKind::Rparen),
            Token::new("{".to_string(), TokenKind::Lbrace),
            Token::new("return".to_string(), TokenKind::Return),
            Token::new("true".to_string(), TokenKind::True),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("}".to_string(), TokenKind::Rbrace),
            Token::new("else".to_string(), TokenKind::Else),
            Token::new("{".to_string(), TokenKind::Lbrace),
            Token::new("return".to_string(), TokenKind::Return),
            Token::new("false".to_string(), TokenKind::False),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("}".to_string(), TokenKind::Rbrace),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new("==".to_string(), TokenKind::Eq),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("10".to_string(), TokenKind::Int),
            Token::new("!=".to_string(), TokenKind::NotEq),
            Token::new("9".to_string(), TokenKind::Int),
            Token::new(";".to_string(), TokenKind::Semicolon),
            Token::new("".to_string(), TokenKind::EOF),
        ];

        let mut lexer = Lexer::new(sample_input);

        for (idx, token) in expected.iter().enumerate() {
            let rx_token = lexer.next_token();
            assert_eq!(
                token.kind, rx_token.kind,
                "{} - token kind is wrong: expected {}, got {}",
                idx, token.kind, rx_token.kind
            );
            assert_eq!(
                token.literal, rx_token.literal,
                "{} - token kind is wrong: expected {}, got {}",
                idx, token.literal, rx_token.literal
            );
        }
    }
}
