use std::{iter::Peekable, str::Chars};

use super::token::Token;

pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
        }
    }

    /// collect all consecutive numbers to a new String
    fn read_number(&mut self, start: char) -> String {
        let mut res = String::new();
        res.push(start);
        while self.peek_char().is_numeric() {
            res.push(self.read_char());
        }

        res
    }

    /// whether current character is a valid part of an identifier
    fn is_valid_id_char(ch: &char) -> bool {
        ch.is_alphabetic() || *ch == '_'
    }

    /// collect all valid letters to a new String
    fn read_identifier(&mut self, start: char) -> String {
        let mut res = String::new();
        res.push(start);
        while Self::is_valid_id_char(self.peek_char()) {
            res.push(self.read_char());
        }

        res
    }

    fn read_char(&mut self) -> char {
        // TODO: return & process Option
        self.input.next().unwrap_or('\0')
    }

    fn peek_char(&mut self) -> &'_ char {
        self.input.peek().unwrap_or(&'\0')
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        // skip whitespaces
        while self.peek_char().is_ascii_whitespace() {
            self.read_char();
        }

        Some(match self.read_char() {
            '=' => {
                if self.peek_char() == &'=' {
                    self.read_char(); // skip the 2nd '='
                    Token::Eq
                } else {
                    Token::Assign
                }
            }
            ';' => Token::Semicolon,
            '(' => Token::Lparen,
            ')' => Token::Rparen,
            ',' => Token::Comma,
            '+' => Token::Plus,
            '{' => Token::Lbrace,
            '}' => Token::Rbrace,
            '-' => Token::Minus,
            '!' => {
                if self.peek_char() == &'=' {
                    self.read_char(); // skip the 2nd '='
                    Token::NotEq
                } else {
                    Token::Bang
                }
            }
            '/' => Token::Slash,
            '*' => Token::Asterisk,
            '<' => Token::Lt,
            '>' => Token::Gt,
            '\0' => return None,
            ch => {
                if Self::is_valid_id_char(&ch) {
                    Token::from_identifier(self.read_identifier(ch))
                } else if ch.is_numeric() {
                    Token::Int(self.read_number(ch))
                } else {
                    Token::Illegal("".to_string())
                }
            }
        })
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::alt::token::*;

    #[test]
    fn test_next_token() {
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
            Token::Let,
            Token::Identifier("five".to_string()),
            Token::Assign,
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::Let, // 5
            Token::Identifier("ten".to_string()),
            Token::Assign,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Let, // 10
            Token::Identifier("add".to_string()),
            Token::Assign,
            Token::Function,
            Token::Lparen,
            Token::Identifier("x".to_string()), // 15
            Token::Comma,
            Token::Identifier("y".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Identifier("x".to_string()), // 20
            Token::Plus,
            Token::Identifier("y".to_string()),
            Token::Semicolon,
            Token::Rbrace,
            Token::Semicolon, // 25
            Token::Let,
            Token::Identifier("result".to_string()),
            Token::Assign,
            Token::Identifier("add".to_string()),
            Token::Lparen, // 30
            Token::Identifier("five".to_string()),
            Token::Comma,
            Token::Identifier("ten".to_string()),
            Token::Rparen,
            Token::Semicolon, // 35
            Token::Bang,
            Token::Minus,
            Token::Slash,
            Token::Asterisk,
            Token::Int("5".to_string()), // 40
            Token::Semicolon,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Gt, // 45
            Token::Int("5".to_string()),
            Token::Semicolon,
            Token::If,
            Token::Lparen,
            Token::Int("5".to_string()),
            Token::Lt,
            Token::Int("10".to_string()),
            Token::Rparen,
            Token::Lbrace,
            Token::Return,
            Token::True,
            Token::Semicolon,
            Token::Rbrace,
            Token::Else,
            Token::Lbrace,
            Token::Return,
            Token::False,
            Token::Semicolon,
            Token::Rbrace,
            Token::Int("10".to_string()),
            Token::Eq,
            Token::Int("10".to_string()),
            Token::Semicolon,
            Token::Int("10".to_string()),
            Token::NotEq,
            Token::Int("9".to_string()),
            Token::Semicolon,
        ];

        let lexer = Lexer::new(sample_input);

        for (token, (idx, expected_token)) in lexer.zip(expected.into_iter().enumerate()) {
            assert_eq!(
                expected_token, token,
                "incorrect token at {}: {:?} VS {:?}",
                idx, expected_token, token
            );
        }
    }
}
