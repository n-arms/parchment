macro_rules! matches {
    ($t:pat, $e:expr) => {
        if let $t = $e {
            true
        } else {
            false
        }
    };
}

use std::cmp;

#[derive(Clone, Debug)]
pub enum Token {
    Lpar,
    Rpar,
    Identifier(String),
    Fn,
    Rarrow,
    Number(f64),
    Let,
    Eqs,
    In,
    LBrace,
    RBrace,
    Colon,
    Comma,
    If,
    Then,
    Else,
    True,
    False,
    Match,
    Semicolon,
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Token::Lpar => matches!(Token::Lpar, other),
            Token::Rpar => matches!(Token::Rpar, other),
            Token::Fn => matches!(Token::Fn, other),
            Token::Rarrow => matches!(Token::Rarrow, other),
            Token::Identifier(i) => {
                if let Token::Identifier(j) = other {
                    i == j
                } else {
                    false
                }
            }
            Token::Number(i) => {
                if let Token::Number(j) = other {
                    (i - j).abs() < 0.0001
                } else {
                    false
                }
            }
            Token::Let => matches!(Token::Let, other),
            Token::Eqs => matches!(Token::Eqs, other),
            Token::In => matches!(Token::In, other),
            Token::LBrace => matches!(Token::LBrace, other),
            Token::RBrace => matches!(Token::RBrace, other),
            Token::Colon => matches!(Token::Colon, other),
            Token::Comma => matches!(Token::Comma, other),
            Token::If => matches!(Token::If, other),
            Token::Then => matches!(Token::Then, other),
            Token::Else => matches!(Token::Else, other),
            Token::True => matches!(Token::True, other),
            Token::False => matches!(Token::False, other),
            Token::Match => matches!(Token::Match, other),
            Token::Semicolon => matches!(Token::Semicolon, other),
        }
    }
}
