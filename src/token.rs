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
    Plus,
    Minus,
    Times,
    DoubleEqs,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Token::Plus => matches!(Token::Plus, other),
            Token::Minus => matches!(Token::Minus, other),
            Token::Times => matches!(Token::Times, other),
            Token::DoubleEqs => matches!(Token::DoubleEqs, other),
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
            Token::LessThan => matches!(Token::LessThan, other),
            Token::LessThanEquals => matches!(Token::LessThanEquals, other),
            Token::GreaterThan => matches!(Token::GreaterThan, other),
            Token::GreaterThanEquals => matches!(Token::GreaterThanEquals, other),
        }
    }
}

impl std::hash::Hash for Token {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write(&[match self {
            Token::Lpar => 0,
            Token::Rpar => 1,
            Token::Identifier(_) => 2,
            Token::Fn => 3,
            Token::Rarrow => 4,
            Token::Number(_) => 5,
            Token::Let => 6,
            Token::Eqs => 7,
            Token::In => 8,
            Token::LBrace => 9,
            Token::RBrace => 10,
            Token::Colon => 11,
            Token::Comma => 12,
            Token::If => 13,
            Token::Then => 14,
            Token::Else => 15,
            Token::True => 16,
            Token::False => 17,
            Token::Match => 18,
            Token::Semicolon => 19,
            Token::Plus => 20,
            Token::Minus => 21,
            Token::Times => 22,
            Token::DoubleEqs => 23,
            Token::LessThan => 24,
            Token::LessThanEquals => 25,
            Token::GreaterThan => 26,
            Token::GreaterThanEquals => 27,
        }])
    }
}

impl cmp::Eq for Token {}
