macro_rules! matches {
    ($t:pat, $e:expr) => {
        if let $t = $e {true} else {false}
    }
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
    In
}

impl cmp::PartialEq for Token {
    fn eq(&self, other: &Token) -> bool {
        match self {
            Token::Lpar => matches!(Token::Lpar, other),
            Token::Rpar => matches!(Token::Rpar, other),
            Token::Fn => matches!(Token::Fn, other),
            Token::Rarrow => matches!(Token::Rarrow, other),
            Token::Identifier(i) => if let Token::Identifier(j) = other {
                i == j
            } else {false},
            Token::Number(i)  => if let Token::Number(j) = other {
                (i - j).abs() < 0.0001
            } else {false},
            Token::Let => matches!(Token::Let, other),
            Token::Eqs => matches!(Token::Eqs, other),
            Token::In => matches!(Token::In, other),
        }
    }
}
