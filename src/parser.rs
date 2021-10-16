use super::expr::{Expr, Pattern};
use super::token::Token;

macro_rules! expect {
    ($token:expr, $tokens:ident) => {
        if $tokens.len() == 0 {
            return Err(ParseError::UnexpectedEof);
        }
        if $tokens[0] != $token {
            return Err(ParseError::UnexpectedToken($tokens[0].clone(), $token));
        }
        let $tokens = &$tokens[1..];
    }
}

#[derive(Clone, Debug)]
pub enum ParseError {
    NotIdentifier(Vec<Token>),
    NotNumber(Vec<Token>),
    UnexpectedEof,
    UnexpectedToken(Token, Token)
}

pub fn parse_let<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::Let, tokens);
    let (pat, tokens) = parse_pattern(tokens)?;
    expect!(Token::Eqs, tokens);
    let (v, tokens) = parse_expr(tokens)?;
    expect!(Token::In, tokens);
    let (e, tokens) = parse_expr(tokens)?;
    Ok((Expr::Let(pat, Box::new(v), Box::new(e)), tokens))
}

pub fn parse_expr<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    println!("calling parse expr with tokens {:?}", tokens);
    parse_application(tokens)
        .or_else(|_| parse_not_app(tokens))
}

pub fn parse_not_app<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    parse_variable(tokens)
        .or_else(|_| parse_number(tokens))
        .or_else(|_| parse_parens(tokens))
        .or_else(|_| parse_let(tokens))
        .or_else(|_| parse_function(tokens))
}

pub fn parse_identifier<'a>(tokens: &'a [Token]) -> Result<(String, &'a [Token]), ParseError> {
    if tokens.len() > 0 {
        if let Token::Identifier(i) = &tokens[0] {
            Ok((i.clone(), &tokens[1..]))
        } else {
            Err(ParseError::NotIdentifier(tokens.to_owned()))
        }
    } else {
        Err(ParseError::UnexpectedEof)
    }
}

pub fn parse_variable<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    let (v, tokens) = parse_identifier(tokens)?;
    Ok((Expr::Variable(v), tokens))
}

pub fn parse_number<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    if tokens.len() > 0 {
        if let Token::Number(i) = &tokens[0] {
            Ok((Expr::Number(*i), &tokens[1..]))
        } else {
            Err(ParseError::NotNumber(tokens.to_owned()))
        }
    } else {
        Err(ParseError::UnexpectedEof)
    }
}

pub fn make_app(terms: &[Expr]) -> Expr {
    if terms.len() == 2 {
        Expr::Application(Box::new(terms[0].clone()), Box::new(terms[1].clone()))
    } else if terms.len() < 2 {
        panic!("unreachable");
    } else {
        Expr::Application(Box::new(make_app(&terms[..(terms.len() - 1)])), Box::new(terms[terms.len() - 1].clone()))
    }
}

pub fn parse_application<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    println!("calling parse application with tokens {:?}", tokens);
    let mut terms : Vec<Expr> = Vec::new();
    let mut tokens = tokens;
    loop {
        let r = parse_not_app(tokens);
        println!("parsed non-application {:?}", r);
        if let Ok((temp, rest)) = r {
            tokens = rest;
            terms.push(temp);
        } else if let Err(_) = r {
            if terms.len() == 0 {
                println!("applications length was 0, returning error");
                return r;
            } else if terms.len() == 1 {
                println!("application of length 1, returning single terminal");
                return Ok((terms[0].clone(), tokens));
            } else {
                println!("application of length {}, building an application", terms.len());
                return Ok((make_app(&terms[..]), tokens));
            }
        }
    }
}

pub fn parse_parens<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::Lpar, tokens);
    let (e, tokens) = parse_expr(tokens)?;
    expect!(Token::Rpar, tokens);
    Ok((e, tokens))
}

pub fn parse_pattern<'a>(tokens: &'a [Token]) -> Result<(Pattern, &'a [Token]), ParseError> {
    let (v, tokens) = parse_identifier(tokens)?;
    Ok((Pattern::Variable(v), tokens))
}

fn make_fun(pats: &[Pattern], body: Expr) -> Expr {
    if pats.len() < 2 {
        Expr::Function(pats[0].clone(), Box::new(body))
    } else {
        Expr::Function(pats[0].clone(), Box::new(make_fun(&pats[1..], body)))
    }
}

pub fn parse_function<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::Fn, tokens);
    let mut tokens = tokens;
    let mut pats : Vec<Pattern> = Vec::new();
    loop {
        let r = parse_pattern(tokens);
        if let Ok((p, t)) = r {
            tokens = t;
            pats.push(p);
        } else if pats.len() == 0 {
            return Err(r.unwrap_err());
        } else {
            break;
        }
    }

    expect!(Token::Rarrow, tokens);
    let (b, tokens) = parse_expr(tokens)?;
    Ok((make_fun(&pats[..], b), tokens))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::scan;
    #[test]
    fn parse_var() {
        let (expr, _) = parse_variable(&scan("abc123")).unwrap();
        assert_eq!(expr, Expr::Variable(String::from("abc123")));
    }
    #[test]
    fn parse_num() {
        let (expr, _) = parse_number(&scan("3.14")).unwrap();
        assert_eq!(expr, Expr::Number(3.14));
    }
    #[test]
    fn parse_app() {
        let (expr, _) = parse_application(&scan("(a b)")).unwrap();
        assert_eq!(expr, 
                   Expr::Application(
                       Box::new(Expr::Variable(String::from("a"))), 
                       Box::new(Expr::Variable(String::from("b")))));
    }
    #[test]
    fn parse_fun() {
        let (expr, _) = parse_function(&scan("fn a -> 5")).unwrap();
        assert_eq!(expr,
                   Expr::Function(
                       Pattern::Variable(String::from("a")),
                       Box::new(Expr::Number(5.))));
    }
    #[test]
    fn parse_exp() {
        let (expr, _) = parse_expr(&scan("fn a -> fn b -> a")).unwrap();
        assert_eq!(expr,
                   Expr::Function(
                       Pattern::Variable(String::from("a")),
                       Box::new(Expr::Function(
                               Pattern::Variable(String::from("b")),
                               Box::new(Expr::Variable(String::from("a")))))));
        assert_eq!(
            parse_expr(&scan("fn a -> fn b -> a")).unwrap(),
            parse_expr(&scan("fn a b -> a")).unwrap());

        let (expr, _) = parse_expr(&scan("(fn x -> x x) (fn i -> i)")).unwrap();
        assert_eq!(expr,
                   Expr::Application(
                        Box::new(Expr::Function(
                                Pattern::Variable(String::from("x")),
                                Box::new(Expr::Application(
                                        Box::new(Expr::Variable(String::from("x"))),
                                        Box::new(Expr::Variable(String::from("x"))))))),
                        Box::new(Expr::Function(
                                Pattern::Variable(String::from("i")),
                                Box::new(Expr::Variable(String::from("i")))))));
    }
    #[test]
    fn parse_pars() {
        let (expr, _) = parse_parens(&scan("(5)")).unwrap();
        assert_eq!(expr, Expr::Number(5.));
    }
    #[test]
    fn parse_rassoc() {
        let (e1, _) = parse_expr(&scan("(a b) c")).unwrap();
        println!("");
        let (e2, _) = parse_expr(&scan("a b c")).unwrap();
        println!("");
        assert_eq!(e1, e2);
    }
    #[test]
    fn make_app_rassoc() {
        let a = make_app(&[Expr::Number(0.), Expr::Number(1.), Expr::Number(2.)]);
        assert_eq!(a, Expr::Application(Box::new(Expr::Application(Box::new(Expr::Number(0.)), Box::new(Expr::Number(1.)))), Box::new(Expr::Number(2.))))
    }
}
