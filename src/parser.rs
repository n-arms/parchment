use super::expr::{Expr, Pattern};
use super::token::Token;
use im::hashmap::HashMap;

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

pub fn parse_match<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::Match, tokens);
    let (m, tokens) = parse_expr(tokens)?;
    expect!(Token::With, tokens);
    let mut rest = tokens;
    let mut contents : Vec<(Pattern, Expr)> = Vec::new();
    loop {
        let (p, tokens) = parse_pattern(rest)?;
        expect!(Token::Rarrow, tokens);
        let (e, tokens) = parse_expr(tokens)?;
        if tokens.len() == 0 {return Err(ParseError::UnexpectedEof)}
        contents.push((p, e));
        rest = tokens;
        if tokens[0] != Token::Comma {break}
        rest = &tokens[1..];
    }
    if rest.len() == 0 {return Err(ParseError::UnexpectedEof);}
    expect!(Token::End, rest);
    Ok((Expr::Match(Box::new(m), contents), rest))
}

pub fn parse_if<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::If, tokens);
    let (p, tokens) = parse_expr(tokens)?;
    expect!(Token::Then, tokens);
    let (e1, tokens) = parse_expr(tokens)?;
    expect!(Token::Else, tokens);
    let (e2, tokens) = parse_expr(tokens)?;
    Ok((Expr::If(Box::new(p), Box::new(e1), Box::new(e2)), tokens))
}

pub fn parse_record<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    expect!(Token::LBrace, tokens);
    let mut contents : HashMap<String, Expr> = HashMap::new();
    let mut rest = tokens;
    loop {
        if let Ok((field, tokens)) = parse_identifier(rest) {
            expect!(Token::Colon, tokens);
            let (value, tokens) = parse_expr(tokens)?;
            contents = contents.update(field, value);
            if tokens.len() == 0 {return Err(ParseError::UnexpectedEof);}
            if tokens[0] == Token::RBrace {
                return Ok((Expr::Record(contents), &tokens[1..]));
            }
            expect!(Token::Comma, tokens);
            rest = tokens;
        } else {
            expect!(Token::RBrace, rest);
            return Ok((Expr::Record(contents), rest));
        }
    }
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

pub fn parse_boolean<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    if tokens.len() == 0 {
        return Err(ParseError::UnexpectedEof);
    }
    if let Token::True = tokens[0] {
        Ok((Expr::Boolean(true), &tokens[1..]))
    } else if let Token::False = tokens[0] {
        Ok((Expr::Boolean(false), &tokens[1..]))
    } else {
        Err(ParseError::UnexpectedToken(Token::False, tokens[0].clone()))
    }
}

pub fn parse_expr<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    parse_application(tokens)
        .or_else(|_| parse_not_app(tokens))
}

pub fn parse_not_app<'a>(tokens: &'a [Token]) -> Result<(Expr, &'a [Token]), ParseError> {
    parse_boolean(tokens)
        .or_else(|_| parse_record(tokens))
        .or_else(|_| parse_number(tokens))
        .or_else(|_| parse_variable(tokens))
        .or_else(|_| parse_parens(tokens))
        .or_else(|_| parse_let(tokens))
        .or_else(|_| parse_if(tokens))
        .or_else(|_| parse_function(tokens))
        .or_else(|_| parse_match(tokens))
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
    let mut terms : Vec<Expr> = Vec::new();
    let mut tokens = tokens;
    loop {
        let r = parse_not_app(tokens);
        if let Ok((temp, rest)) = r {
            tokens = rest;
            terms.push(temp);
        } else if let Err(_) = r {
            if terms.len() == 0 {
                return r;
            } else if terms.len() == 1 {
                return Ok((terms[0].clone(), tokens));
            } else {
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
/*
pub fn parse_pattern_record<'a>(tokens: &'a [Token]) -> Result<(Pattern, &'a [Token]), ParseError> {
    expect!(Token::LBrace, tokens);
    let mut rest = tokens;
    println!("parsing pattern record with rest {:?}", tokens);
    let mut contents = HashMap::new();
    loop {
        if let Ok((field, tokens)) = parse_identifier(rest) {
            if tokens.len() > 1 && tokens[0] == Token::Colon {
                let (value, tokens) = parse_pattern(&tokens[1..])?;
                rest = tokens;
                contents.insert(field, value);
            } else {
                rest = tokens;
                contents.insert(field.clone(), Pattern::Variable(field));
            }
        } else {break;}
    }
    expect!(Token::RBrace, rest);
    Ok((Pattern::Record(contents), rest))
}
*/

pub fn parse_pattern_record<'a>(tokens: &'a [Token]) -> Result<(Pattern, &'a [Token]), ParseError> {
    expect!(Token::LBrace, tokens);
    let mut contents = HashMap::new();
    let mut rest = tokens;
    loop {
        if rest.len() == 0 {
            return Err(ParseError::UnexpectedEof);
        }
        if rest[0] == Token::RBrace {
            return Ok((Pattern::Record(contents), &rest[1..]));
        } else {
            let (i, tokens) = parse_identifier(rest)?;
            rest = tokens;
            if rest.len() == 0 {
                return Err(ParseError::UnexpectedEof);
            }
            if rest[0] == Token::Colon {
                let (p, tokens) = parse_pattern(&rest[1..])?;
                contents.insert(i, p);
                rest = tokens;
            } else {
                contents.insert(i.clone(), Pattern::Variable(i));
            }
            if rest.len() == 0 {
                return Err(ParseError::UnexpectedEof);
            }
            if rest[0] == Token::RBrace {
                return Ok((Pattern::Record(contents), &rest[1..]));
            } else if rest[0] == Token::Comma {
                rest = &rest[1..];
            }
        }
    }
}

pub fn parse_pattern<'a>(tokens: &'a [Token]) -> Result<(Pattern, &'a [Token]), ParseError> {
    parse_pattern_record(tokens)
        .or_else(|_| {
            let (v, tokens) = parse_identifier(tokens)?;
            Ok((Pattern::Variable(v), tokens))
        })
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
        let (e2, _) = parse_expr(&scan("a b c")).unwrap();
        assert_eq!(e1, e2);
    }
    #[test]
    fn make_app_rassoc() {
        let a = make_app(&[Expr::Number(0.), Expr::Number(1.), Expr::Number(2.)]);
        assert_eq!(a, Expr::Application(Box::new(Expr::Application(Box::new(Expr::Number(0.)), Box::new(Expr::Number(1.)))), Box::new(Expr::Number(2.))))
    }
    #[test]
    fn parse_record() {
        let (expr, _) = parse_expr(&scan("{}")).unwrap();
        assert_eq!(expr, Expr::Record(im::hashmap!{}));

        let (expr, _) = parse_expr(&scan("{a:1}")).unwrap();
        assert_eq!(expr, Expr::Record(im::hashmap!{
            String::from("a") => Expr::Number(1.)
        }));

        let (expr, _) = parse_expr(&scan("{a:1,}")).unwrap();
        assert_eq!(expr, Expr::Record(im::hashmap!{
            String::from("a") => Expr::Number(1.)
        }));

        let (expr, _) = parse_expr(&scan("{a:1, b:2}")).unwrap();
        assert_eq!(expr, Expr::Record(im::hashmap!{
            String::from("a") => Expr::Number(1.),
            String::from("b") => Expr::Number(2.)
        }));
    }
    #[test]
    fn parse_patterns() {
        let (p, _) = parse_pattern(&scan("a")).unwrap();
        assert_eq!(p, Pattern::Variable(String::from("a")));
        let (p, _) = parse_pattern(&scan("{a:a}")).unwrap();
        assert_eq!(p, Pattern::Record(im::hashmap!{
            String::from("a") => Pattern::Variable(String::from("a"))
        }));
        let (p, _) = parse_pattern(&scan("{a}")).unwrap();
        assert_eq!(p, Pattern::Record(im::hashmap!{
            String::from("a") => Pattern::Variable(String::from("a"))
        }));
        println!("\nPARSING HARD PATTERN\n");
        let (p, _) = parse_pattern(&scan("{a,b}")).unwrap();
        assert_eq!(p, Pattern::Record(im::hashmap!{
            String::from("a") => Pattern::Variable(String::from("a")),
            String::from("b") => Pattern::Variable(String::from("b"))
        }));
    }
}
