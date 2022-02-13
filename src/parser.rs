use super::expr::{Expr, Pattern, Statement};
use super::token::Token;
use im::hashmap::HashMap;
use std::result;

macro_rules! expect {
    ($token:expr, $tokens:ident, $can_fail:expr) => {
        if $tokens.len() == 0 {
            return if $can_fail {
                Ok(Err(ParseError::UnexpectedEof))
            } else {
                Err(ParseError::UnexpectedEof)
            };
        }
        if $tokens[0] != $token {
            let e = ParseError::UnexpectedToken($tokens[0].clone(), $token);
            return if $can_fail { Ok(Err(e)) } else { Err(e) };
        }
        $tokens = &$tokens[1..];
    };
}

macro_rules! check_eof {
    ($tokens:ident) => {
        if $tokens.len() == 0 {
            return Ok(Err(ParseError::UnexpectedEof));
        }
    };
}

#[derive(Clone, Debug)]
pub enum ParseError {
    NotIdentifier(Token),
    NotNumber(Token),
    NotBoolean(Token),
    UnexpectedEof,
    UnexpectedToken(Token, Token),
    ExpectedAtLeastOneTerm,
    ExpectedAtLeastOnePattern,
}

pub type Result<'a, O> = result::Result<result::Result<(O, &'a [Token]), ParseError>, ParseError>;

pub fn many<'a, O, P: Fn(&'a [Token]) -> Result<O>>(
    p: P,
    mut text: &'a [Token],
) -> Result<'a, Vec<O>> {
    let mut res = Vec::new();
    while let Ok((v, rest)) = p(text)? {
        res.push(v);
        text = rest;
    }
    Ok(Ok((res, text)))
}

pub fn parse_num(text: &[Token]) -> Result<f64> {
    let token = text.get(0);
    if let Some(Token::Number(n)) = token {
        Ok(Ok((*n, &text[1..])))
    } else if let Some(t) = token {
        Ok(Err(ParseError::NotNumber(t.clone())))
    } else {
        Ok(Err(ParseError::UnexpectedEof))
    }
}

pub fn parse_id(text: &[Token]) -> Result<String> {
    let token = text.get(0);
    if let Some(Token::Identifier(i)) = token {
        Ok(Ok((i.clone(), &text[1..])))
    } else if let Some(t) = token {
        Ok(Err(ParseError::NotIdentifier(t.clone())))
    } else {
        Ok(Err(ParseError::UnexpectedEof))
    }
}

pub fn parse_fun(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::Fn, text, true);
    let (p, mut text) = many(parse_pattern, text)??;
    expect!(Token::Rarrow, text, false);
    let (b, text) = parse_expr(text)??;
    if p.is_empty() {
        return Err(ParseError::ExpectedAtLeastOnePattern);
    }
    let mut out = Expr::Function(p[p.len() - 1].clone(), Box::new(b));
    for i in (0..p.len() - 1).rev() {
        out = Expr::Function(p[i].clone(), Box::new(out));
    }
    Ok(Ok((out, text)))
}

pub fn parse_app(text: &[Token]) -> Result<Expr<String>> {
    let (apps, text) = match many(parse_non_app, text)? {
        Err(e) => return Ok(Err(e)),
        Ok(r) => r,
    };
    if apps.is_empty() {
        return Ok(Err(ParseError::ExpectedAtLeastOneTerm));
    }
    if apps.len() == 1 {
        return Ok(Ok((apps[0].clone(), text)));
    }
    let mut res = Expr::Application(Box::new(apps[0].clone()), Box::new(apps[1].clone()));
    for e in apps.into_iter().skip(2) {
        res = Expr::Application(Box::new(res), Box::new(e));
    }
    Ok(Ok((res, text)))
}

pub fn parse_paren(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::Lpar, text, true);
    let (e, mut text) = parse_expr(text)??;
    expect!(Token::Rpar, text, false);
    Ok(Ok((e, text)))
}

pub fn parse_if(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::If, text, true);
    let (p, mut text) = parse_expr(text)??;
    expect!(Token::Then, text, false);
    let (c, mut text) = parse_expr(text)??;
    expect!(Token::Else, text, false);
    let (a, text) = parse_expr(text)??;

    Ok(Ok((Expr::If(Box::new(p), Box::new(c), Box::new(a)), text)))
}

pub fn parse_non_app(text: &[Token]) -> Result<Expr<String>> {
    Ok(parse_num(text)?
        .map(|(n, text)| (Expr::Number(n), text))
        .or_else(|_| parse_boolean(text)?.map(|(b, text)| (Expr::Boolean(b), text)))
        .or_else(|_| parse_id(text)?.map(|(id, text)| (Expr::Variable(id), text)))
        .or_else(|_| parse_fun(text)?)
        .or_else(|_| parse_if(text)?)
        .or_else(|_| parse_paren(text)?)
        .or_else(|_| parse_match(text)?)
        .or_else(|_| parse_record_lit(text)?)
        .or_else(|_| parse_block(text)?))
}

pub fn parse_expr(text: &[Token]) -> Result<Expr<String>> {
    parse_app(text)
}

pub fn parse_pattern(text: &[Token]) -> Result<Pattern<String>> {
    Ok(parse_pattern_var(text)?.or_else(|_| parse_pattern_record(text)?))
}

pub fn parse_pattern_var(text: &[Token]) -> Result<Pattern<String>> {
    Ok(parse_id(text)?.map(|(id, text)| (Pattern::Variable(id), text)))
}

pub fn parse_pattern_record(mut text: &[Token]) -> Result<Pattern<String>> {
    expect!(Token::LBrace, text, true);
    check_eof!(text);

    let mut h = HashMap::new();
    while text[0] != Token::RBrace {
        let (v, rest) = parse_id(text)??;
        text = rest;
        expect!(Token::Colon, text, false);
        let (p, rest) = parse_pattern(text)??;
        text = rest;
        expect!(Token::Comma, text, false);
        h.insert(v, p);

        check_eof!(text);
    }
    expect!(Token::RBrace, text, false);
    Ok(Ok((Pattern::Record(h), text)))
}

pub fn parse_statement(text: &[Token]) -> Result<Statement<String>> {
    Ok(parse_let(text)?.or_else(|_| parse_expr_statement(text)?))
}

pub fn parse_let(mut text: &[Token]) -> Result<Statement<String>> {
    expect!(Token::Let, text, true);
    let (p, mut text) = parse_pattern(text)??;
    expect!(Token::Eqs, text, false);
    let (e, mut text) = parse_expr(text)??;
    expect!(Token::Semicolon, text, false);
    Ok(Ok((Statement::Let(p, e), text)))
}

pub fn parse_expr_statement(text: &[Token]) -> Result<Statement<String>> {
    let (e, mut text) = parse_expr(text)??;
    expect!(Token::Semicolon, text, false);
    Ok(Ok((Statement::Raw(e), text)))
}

pub fn parse_block(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::LBrace, text, true);
    check_eof!(text);
    let mut statements = Vec::new();
    while text[0] != Token::RBrace {
        let (s, rest) = parse_statement(text)??;
        text = rest;
        statements.push(s);
        check_eof!(text);
    }
    expect!(Token::RBrace, text, false);
    Ok(Ok((Expr::Block(statements), text)))
}

pub fn parse_match(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::Match, text, true);
    let (e, mut text) = parse_expr(text)??;
    expect!(Token::LBrace, text, false);
    let mut terms = Vec::new();
    check_eof!(text);
    while text[0] != Token::RBrace {
        let (p, mut rest) = parse_pattern(text)??;
        expect!(Token::Rarrow, rest, false);
        let (e, rest) = parse_expr(rest)??;
        text = rest;
        terms.push((p, e))
    }
    expect!(Token::RBrace, text, false);
    Ok(Ok((Expr::Match(Box::new(e), terms), text)))
}

pub fn parse_boolean(text: &[Token]) -> Result<bool> {
    check_eof!(text);
    if text[0] == Token::True {
        Ok(Ok((true, &text[1..])))
    } else if text[0] == Token::False {
        Ok(Ok((false, &text[1..])))
    } else {
        Ok(Err(ParseError::NotBoolean(text[0].clone())))
    }
}

pub fn parse_record_lit(mut text: &[Token]) -> Result<Expr<String>> {
    expect!(Token::LBrace, text, true);

    let mut h = HashMap::new();

    check_eof!(text);
    if let Ok((field, rest)) = parse_id(text)? {
        text = rest;
        expect!(Token::Colon, text, true);

        let (val, rest) = parse_expr(text)??;
        text = rest;
        expect!(Token::Comma, text, false);
        check_eof!(text);
        h.insert(field, val);
    } else {
        return Ok(Err(ParseError::NotIdentifier(text[0].clone())));
    }
    while text[0] != Token::RBrace {
        let (field, rest) = parse_id(text)??;
        text = rest;

        expect!(Token::Colon, text, false);

        let (val, rest) = parse_expr(text)??;
        text = rest;

        expect!(Token::Comma, text, false);

        check_eof!(text);

        h.insert(field, val);
    }

    expect!(Token::RBrace, text, false);
    Ok(Ok((Expr::Record(h), text)))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::scan;
    #[test]
    fn parse_var() {
        let (expr, _) = parse_id(&scan("abc123")).unwrap().unwrap();
        assert_eq!(expr, String::from("abc123"));
    }
    #[test]
    fn parse_number() {
        let (expr, _) = parse_num(&scan("3.14")).unwrap().unwrap();
        assert_eq!(expr, 3.14);
    }
    #[test]
    fn parse_app() {
        let (expr, _) = parse_paren(&scan("(a b)")).unwrap().unwrap();
        assert_eq!(
            expr,
            Expr::Application(
                Box::new(Expr::Variable(String::from("a"))),
                Box::new(Expr::Variable(String::from("b")))
            )
        );
    }
    #[test]
    fn parse_function() {
        let (expr, _) = parse_fun(&scan("fn a -> 5")).unwrap().unwrap();
        assert_eq!(
            expr,
            Expr::Function(
                Pattern::Variable(String::from("a")),
                Box::new(Expr::Number(5.))
            )
        );
    }
    #[test]
    fn parse_exp() {
        let (expr, _) = parse_expr(&scan("fn a -> fn b -> a")).unwrap().unwrap();
        assert_eq!(
            expr,
            Expr::Function(
                Pattern::Variable(String::from("a")),
                Box::new(Expr::Function(
                    Pattern::Variable(String::from("b")),
                    Box::new(Expr::Variable(String::from("a")))
                ))
            )
        );
        assert_eq!(
            parse_expr(&scan("fn a -> fn b -> a")).unwrap().unwrap(),
            parse_expr(&scan("fn a b -> a")).unwrap().unwrap()
        );

        let (expr, _) = parse_expr(&scan("(fn x -> x x) (fn i -> i)"))
            .unwrap()
            .unwrap();
        assert_eq!(
            expr,
            Expr::Application(
                Box::new(Expr::Function(
                    Pattern::Variable(String::from("x")),
                    Box::new(Expr::Application(
                        Box::new(Expr::Variable(String::from("x"))),
                        Box::new(Expr::Variable(String::from("x")))
                    ))
                )),
                Box::new(Expr::Function(
                    Pattern::Variable(String::from("i")),
                    Box::new(Expr::Variable(String::from("i")))
                ))
            )
        );
    }
    #[test]
    fn parse_pars() {
        let (expr, _) = parse_paren(&scan("(5)")).unwrap().unwrap();
        assert_eq!(expr, Expr::Number(5.));
    }
    #[test]
    fn parse_rassoc() {
        let (e1, _) = parse_expr(&scan("(a b) c")).unwrap().unwrap();
        let (e2, _) = parse_expr(&scan("a b c")).unwrap().unwrap();
        assert_eq!(e1, e2);
    }
    #[test]
    fn parse_record() {
        let (expr, _) = parse_expr(&scan("{a:1,}")).unwrap().unwrap();
        assert_eq!(
            expr,
            Expr::Record(im::hashmap! {
                String::from("a") => Expr::Number(1.)
            })
        );

        let (expr, _) = parse_expr(&scan("{a:1, b:2,}")).unwrap().unwrap();
        assert_eq!(
            expr,
            Expr::Record(im::hashmap! {
                String::from("a") => Expr::Number(1.),
                String::from("b") => Expr::Number(2.)
            })
        );
    }
    #[test]
    fn parse_patterns() {
        let (p, _) = parse_pattern(&scan("a")).unwrap().unwrap();
        assert_eq!(p, Pattern::Variable(String::from("a")));
        let (p, _) = parse_pattern(&scan("{a:a,}")).unwrap().unwrap();
        assert_eq!(
            p,
            Pattern::Record(im::hashmap! {
                String::from("a") => Pattern::Variable(String::from("a"))
            })
        );
    }
    #[test]
    fn record_field_punning() {
        todo!()
    }
    #[test]
    fn record_field_trailing_comma() {
        todo!()
    }
    #[test]
    fn pattern_field_punning() {
        todo!()
    }
    #[test]
    fn pattern_field_trailing_comma() {
        todo!()
    }
}
