macro_rules! make_bin {
    ($op:expr) => {
        (|l, r| {
            Expr::Application(
                Box::new(Expr::Application(
                    Box::new(Expr::Operator($op)),
                    Box::new(l),
                )),
                Box::new(r),
            )
        }) as fn(_, _) -> _
    };
}

use super::expr::{Expr, Operator, Pattern, Statement};
use super::token::Token;
use chumsky::prelude::*;

pub fn parse(t: &[Token]) -> Result<Expr<String>, Vec<Simple<Token>>> {
    parser().parse(t)
}

fn record<L, R>(
    lhs: impl Parser<Token, L, Error = Simple<Token>> + Clone,
    rhs: impl Parser<Token, R, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Vec<(L, R)>, Error = Simple<Token>> + Clone {

    let pair = lhs.clone()
        .then_ignore(just(Token::Colon))
        .then(rhs.clone());

    let field_list = pair.clone().then_ignore(just(Token::Comma))
        .repeated()
        .then(pair)
        .map(|(xs, x)| xs.into_iter().chain(vec![x]).collect());
    field_list
        .delimited_by(just(Token::LBrace), just(Token::RBrace))
}

fn parser() -> impl Parser<Token, Expr<String>, Error = Simple<Token>> {
    recursive(|expr| {
        let constant = select!(
            Token::Number(n) => Expr::Number(n),
            Token::True => Expr::Boolean(true),
            Token::False => Expr::Boolean(false)
        );
        let variable = select!(
            Token::Identifier(s) => s
        );

        let pattern = recursive(|pattern| {
            variable
                .clone()
                .map(Pattern::Variable)
                .or(record(variable.clone(), pattern.clone())
                    .map(|v| Pattern::Record(v.into_iter().collect())))
        });

        let function = just(Token::Fn)
            .ignore_then(pattern.clone().repeated())
            .then_ignore(just(Token::Rarrow))
            .then(expr.clone())
            .foldr(|p, e| Expr::Function(p, Box::new(e)));

        let statement = just(Token::Let)
            .ignore_then(pattern)
            .then_ignore(just(Token::Eqs))
            .then(expr.clone())
            .map(|(p, e)| Statement::Let(p, e))
            .or(expr.clone()
                .map(Statement::Raw))
            .then_ignore(just(Token::Semicolon));

        let block = just(Token::LBrace)
            .ignore_then(statement.repeated())
            .then_ignore(just(Token::RBrace))
            .map(Expr::Block);

        let parens = expr.clone().delimited_by(just(Token::Lpar), just(Token::Rpar));
        let record_lit = record(variable.clone(), expr.clone()).map(|v| Expr::Record(v.into_iter().collect()));
        let atom = parens
            .or(function)
            .or(constant)
            .or(record_lit)
            .or(block)
            .or(variable.map(Expr::Variable));

        let make_times = make_bin!(Operator::Times);
        let product = atom
            .clone()
            .then(just(Token::Times).to(make_times).then(atom).repeated())
            .foldl(|l, (f, r)| f(l, r));

        let make_plus = make_bin!(Operator::Plus);
        let make_minus = make_bin!(Operator::Minus);
        let sum = product
            .clone()
            .then(
                just(Token::Plus)
                    .to(make_plus)
                    .or(just(Token::Minus).to(make_minus))
                    .then(product)
                    .repeated(),
            )
            .foldl(|l, (f, r)| f(l, r));

        let expr = sum
            .clone()
            .then(sum.repeated())
            .foldl(|lhs, rhs| Expr::Application(Box::new(lhs), Box::new(rhs)));
        expr
    })
    .then_ignore(end())
}
