macro_rules! make_bin {
    ($op:expr) => {
        (|l, r| {
            Expr::Application(
                Box::new(Expr::Application(
                    Box::new(Expr::Operator($op, ())),
                    Box::new(l),
                    (),
                )),
                Box::new(r),
                (),
            )
        }) as fn(_, _) -> _
    };
}

use super::expr::{Expr, Operator, Pattern, Statement};
use super::kind::Kind;
use super::token::Token;
use super::types::{bool_type, num_type, Type, Variant};
use chumsky::prelude::*;
use chumsky::Stream;
use core::ops::Range;
use std::rc::Rc;

/// `parse` will use a chumsky parser to turn a token slice into an untyped abstract syntax tree
/// # Errors
/// `parse` will only parse valid Parchment expressions. If an invalid token slice is given, it
/// will return a parse error
pub fn parse(
    tokens: Vec<(Token, Range<usize>)>,
    end: Range<usize>,
) -> Result<Expr<()>, Vec<Simple<Token>>> {
    parser().parse(Stream::from_iter(end, tokens.into_iter()))
}

fn record<L, R>(
    lhs: impl Parser<Token, L, Error = Simple<Token>> + Clone,
    rhs: impl Parser<Token, R, Error = Simple<Token>> + Clone,
) -> impl Parser<Token, Vec<(L, R)>, Error = Simple<Token>> + Clone {
    let pair = lhs.then_ignore(just(Token::Colon)).then(rhs);

    let field_list = pair
        .clone()
        .then_ignore(just(Token::Comma))
        .repeated()
        .then(pair)
        .map(|(xs, x)| xs.into_iter().chain(vec![x]).collect());
    field_list.delimited_by(just(Token::LBrace), just(Token::RBrace))
}

#[allow(clippy::too_many_lines)]
fn parser() -> impl Parser<Token, Expr<()>, Error = Simple<Token>> {
    recursive(|expr| {
        let constant = select!(
            Token::Number(n) => Expr::Number(n),
            Token::True => Expr::Boolean(true),
            Token::False => Expr::Boolean(false)
        );

        let variable = select!(
            Token::Identifier(s) => s
        );

        let constructor = select!(
            Token::Constructor(c) => c
        );

        let type_constant = select!(
            Token::Constructor(c) if &c == "Num" => Type::Constant(Rc::new(String::from("Num")), ()),
            Token::Constructor(c) if &c == "Bool" => Type::Constant(Rc::new(String::from("Bool")), ())        
        );

        let typ = recursive(|typ| {
            variable
                .map(|var| Type::Variable(Rc::new(var), ()))
                .or(type_constant.clone())
                .or(just(Token::Lpar)
                    .ignore_then(typ.clone())
                    .then(just(Token::Comma).ignore_then(typ.clone()).repeated())
                    .then_ignore(just(Token::Rpar))
                    .map(|(hd, mut tl)| {
                        tl.insert(0, hd);
                        Type::Tuple(tl)
                    }))
        });

        let variant = constructor
            .then(typ.clone().repeated())
            .map(|(name, fields)| Variant {
                constructor: name,
                fields,
            });

        let pattern = recursive(|pattern| {
            variable
                .map(|v| Pattern::Variable(v, ()))
                .or(record(variable, pattern.clone())
                    .map(|v| Pattern::Record(v.into_iter().collect())))
                .or(just(Token::Lpar)
                    .ignore_then(pattern.clone())
                    .then(just(Token::Comma).ignore_then(pattern.clone()).repeated())
                    .then_ignore(just(Token::Rpar))
                    .map(|(hd, mut tl)| {
                        tl.insert(0, hd);
                        Pattern::Tuple(tl)
                    }))
                .or(just(Token::Lpar)
                    .ignore_then(pattern.clone())
                    .then_ignore(just(Token::Rpar)))
                .or(constructor
                    .then(pattern.clone().repeated())
                    .map(|(c, es)| Pattern::Construction(c, es, ())))
        });

        let function = just(Token::Fn)
            .ignore_then(pattern.clone().repeated())
            .then_ignore(just(Token::Rarrow))
            .then(expr.clone())
            .foldr(|p, e| Expr::Function(p, Box::new(e), ()));

        let statement = just(Token::Let)
            .ignore_then(pattern.clone())
            .then_ignore(just(Token::Eqs))
            .then(expr.clone())
            .map(|(p, e)| Statement::Let(p, e, ()))
            .or(expr.clone().map(Statement::Raw))
            .then_ignore(just(Token::Semicolon))
            .or(just(Token::Type)
                .ignore_then(variable)
                .then(variable.map(Rc::new).repeated())
                .then_ignore(just(Token::Eqs))
                .then(variant.clone())
                .then(just(Token::Pipe).ignore_then(variant.clone()).repeated())
                .map(|(((name, tvs), hd), mut tl)| {
                    tl.insert(0, hd);
                    Statement::TypeDef(name, tvs, tl.into_iter().collect())
                })
                .then_ignore(just(Token::Semicolon)));

        let block = just(Token::LBrace)
            .ignore_then(statement.repeated())
            .then_ignore(just(Token::RBrace))
            .map(Expr::Block);

        let if_ = just(Token::If)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::Then))
            .then(expr.clone())
            .then_ignore(just(Token::Else))
            .then(expr.clone())
            .map(|((p, c), a)| Expr::If(Box::new(p), Box::new(c), Box::new(a)));

        let parens = expr
            .clone()
            .delimited_by(just(Token::Lpar), just(Token::Rpar));

        let record_lit =
            record(variable, expr.clone()).map(|v| Expr::Record(v.into_iter().collect()));

        let tuple = just(Token::Lpar)
            .ignore_then(expr.clone())
            .then(just(Token::Comma).ignore_then(expr.clone()).repeated())
            .then_ignore(just(Token::Rpar))
            .map(|(e, es)| {
                let mut terms = vec![e];
                terms.extend(es);

                Expr::Tuple(terms)
            });

        let match_expr = just(Token::Match)
            .ignore_then(expr.clone())
            .then_ignore(just(Token::LBrace))
            .then(
                pattern
                    .clone()
                    .then_ignore(just(Token::Rarrow))
                    .then(expr.clone())
                    .then_ignore(just(Token::Comma))
                    .repeated(),
            )
            .then_ignore(just(Token::RBrace))
            .map(|(matchand, arms)| Expr::Match(Box::new(matchand), arms, ()));

        let atom = parens
            .or(function)
            .or(constant)
            .or(record_lit)
            .or(block)
            .or(if_)
            .or(tuple)
            .or(match_expr)
            .or(constructor.map(|c| Expr::Constructor(c, ())))
            .or(variable.map(|v| Expr::Variable(v, ())));

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

        let less_than = make_bin!(Operator::LessThan);
        let less_than_equals = make_bin!(Operator::LessThanEqual);
        let greater_than = make_bin!(Operator::GreaterThan);
        let greater_than_equals = make_bin!(Operator::GreaterThanEqual);

        let cmp = sum
            .clone()
            .then(
                just(Token::LessThan)
                    .to(less_than)
                    .or(just(Token::LessThanEquals).to(less_than_equals))
                    .or(just(Token::GreaterThan).to(greater_than))
                    .or(just(Token::GreaterThanEquals).to(greater_than_equals))
                    .then(sum)
                    .repeated(),
            )
            .foldl(|l, (f, r)| f(l, r));

        let equals = make_bin!(Operator::Equals);
        let eqs = cmp
            .clone()
            .then(just(Token::DoubleEqs).to(equals).then(cmp).repeated())
            .foldl(|l, (f, r)| f(l, r));

        eqs.clone()
            .then(eqs.repeated())
            .foldl(|lhs, rhs| Expr::Application(Box::new(lhs), Box::new(rhs), ()))
    })
    .then_ignore(end())
}
