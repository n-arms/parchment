use super::kind::Kind;
use super::types::{bool_type, num_type, unit_type, Type, Var, Variant};
use im::{HashMap, HashSet};
use rand::prelude::*;
use std::cmp;
use std::fmt::{self, Debug};
use std::hash::Hash;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<A: std::clone::Clone> {
    /// the type of the variable
    Variable(String, A),
    Record(HashMap<String, Pattern<A>>),
    Tuple(Vec<Pattern<A>>),
    /// a construction that matches an expression of type A
    Construction(String, Vec<Pattern<A>>, A),
}

#[derive(Clone, Debug)]
pub enum Expr<A: std::clone::Clone> {
    /// a function with a domain of type A
    Function(Pattern<A>, Box<Expr<A>>, A),
    /// an application that will return type A
    Application(Box<Expr<A>>, Box<Expr<A>>, A),
    Number(f64),
    Boolean(bool),
    Operator(Operator, A),
    Variable(String, A),
    Record(HashMap<String, Expr<A>>),
    If(Box<Expr<A>>, Box<Expr<A>>, Box<Expr<A>>),
    /// a match expression that will return type A
    Match(Box<Expr<A>>, Vec<(Pattern<A>, Expr<A>)>, A),
    Block(Vec<Statement<A>>),
    Tuple(Vec<Expr<A>>),
    Constructor(String, A),
}

impl Expr<Type<Kind>> {
    pub fn free_vars<'a>(&'a self) -> HashSet<(&'a str, Type<Kind>)> {
        match self {
            Expr::Function(pattern, body, _) => {
                body.free_vars().relative_complement(pattern.bound_vars())
            }
            Expr::Application(e1, e2, _) => e1.free_vars().union(e2.free_vars()),
            Expr::Constructor(..) | Expr::Operator(..) | Expr::Number(_) | Expr::Boolean(_) => {
                HashSet::new()
            }
            Expr::Variable(var, var_type) => HashSet::unit((var, var_type.clone())),
            Expr::Record(r) => r.values().flat_map(Expr::free_vars).collect(),
            Expr::Tuple(es) => es.iter().flat_map(Expr::free_vars).collect(),
            Expr::If(p, c, a) => p.free_vars().union(c.free_vars()).union(a.free_vars()),
            Expr::Block(b) => free_block(b),
            Expr::Match(matchand, arms, _) => matchand.free_vars().union(
                arms.iter()
                    .flat_map(|(pattern, expr)| {
                        expr.free_vars().relative_complement(pattern.bound_vars())
                    })
                    .collect(),
            ),
        }
    }
}

fn free_block<'a>(b: &'a [Statement<Type<Kind>>]) -> HashSet<(&'a str, Type<Kind>)> {
    match b {
        [Statement::Let(pattern, body, _), rest @ ..] => body
            .free_vars()
            .union(free_block(rest))
            .relative_complement(pattern.bound_vars()),
        [Statement::Raw(e), rest @ ..] => e.free_vars().union(free_block(rest)),
        [Statement::TypeDef(..), rest @ ..] => free_block(rest),
        [] => HashSet::new(),
    }
}
impl Expr<Type<Kind>> {
    pub fn get_type(&self) -> Type<Kind> {
        match self {
            Expr::Function(_, body, pattern_type) => {
                Type::Arrow(Rc::new(pattern_type.clone()), Rc::new(body.get_type()))
            }
            Expr::Application(_, _, app_type) => app_type.clone(),
            Expr::Number(_) => num_type(),
            Expr::Boolean(_) => bool_type(),
            Expr::Record(record) => Type::Record(
                record
                    .iter()
                    .map(|(val, var)| (val.clone(), var.get_type()))
                    .collect(),
            ),
            Expr::If(_, expr, _) => expr.get_type(),
            Expr::Match(_, _, match_type) => match_type.clone(),
            Expr::Block(block) => block.last().map_or_else(unit_type, Statement::get_type),
            Expr::Tuple(tuple) => Type::Tuple(tuple.iter().map(Expr::get_type).collect()),
            Expr::Operator(_, t) | Expr::Constructor(_, t) | Expr::Variable(_, t) => t.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    And,
    Or,
    Equals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

impl Operator {
    pub fn argument_type(&self) -> Type<Kind> {
        match self {
            Operator::Plus
            | Operator::Minus
            | Operator::Times
            | Operator::Equals
            | Operator::LessThan
            | Operator::LessThanEqual
            | Operator::GreaterThan
            | Operator::GreaterThanEqual => num_type(),
            Operator::And | Operator::Or => bool_type(),
        }
    }

    pub fn result_type(&self) -> Type<Kind> {
        match self {
            Operator::Plus | Operator::Minus | Operator::Times => num_type(),
            Operator::Equals
            | Operator::LessThan
            | Operator::LessThanEqual
            | Operator::GreaterThan
            | Operator::GreaterThanEqual
            | Operator::And
            | Operator::Or => bool_type(),
        }
    }

    pub fn get_type(&self) -> Type<Kind> {
        Type::Arrow(
            Rc::new(self.argument_type()),
            Rc::new(Type::Arrow(
                Rc::new(self.argument_type()),
                Rc::new(self.result_type()),
            )),
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<A: std::clone::Clone> {
    /// a let statement with a binding of type A
    Let(Pattern<A>, Expr<A>, A),
    Raw(Expr<A>),
    /// the name of the type, the polymorphic type variables, the possible variants
    TypeDef(String, Vec<Var>, HashSet<Variant<()>>),
}

impl Statement<Type<Kind>> {
    pub fn get_type(&self) -> Type<Kind> {
        match self {
            Statement::TypeDef(..) | Statement::Let(..) => unit_type(),
            Statement::Raw(expr) => expr.get_type(),
        }
    }
}

impl<A: cmp::PartialEq + std::clone::Clone> cmp::PartialEq for Expr<A> {
    fn eq(&self, other: &Expr<A>) -> bool {
        match self {
            Expr::Operator(o1, t1) => {
                if let Expr::Operator(o2, t2) = other {
                    o1 == o2 && t1 == t2
                } else {
                    false
                }
            }
            Expr::Constructor(c1, _) => {
                if let Expr::Constructor(c2, _) = other {
                    c1 == c2
                } else {
                    false
                }
            }
            Expr::Tuple(es1) => {
                if let Expr::Tuple(es2) = other {
                    es1 == es2
                } else {
                    false
                }
            }
            Expr::Function(p1, b1, _) => {
                if let Expr::Function(p2, b2, _) = other {
                    p1 == p2 && b1 == b2
                } else {
                    false
                }
            }
            Expr::Number(i) => {
                if let Expr::Number(j) = other {
                    (i - j).abs() < 0.0001_f64
                } else {
                    false
                }
            }
            Expr::Variable(a, _) => {
                if let Expr::Variable(b, _) = other {
                    a == b
                } else {
                    false
                }
            }
            Expr::Application(l, r, _) => {
                if let Expr::Application(l1, r1, _) = other {
                    l == l1 && r == r1
                } else {
                    false
                }
            }
            Expr::Match(m, l, _) => {
                if let Expr::Match(m1, l1, _) = other {
                    m == m1 && l == l1
                } else {
                    false
                }
            }
            Expr::Record(r) => {
                if let Expr::Record(r1) = other {
                    r == r1
                } else {
                    false
                }
            }
            Expr::Boolean(b) => {
                if let Expr::Boolean(b1) = other {
                    b == b1
                } else {
                    false
                }
            }
            Expr::If(p, e1, e2) => {
                if let Expr::If(p1, e11, e21) = other {
                    p == p1 && e1 == e11 && e2 == e21
                } else {
                    false
                }
            }
            Expr::Block(b) => {
                if let Expr::Block(b1) = other {
                    b == b1
                } else {
                    false
                }
            }
        }
    }
}

impl<A: cmp::Eq + std::clone::Clone> cmp::Eq for Expr<A> {}

fn show_tailing_fn(margin: usize, e: &Expr<Type<Kind>>) -> String {
    match e {
        Expr::Function(pattern, body, _) if matches!(body.as_ref(), Expr::Function(..)) => {
            format!("{} {}", pattern, show_tailing_fn(margin, body))
        }
        Expr::Function(pattern, body, _) => format!(
            "{} -> \n{}{}",
            pattern,
            vec![' '; margin * 2 + 2].iter().collect::<String>(),
            show_expr(margin + 1, body)
        ),
        _ => show_expr(margin, e),
    }
}

fn show_expr(margin: usize, expr: &Expr<Type<Kind>>) -> String {
    let margin_str: String = vec![' '; margin * 2].iter().collect();
    match expr {
        Expr::Application(l, r, _) => format!(
            "{} {}",
            format!("({})", show_expr(margin, l)),
            format!("({})", show_expr(margin, r))
        ),
        Expr::Number(n) => n.to_string(),
        Expr::Boolean(b) => b.to_string(),
        Expr::Variable(var, _) => var.to_string(),
        Expr::Block(ss) => format!(
            "{{\n{}{}}}",
            ss.iter().fold(String::new(), |mut acc, x| {
                acc.push_str(&margin_str);
                acc.push_str("  ");
                acc.push_str(&x.to_string());
                acc.push('\n');
                acc
            }),
            &margin_str
        ),
        Expr::Record(r) => format!(
            "{{\n{}}}",
            r.iter().fold(String::new(), |mut acc, (f, v)| {
                for _ in 0..(margin * 2 + 2) {
                    acc.push(' ');
                }
                acc.push_str(f);
                acc.push_str(": ");
                acc.push_str(&show_expr(margin + 1, v));
                acc.push('\n');
                acc
            })
        ),
        Expr::Constructor(c, _) => c.to_string(),
        Expr::If(p, e1, e2) => format!(
            "if {}\n{}  then {}\n{}  else {}",
            show_expr(margin, p),
            &margin_str,
            show_expr(margin + 1, e1),
            &margin_str,
            show_expr(margin + 1, e2)
        ),
        Expr::Match(m, l, _) => format!(
            "match {} {{\n{}}}",
            m,
            l.iter().fold(String::new(), |mut acc, (p, e)| {
                acc.push_str(&margin_str);
                acc.push_str("  ");
                acc.push_str(&p.to_string());
                acc.push_str(" -> ");
                acc.push_str(&show_expr(margin + 1, e));
                acc.push('\n');
                acc
            })
        ),
        Expr::Operator(o, _) => o.to_string(),
        e => format!("fn {}", show_tailing_fn(margin, e)),
    }
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Operator::Plus => "+",
                Operator::Minus => "-",
                Operator::Times => "*",
                Operator::And => "&",
                Operator::Or => "|",
                Operator::Equals => "==",
                Operator::LessThan => "<",
                Operator::LessThanEqual => "<=",
                Operator::GreaterThan => ">",
                Operator::GreaterThanEqual => ">=",
            }
        )
    }
}

impl fmt::Display for Expr<()> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Function(pattern, body, ()) => write!(f, "(fn {} -> {})", pattern, body),
            Expr::Application(left, right, ()) => write!(f, "({} {})", left, right),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::Operator(op, ()) => write!(f, "{}", op),
            Expr::Variable(var, ()) => write!(f, "{}", var),
            Expr::Record(record) => {
                write!(f, "{{")?;
                for (var, val) in record {
                    write!(f, "{}: {}, ", var, val)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Expr::If(pred, cons, altr) => write!(f, "if {} then {} else {}", pred, cons, altr),
            Expr::Block(block) => {
                write!(f, "{{")?;
                for statement in block {
                    write!(f, "{}; ", statement)?;
                }
                write!(f, "}}")?;
                Ok(())
            }
            Expr::Tuple(tuple) => {
                write!(f, "(")?;
                for val in tuple {
                    write!(f, "{}, ", val)?;
                }
                write!(f, ")")?;
                Ok(())
            }
            Expr::Constructor(cons, _) => write!(f, "{}", cons),
            Expr::Match(matchand, arms, _) => {
                writeln!(f, "match {} {{", matchand)?;
                for (pattern, expr) in arms {
                    writeln!(f, "{} -> {},", pattern, expr)?;
                }
                write!(f, "}}")
            }
        }
    }
}

impl fmt::Display for Statement<Type<Kind>> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(pattern, body, _) => write!(f, "let {} = {}", pattern, body),
            Statement::Raw(expr) => write!(f, "{}", expr),
            Statement::TypeDef(tn, tvs, vs) => {
                write!(f, "type {} ", tn)?;
                for var in tvs {
                    write!(f, "{} ", var)?;
                }
                write!(f, "= {:?}", vs)
            }
        }
    }
}

impl fmt::Display for Statement<()> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Let(pattern, body, _) => write!(f, "let {} = {}", pattern, body),
            Statement::Raw(expr) => write!(f, "{}", expr),
            Statement::TypeDef(tn, tvs, vs) => {
                write!(f, "type {} ", tn)?;
                for var in tvs {
                    write!(f, "{} ", var)?;
                }
                write!(f, "= {:?}", vs)
            }
        }
    }
}

impl fmt::Display for Expr<Type<Kind>> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", show_expr(0, self))
    }
}

impl Expr<()> {
    #[allow(dead_code)]
    fn rand(from_num: fn(usize) -> String) -> Self {
        match rand::thread_rng().gen::<u8>() % 4 {
            0 => Expr::Application(
                Box::new(Expr::rand(from_num)),
                Box::new(Expr::rand(from_num)),
                (),
            ),
            1 => Expr::Function(Pattern::rand(from_num), Box::new(Expr::rand(from_num)), ()),
            2 => Expr::Number(rand::thread_rng().gen()),
            3 => Expr::Variable(from_num(rand::thread_rng().gen()), ()),
            _ => unreachable!(),
        }
    }
}

impl<A: std::clone::Clone + std::fmt::Debug> fmt::Display for Pattern<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Variable(v, _) => write!(f, "{}", v),
            Pattern::Record(r) => write!(f, "{:?}", r),
            Pattern::Tuple(t) => write!(f, "{:?}", t),
            Pattern::Construction(c, ps, _) => write!(f, "{} {:?}", c, ps),
        }
    }
}

impl Pattern<Type<Kind>> {
    pub fn bound_vars<'a>(&'a self) -> HashSet<(&'a str, Type<Kind>)> {
        match self {
            Pattern::Variable(v, var_type) => HashSet::unit((v, var_type.clone())),
            Pattern::Record(r) => r.values().flat_map(Pattern::bound_vars).collect(),
            Pattern::Construction(_, t, _) | Pattern::Tuple(t) => {
                t.iter().flat_map(Pattern::bound_vars).collect()
            }
        }
    }
}

impl<K: Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash> Pattern<Type<K>> {
    pub fn get_type(&self) -> Type<K> {
        match self {
            Pattern::Construction(_, _, t) | Pattern::Variable(_, t) => t.clone(),
            Pattern::Record(record) => Type::Record(
                record
                    .iter()
                    .map(|(var, val)| (var.clone(), val.get_type()))
                    .collect(),
            ),
            Pattern::Tuple(tuple) => Type::Tuple(tuple.iter().map(Pattern::get_type).collect()),
        }
    }
}

impl Pattern<()> {
    fn rand(from_num: fn(usize) -> String) -> Self {
        Pattern::Variable(from_num(thread_rng().gen()), ())
    }
}
