use super::types::{Type, TypeVar, TypeVarSet, Variant, num_type, bool_type, unit_type};
use super::gen::{TypeError, GenState};
use im::{HashMap, HashSet};
use rand::prelude::*;
use std::cmp;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    Variable(String),
    Record(HashMap<String, Pattern>),
    Tuple(Vec<Pattern>),
    Construction(String, Vec<Pattern>),
}

impl Pattern {
    /// create a mapping from type vars to types, and return a type representing the pattern
    pub fn type_pattern(&self, st: &GenState) -> Result<(HashMap<String, TypeVar>, Type), TypeError> {
        match self {
            Pattern::Variable(v) => {
                let b = st.fresh();
                Ok((HashMap::unit(v.clone(), b.clone()), Type::Variable(b)))
            }
            Pattern::Record(r) => {
                let mut bindings = HashMap::new();
                let mut record = HashMap::new();
                for (var, val) in r {
                    let (b, t) = val.type_pattern(st)?;
                    bindings.extend(b);
                    record.insert(var.clone(), t);
                }
                Ok((bindings, Type::Record(record)))
            }
            Pattern::Tuple(ps) => {
                let mut bindings = HashMap::new();
                let mut terms = Vec::new();
                for p in ps {
                    let (b1, p1) = p.type_pattern(st)?;
                    bindings.extend(b1);
                    terms.push(p1);
                }
                Ok((bindings, Type::Tuple(terms)))
            }
            Pattern::Construction(name, ps) => {
                let mut bindings = HashMap::new();
                let (name, Variant {fields, ..}) = st.lookup_variant(name).ok_or_else(|| TypeError::UnknownVariant(name.clone()))?;
                for p in ps {
                    let (b1, p1) = p.type_pattern(st)?;
                    bindings.extend(b1);
                }
                Ok((bindings, Type::Constructor(name)))
            }
        }
    }

    pub fn bound_vars(&self) -> HashSet<String> {
        match self {
            Pattern::Variable(v) => HashSet::unit(v.clone()),
            Pattern::Record(r) => r.values().flat_map(Pattern::bound_vars).collect(),
            Pattern::Construction(_, t) | Pattern::Tuple(t) => t.iter().flat_map(Pattern::bound_vars).collect(),
            
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<A> {
    /// a function with a domain of type A
    Function(Pattern, Box<Expr<A>>, A),
    /// an application that will return type A
    Application(Box<Expr<A>>, Box<Expr<A>>, A),
    Number(f64),
    Boolean(bool),
    Operator(Operator, A),
    Variable(String, A),
    Record(HashMap<String, Expr<A>>),
    If(Box<Expr<A>>, Box<Expr<A>>, Box<Expr<A>>),
    Match(Box<Expr<A>>, Vec<(Pattern, Expr<A>)>),
    Block(Vec<Statement<A>>),
    Tuple(Vec<Expr<A>>),
    Constructor(String, A),
}

impl Expr<Type> {
    pub fn get_type(&self) -> Type {
        match self {
            Expr::Function(_, body, pattern_type) => Type::Arrow(Box::new(pattern_type.clone()), Box::new(body.get_type())),
            Expr::Application(_, _, app_type) => app_type.clone(),
            Expr::Number(_) => num_type(),
            Expr::Boolean(_) => bool_type(),
            Expr::Record(record) => Type::Record(record.iter().map(|(val, var)| (val.clone(), var.get_type())).collect()),
            Expr::If(_, expr, _) => expr.get_type(),
            Expr::Match(_, _) => todo!(),
            Expr::Block(block) => block.last().map(Statement::get_type).unwrap_or_else(|| unit_type()),
            Expr::Tuple(tuple) => Type::Tuple(tuple.iter().map(|val| val.get_type()).collect()),
            Expr::Operator(_, t) | Expr::Constructor(_, t) | Expr::Variable(_, t) => t.clone(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Copy)]
pub enum Operator {
    Plus,
    Minus,
    Times,
    Equals,
    LessThan,
    LessThanEqual,
    GreaterThan,
    GreaterThanEqual,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<A> {
    /// a let statement with a binding of type A
    Let(Pattern, Expr<A>, A),
    Raw(Expr<A>),
    TypeDef(String, HashSet<Variant>)
}

impl Statement<Type> {
    pub fn get_type(&self) -> Type {
        match self {
            Statement::TypeDef(..) | Statement::Let(..) => unit_type(),
            Statement::Raw(expr) => expr.get_type(),
        }
    }
}

impl<A: cmp::PartialEq> cmp::PartialEq for Expr<A> {
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
                    (i - j).abs() < 0.0001
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
            Expr::Match(m, l) => {
                if let Expr::Match(m1, l1) = other {
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

impl<A: fmt::Display> fmt::Display
    for Statement<A>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Raw(r) => r.fmt(f),
            Statement::Let(pattern, body, _) => write!(f, "let {} = {}", pattern, body),
            Statement::TypeDef(tn, vs) => write!(f, "type {} = {:?}", tn, vs),
        }
    }
}

impl<A: cmp::Eq> cmp::Eq for Expr<A> {}

fn show_tailing_fn<A: fmt::Display>(
    margin: usize,
    e: &Expr<A>,
) -> String {
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

fn show_expr<A: fmt::Display>(
    margin: usize,
    e: &Expr<A>,
) -> String {
    let margin_str: String = vec![' '; margin * 2].iter().collect();
    match e {
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
        Expr::Match(m, l) => format!(
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
        Expr::Operator(o, _) => String::from(match o {
            Operator::Plus => "+",
            Operator::Minus => "-",
            Operator::Times => "*",
            Operator::Equals => "==",
            Operator::LessThan => "<",
            Operator::LessThanEqual => "<=",
            Operator::GreaterThan => ">",
            Operator::GreaterThanEqual => ">=",
        }),
        e => format!("fn {}", show_tailing_fn(margin, e)),
    }
}

impl<A: fmt::Display> fmt::Display for Expr<A> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", show_expr(0, self))
    }
}

impl Expr<()> {
    #[allow(dead_code)]
    fn rand(from_num: fn(usize) -> String) -> Self {
        match rand::thread_rng().gen::<u8>() >> 6 {
            0 => Expr::Application(
                Box::new(Expr::rand(from_num)),
                Box::new(Expr::rand(from_num)),
                ()
            ),
            1 => Expr::Function(Pattern::rand(from_num), Box::new(Expr::rand(from_num)), ()),
            2 => Expr::Number(rand::thread_rng().gen()),
            3 => Expr::Variable(from_num(rand::thread_rng().gen()), ()),
            _ => panic!(),
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Variable(v) => write!(f, "{}", v),
            Pattern::Record(r) => write!(f, "{:?}", r),
            Pattern::Tuple(t) => write!(f, "{:?}", t),
            Pattern::Construction(c, ps) => write!(f, "{} {:?}", c, ps)
        }
    }
}

impl Pattern {
    fn rand(from_num: fn(usize) -> String) -> Self {
        Pattern::Variable(from_num(thread_rng().gen()))
    }
}
