use super::types::{Type, TypeVar, TypeVarSet};
use im::{HashMap, HashSet};
use rand::prelude::*;
use std::cmp;
use std::fmt;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern<V: Clone + fmt::Debug + cmp::PartialEq + cmp::Eq + std::hash::Hash> {
    Variable(V),
    Record(HashMap<V, Pattern<V>>),
}

impl Pattern<String> {
    pub fn type_pattern(&self, t: &TypeVarSet) -> (HashMap<String, TypeVar>, Type) {
        match self {
            Pattern::Variable(v) => {
                let b = t.fresh();
                (HashMap::unit(v.clone(), b.clone()), Type::Variable(b))
            }
            Pattern::Record(r) => {
                let (b, t) = r
                    .iter()
                    .map(|(var, val)| (var.clone(), val.type_pattern(t)))
                    .fold(
                        (HashMap::new(), HashMap::new()),
                        |(mut bs, mut ts), (var, (b, t))| {
                            bs.extend(b);
                            ts.insert(var, t);
                            (bs, ts)
                        },
                    );
                (b, Type::Record(t))
            }
        }
    }

    pub fn bound_vars(&self) -> HashSet<String> {
        match self {
            Pattern::Variable(v) => HashSet::unit(v.clone()),
            Pattern::Record(r) => r.values().flat_map(Pattern::bound_vars).collect(),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> {
    Function(Pattern<V>, Box<Expr<V>>),
    Application(Box<Expr<V>>, Box<Expr<V>>),
    Number(f64),
    Boolean(bool),
    Variable(V),
    Record(HashMap<String, Expr<V>>),
    If(Box<Expr<V>>, Box<Expr<V>>, Box<Expr<V>>),
    Match(Box<Expr<V>>, Vec<(Pattern<V>, Expr<V>)>),
    Block(Vec<Statement<V>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Statement<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> {
    Let(Pattern<V>, Expr<V>),
    Raw(Expr<V>),
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> cmp::PartialEq for Expr<V> {
    fn eq(&self, other: &Expr<V>) -> bool {
        match self {
            Expr::Function(p, e) => {
                if let Expr::Function(p1, e1) = other {
                    p == p1 && e == e1
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
            Expr::Variable(a) => {
                if let Expr::Variable(b) = other {
                    a == b
                } else {
                    false
                }
            }
            Expr::Application(l, r) => {
                if let Expr::Application(l1, r1) = other {
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

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq + fmt::Display> fmt::Display
    for Statement<V>
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::Raw(r) => r.fmt(f),
            Statement::Let(p, e) => write!(f, "let {} = {}", p, e),
        }
    }
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> cmp::Eq for Expr<V> {}

fn show_tailing_fn<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq + fmt::Display>(
    margin: usize,
    e: &Expr<V>,
) -> String {
    match e {
        Expr::Function(p, b) if matches!(b.as_ref(), Expr::Function(..)) => {
            format!("{} {}", p, show_tailing_fn(margin, b))
        }
        Expr::Function(p, b) => format!(
            "{} -> \n{}{}",
            p,
            vec![' '; margin * 2 + 2].iter().collect::<String>(),
            show_expr(margin + 1, b)
        ),
        _ => show_expr(margin, e),
    }
}

fn show_expr<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq + ToString + fmt::Display>(
    margin: usize,
    e: &Expr<V>,
) -> String {
    let margin_str: String = vec![' '; margin * 2].iter().collect();
    match e {
        Expr::Application(l, r) => format!(
            "{} {}",
            if matches!(l.as_ref(), Expr::Function(..)) {
                format!("({})", show_expr(margin, l))
            } else {
                show_expr(margin, l)
            },
            if matches!(r.as_ref(), Expr::Application(..)) {
                format!("({})", show_expr(margin, r))
            } else {
                show_expr(margin, r)
            }
        ),
        Expr::Number(n) => n.to_string(),
        Expr::Boolean(b) => b.to_string(),
        Expr::Variable(v) => v.to_string(),
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
        e => format!("fn {}", show_tailing_fn(margin, e)),
    }
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq + fmt::Display> fmt::Display for Expr<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", show_expr(0, self))
    }
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> Expr<V> {
    #[allow(dead_code)]
    fn rand(from_num: fn(usize) -> V) -> Self {
        match rand::thread_rng().gen::<u8>() >> 6 {
            0 => Expr::Application(
                Box::new(Expr::rand(from_num)),
                Box::new(Expr::rand(from_num)),
            ),
            1 => Expr::Function(Pattern::rand(from_num), Box::new(Expr::rand(from_num))),
            2 => Expr::Number(rand::thread_rng().gen()),
            3 => Expr::Variable(from_num(rand::thread_rng().gen())),
            _ => panic!(),
        }
    }
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq + fmt::Display> fmt::Display for Pattern<V> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Variable(v) => write!(f, "{}", v),
            Pattern::Record(r) => write!(f, "{:?}", r),
        }
    }
}

impl<V: Clone + fmt::Debug + std::hash::Hash + cmp::Eq> Pattern<V> {
    fn rand(from_num: fn(usize) -> V) -> Self {
        Pattern::Variable(from_num(thread_rng().gen()))
    }
}
