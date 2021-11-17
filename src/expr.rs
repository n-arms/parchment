use std::cmp;
use std::fmt;
use rand::prelude::*;
use im::hashmap::HashMap;
use im::hashset::HashSet;
use super::types::{Type, Scheme, Env};
use super::infer::TypeError;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Pattern {
    Variable(String),
    Record(HashMap<String, Pattern>)
}

impl Pattern {
    pub fn bound_vars(&self) -> HashSet<String> {
        match self {
            Pattern::Variable(s) => HashSet::unit(s.clone()),
            Pattern::Record(r) => 
                r.values()
                    .flat_map(|p| {
                        p.bound_vars()
                    }).collect()
        }
    }
    pub fn into_type(&self, env: &HashMap<String, Type>) -> Type {
        match self {
            Pattern::Variable(s) => env.get(s).unwrap().clone(),
            Pattern::Record(r) =>
                super::types::Type::Record(
                    r.iter()
                        .map(|(k, v)| (k.clone(), v.into_type(env)))
                        .collect())
        }
    }
    pub fn into_env(&self, env: &Env, target: &Type) -> Result<Env, TypeError> {
        match self {
            Pattern::Variable(s) => Ok(Env(HashMap::unit(s.clone(), target.generalize(env)))),
            Pattern::Record(r1) => match target {
                Type::Record(r2) => {
                    r1.iter()
                        .map(|(f, p)| {
                            let nt = r2.get(f).ok_or(TypeError::MissingRecordField(f.clone()))?;
                            p.into_env(env, nt)
                        }) // list of Result<Env, TypeError>
                        .fold(Ok(HashMap::new()), |mut acc : Result<_, TypeError>, x| {
                            let x = x?;
                            acc.as_mut()
                                .map(|e| e.extend(x.0))
                                .map_err(|e| e.clone())?;
                            acc
                        })
                        .map(|e| Env(e))
                },
                _ => Err(TypeError::IsntRecord(target.clone()))
            }
        }
    }
    pub fn into_env_match(&self, env: &Env, target: &Type) -> Result<Env, TypeError> {
        match self {
            Pattern::Variable(s) => Ok(Env(HashMap::unit(s.clone(), Scheme(HashSet::new(), target.clone())))),
            Pattern::Record(r1) => match target {
                Type::Record(r2) => {
                    r1.iter()
                        .map(|(f, p)| {
                            let nt = r2.get(f).ok_or(TypeError::MissingRecordField(f.clone()))?;
                            p.into_env(env, nt)
                        }) // list of Result<Env, TypeError>
                        .fold(Ok(HashMap::new()), |mut acc : Result<_, TypeError>, x| {
                            let x = x?;
                            acc.as_mut()
                                .map(|e| e.extend(x.0))
                                .map_err(|e| e.clone())?;
                            acc
                        })
                        .map(|e| Env(e))
                },
                _ => Err(TypeError::IsntRecord(target.clone()))
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Function(Pattern, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Number(f64),
    Boolean(bool),
    Variable(String),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Record(HashMap<String, Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Expr)>),
}

impl cmp::PartialEq for Expr {
    fn eq(&self, other: &Expr) -> bool {
        match self {
            Expr::Function(p, e) => if let Expr::Function(p1, e1) = other {
                p == p1 && e == e1
            } else {false},
            Expr::Number(i) => if let Expr::Number(j) = other {
                (i - j).abs() < 0.0001
            } else {false},
            Expr::Variable(a) => if let Expr::Variable(b) = other {
                a == b
            } else {false},
            Expr::Application(l, r) => if let Expr::Application(l1, r1) = other {
                l == l1 && r == r1
            } else {false},
            Expr::Match(m, l) => if let Expr::Match(m1, l1) = other {
                m == m1 && l == l1
            } else {false},
            Expr::Let(p, v, e) => if let Expr::Let(p1, v1, e1) = other {
                p == p1 && v == v1 && e == e1
            } else {false},
            Expr::Record(r) => if let Expr::Record(r1) = other {
                r == r1
            } else {false},
            Expr::Boolean(b) => if let Expr::Boolean(b1) = other {
                b == b1
            } else {false},
            Expr::If(p, e1, e2) => if let Expr::If(p1, e11, e21) = other {
                p == p1 && e1 == e11 && e2 == e21
            } else {false},
        }
    }
}

impl cmp::Eq for Expr {}

fn show_tailing_fn(margin: usize, e: &Expr) -> String {
    match e {
        Expr::Function(p, b) if matches!(b.as_ref(), Expr::Function(..)) => format!("{} {}", p, show_tailing_fn(margin, b)),
        Expr::Function(p, b) => format!("{} -> \n{}{}", p, vec![' '; margin * 2 + 2].iter().collect::<String>(), show_expr(margin + 1, b)),
        _ => show_expr(margin, e)
    }
}

fn show_expr(margin: usize, e: &Expr) -> String {
    let margin_str : String = vec![' '; margin * 2].iter().collect();
    match e {
        Expr::Application(l, r) => 
            format!("{} {}",
                    if matches!(l.as_ref(), Expr::Function(..)) {
                        format!("({})", show_expr(margin, l))
                    } else {
                        show_expr(margin, l)
                    },
                    if matches!(r.as_ref(), Expr::Application(..)) {
                        format!("({})", show_expr(margin, r))
                    } else {
                        show_expr(margin, r)
                    }),
        Expr::Number(n) => n.to_string(),
        Expr::Boolean(b) => b.to_string(),
        Expr::Variable(v) => v.clone(),
        Expr::Let(p, v, e) => format!("let {} = {} in\n{}{}", p, show_expr(margin, v), margin_str + "  ", show_expr(margin + 1, e)),
        Expr::Record(r) => format!("{{\n{}}}", r.iter().fold(String::new(), |mut acc, (f, v)| {
            for _ in 0..(margin * 2 + 2) {
                acc.push(' ');
            }
            acc.push_str(f);
            acc.push_str(": ");
            acc.push_str(&show_expr(margin + 1, v));
            acc.push('\n');
            acc
        })),
        Expr::If(p, e1, e2) => format!("if {}\n{}  then {}\n{}  else {}", show_expr(margin, p), &margin_str, show_expr(margin + 1, e1), &margin_str, show_expr(margin + 1, e2)),
        Expr::Match(m, l) => format!("match {} with\n{}end", m, l.iter().fold(String::new(), |mut acc, (p, e)| {
            acc.push_str(&margin_str);
            acc.push_str("  ");
            acc.push_str(&p.to_string());
            acc.push_str(" -> ");
            acc.push_str(&show_expr(margin + 1, e));
            acc.push('\n');
            acc
        })),
        e => format!("fn {}", show_tailing_fn(margin, e)),
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", show_expr(0, self))
        /*
        match self {
            Expr::Application(l, r) => write!(f, "({} {})", *l, *r),
            Expr::Function(p, b) => write!(f, "(fn {} -> {})", *p, *b),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Boolean(b) => write!(f, "{}", b),
            Expr::Variable(v) => write!(f, "{}", v),
            Expr::Let(p, v, e) => write!(f, "let {} = {} in {}", p, v, e),
            Expr::Record(r) => write!(f, "{:?}", r),
            Expr::If(p, e1, e2) => write!(f, "if {} then {} else {}", p, e1.as_ref(), e2.as_ref()),
            Expr::Match(m, l) => write!(f, "match {} with \n{}end", m, l.iter().fold(String::new(), |mut acc, (p, e)| {
                acc.push_str("  ");
                acc.push_str(&p.to_string());
                acc.push_str(" -> ");
                acc.push_str(&e.to_string());
                acc.push_str(",\n");
                acc
            })),
        }
        */
    }
}

impl Expr {
    #[allow(dead_code)]
    fn rand() -> Self {
        match rand::thread_rng().gen::<u8>() >> 6 {
            0 => Expr::Application(Box::new(Expr::rand()), Box::new(Expr::rand())),
            1 => Expr::Function(Pattern::rand(), Box::new(Expr::rand())),
            2 => Expr::Number(rand::thread_rng().gen()),
            3 => Expr::Variable(rand::thread_rng().gen::<usize>().to_string()),
            _ => panic!()
        }
    }
}

impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Pattern::Variable(v) => write!(f, "{}", v),
            Pattern::Record(r) => write!(f, "{:?}", r),
        }
    }
}

impl Pattern {
    fn rand() -> Self {
        Pattern::Variable(thread_rng().gen::<usize>().to_string())
    }
}
