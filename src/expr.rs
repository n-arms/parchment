use std::cmp;
use std::fmt;
use rand::prelude::*;
use im::hashmap::HashMap;
use im::hashset::HashSet;

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
    pub fn into_type(&self, env: &HashMap<String, super::types::Type>) -> super::types::Type {
        match self {
            Pattern::Variable(s) => env.get(s).unwrap().clone(),
            Pattern::Record(r) =>
                super::types::Type::Record(
                    r.iter()
                        .map(|(k, v)| (k.clone(), v.into_type(env)))
                        .collect())
        }
    }
}

#[derive(Clone, Debug)]
pub enum Expr {
    Function(Pattern, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Number(f64),
    Variable(String),
    Let(Pattern, Box<Expr>, Box<Expr>),
    Record(HashMap<String, Expr>),
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
            Expr::Let(p, v, e) => if let Expr::Let(p1, v1, e1) = other {
                p == p1 && v == v1 && e == e1
            } else {false},
            Expr::Record(r) => if let Expr::Record(r1) = other {
                r == r1
            } else {false},
        }
    }
}

impl cmp::Eq for Expr {}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Expr::Application(l, r) => write!(f, "({} {})", *l, *r),
            Expr::Function(p, b) => write!(f, "(fn {} -> {})", *p, *b),
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Variable(v) => write!(f, "{}", v),
            Expr::Let(p, v, e) => write!(f, "let {} = {} in {}", p, v, e),
            Expr::Record(r) => write!(f, "{:?}", r)
        }
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
