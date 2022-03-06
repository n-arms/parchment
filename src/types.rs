#![allow(dead_code)]

use im::hashmap::HashMap;
use im::hashset::HashSet;
use std::cell::Cell;
use std::fmt;
use super::gen::GenState;
use super::expr::{Statement, Expr};

pub type TypeVar = String;
pub type TypeEnv = HashMap<String, Scheme>;

#[derive(Debug, PartialEq, Eq)]
pub struct VarSet<V> {
    index: Cell<usize>,
    f: fn(usize) -> V,
}

impl<V> VarSet<V> {
    pub fn new(f: fn(usize) -> V) -> Self {
        VarSet {
            index: Cell::new(0),
            f,
        }
    }

    pub fn fresh(&self) -> V {
        let var = self.index.take();
        self.index.set(var + 1);
        (self.f)(var)
    }
}

impl Default for VarSet<String> {
    fn default() -> Self {
        VarSet::new(|v| v.to_string())
    }
}

impl Default for VarSet<usize> {
    fn default() -> Self {
        VarSet::new(|v| v)
    }
}

pub type TypeVarSet = VarSet<String>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Variable(TypeVar),
    Arrow(Box<Type>, Box<Type>),
    Constructor(String),
    Record(HashMap<String, Type>),
    Tuple(Vec<Type>),
}

pub fn num_type() -> Type {
    Type::Constructor(String::from("Num"))
}

pub fn bool_type() -> Type {
    Type::Constructor(String::from("Bool"))
}

pub fn unit_type() -> Type {
    Type::Constructor(String::from("Unit"))
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub name: String,
    pub fields: Vec<Type>,
}

impl Type {
    /* create a scheme from a type
     * the polymorphic type vars are all the free vars in the type that are not free in Infer
     */
    pub fn generalize(&self, free: HashSet<String>) -> Scheme {
        Scheme(
            self.free_type_vars().relative_complement(free),
            self.clone(),
        )
    }

    pub fn contains(&self, v: &TypeVar) -> bool {
        match self {
            Type::Arrow(l, r) => l.contains(v) || r.contains(v),
            Type::Variable(v1) => v1 == v,
            Type::Constructor(_) => false,
            Type::Record(r) => r.values().any(|t| t.contains(v)),
            Type::Tuple(t) => t.iter().any(|t| t.contains(v)),
        }
    }
}

pub trait Apply {
    fn apply(&self, s: Substitution) -> Self;
}

impl Apply for Type {
    fn apply(&self, s: Substitution) -> Type {
        match self {
            Type::Variable(v) => s.get(v).cloned().unwrap_or_else(|| self.clone()),
            Type::Arrow(l, r) => Type::Arrow(Box::new(l.apply(s.clone())), Box::new(r.apply(s))),
            Type::Constructor(_) => self.clone(),
            Type::Record(r) => Type::Record(
                r.iter()
                    .map(|(k, v)| (k.clone(), v.apply(s.clone())))
                    .collect(),
            ),
            Type::Tuple(t) => Type::Tuple(t.iter().map(|t| t.apply(s.clone())).collect()),
        }
    }
}

impl Apply for Expr<Type> {
    fn apply(&self, s: Substitution) -> Self {
        match self {
            Expr::Function(pattern, body, pattern_type) => Expr::Function(pattern.clone(), Box::new(body.apply(s.clone())), pattern_type.apply(s)),
            Expr::Application(left, right, app_type) => Expr::Application(Box::new(left.apply(s.clone())), Box::new(right.apply(s.clone())), app_type.apply(s)),
            Expr::Number(_) |
            Expr::Boolean(_) => self.clone(),
            Expr::Operator(op, op_type) => Expr::Operator(*op, op_type.apply(s)),
            Expr::Variable(var, var_type) => Expr::Variable(var.clone(), var_type.apply(s)),
            Expr::Record(record) => Expr::Record(record.iter().map(|(var, val)| (var.clone(), val.apply(s.clone()))).collect()),
            Expr::If(pred, con, alt) => Expr::If(Box::new(pred.apply(s.clone())), Box::new(con.apply(s.clone())), Box::new(alt.apply(s))),
            Expr::Block(block) => Expr::Block(block.iter().map(|statement| statement.apply(s.clone())).collect()),
            Expr::Tuple(tuple) => Expr::Tuple(tuple.iter().map(|expr| expr.apply(s.clone())).collect()),
            Expr::Constructor(cons, cons_type) => Expr::Constructor(cons.clone(), cons_type.apply(s)),
            Expr::Match(_, _) => todo!(),
        }
    }
}

impl Apply for Statement<Type> {
    fn apply(&self, s: Substitution) -> Self {
        match self {
            Statement::Let(pattern, body, pattern_type) => Statement::Let(pattern.clone(), body.apply(s.clone()), pattern_type.apply(s)),
            Statement::Raw(expr) => Statement::Raw(expr.apply(s)),
            Statement::TypeDef(_, _) => self.clone(),
        }
    }
}

pub trait Free {
    fn free_type_vars(&self) -> HashSet<TypeVar>;
}

impl Free for Type {
    fn free_type_vars(&self) -> HashSet<TypeVar> {
        match self {
            Type::Variable(v) => HashSet::unit(v.clone()),
            Type::Arrow(e1, e2) => e1.free_type_vars().union(e2.free_type_vars()),
            Type::Record(r) => r.values().flat_map(Free::free_type_vars).collect(),
            Type::Tuple(t) => t.iter().flat_map(Free::free_type_vars).collect(),
            Type::Constructor(_) => HashSet::new()
        }
    }
}

impl Free for Scheme {
    fn free_type_vars(&self) -> HashSet<TypeVar> {
        self.1.free_type_vars().relative_complement(self.0.clone())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Variable(v) => write!(f, "{}", v),
            Type::Arrow(l, r) => write!(f, "({} -> {})", *l, *r),
            Type::Constructor(c) => c.fmt(f),
            Type::Record(r) => {
                write!(f, "{{")?;
                for (i, (val, var)) in r.iter().enumerate() {
                    write!(f, "{}: {}", val, var)?;
                    if i != r.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, "}}")?;
                Ok(())
            },
            Type::Tuple(ts) => {
                write!(f, "(")?;
                for (i, t) in ts.iter().enumerate() {
                    write!(f, "{}", t)?;
                    if i != ts.len() - 1 {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")?;
                Ok(())
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme(pub HashSet<String>, pub Type);

impl Scheme {
    pub fn instantiate(&self, free: &GenState) -> Type {
        let new_tvs = self
            .0
            .iter()
            .map(|old| (old.clone(), Type::Variable(free.fresh())))
            .collect();
        self.1.apply(new_tvs)
    }
}

pub type Substitution = HashMap<TypeVar, Type>;

pub fn combine(s1: Substitution, s2: Substitution) -> Substitution {
    let mut s3: Substitution = s2
        .into_iter()
        .map(|(u, t)| (u, t.apply(s1.clone())))
        .collect();
    s3.extend(s1);

    s3
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "forall {:?}. {}", self.0, self.1)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn free_tv_mono() {
        assert_eq!(
            num_type().free_type_vars(),
            HashSet::new()
        );
        assert_eq!(
            Type::Variable(String::from("0")).free_type_vars(),
            HashSet::unit(String::from("0"))
        );
        assert_eq!(
            Type::Arrow(
                Box::new(Type::Variable(String::from("0"))),
                Box::new(Type::Variable(String::from("1")))
            )
            .free_type_vars(),
            HashSet::unit(String::from("0")).update(String::from("1"))
        );
    }
    #[test]
    fn free_tv_poly() {
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("0")),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("0"))),
                    Box::new(Type::Variable(String::from("1")))
                )
            )
            .free_type_vars(),
            HashSet::unit(String::from("1"))
        );
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("0")),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("0"))),
                    Box::new(Type::Variable(String::from("1")))
                )
            )
            .free_type_vars(),
            HashSet::unit(String::from("1"))
        );
    }
}
