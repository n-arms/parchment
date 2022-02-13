#![allow(dead_code)]

use im::hashmap::HashMap;
use im::hashset::HashSet;
use std::cell::Cell;
use std::fmt;
use std::marker::PhantomData;

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
    Constructor(Constructor),
    Record(HashMap<String, Type>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constructor {
    Boolean,
    Number,
    Unit,
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
            Type::Constructor(_) => HashSet::new(),
            Type::Arrow(e1, e2) => e1.free_type_vars().union(e2.free_type_vars()),
            Type::Record(r) => r.values().flat_map(Free::free_type_vars).collect(),
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
            Type::Constructor(c) => write!(
                f,
                "{}",
                match c {
                    Constructor::Boolean => "Bool",
                    Constructor::Number => "Num",
                    Constructor::Unit => "Unit",
                }
            ),
            Type::Record(r) => write!(f, "{:?}", r),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme(pub HashSet<String>, pub Type);

impl Scheme {
    pub fn instantiate(&self, free: &TypeVarSet) -> Type {
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
            Type::Constructor(Constructor::Number).free_type_vars(),
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
