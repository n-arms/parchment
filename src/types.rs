#![allow(dead_code)]

use im::hashmap::HashMap;
use im::hashset::HashSet;
use std::cell::Cell;
use std::fmt;

pub type TypeVar = String;
pub type TypeEnv = HashMap<String, Scheme>;

#[derive(Debug, PartialEq, Eq)]
pub struct TypeVarSet {
    index: Cell<usize>,
}

impl TypeVarSet {
    pub fn new() -> Self {
        TypeVarSet {
            index: Cell::new(0),
        }
    }

    pub fn fresh(&self) -> TypeVar {
        let var = self.index.take();
        self.index.set(var + 1);
        var.to_string()
    }
}

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
            Type::Arrow(l, r) => l.contains(&v) || r.contains(&v),
            Type::Variable(v1) => v1 == v,
            Type::Constructor(_) => false,
            Type::Record(_) => todo!(),
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
            Type::Arrow(l, r) => l.free_type_vars().union(r.free_type_vars()),
            Type::Record(_) => todo!(),
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
        apply(&self.1, &new_tvs)
    }
}

pub type Substitution = HashMap<TypeVar, Type>;

pub fn apply(t: &Type, s: &Substitution) -> Type {
    match t {
        Type::Variable(v) => s.get(v).cloned().unwrap_or_else(|| t.clone()),
        Type::Arrow(l, r) => Type::Arrow(Box::new(apply(l, s)), Box::new(apply(r, s))),
        Type::Constructor(_) => t.clone(),
        Type::Record(_) => todo!(),
    }
}

pub fn combine(s1: Substitution, s2: Substitution) -> Substitution {
    let mut s3: Substitution = s2.into_iter().map(|(u, t)| (u, apply(&t, &s1))).collect();
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
        assert_eq!(Type::Variable(String::from("0")).free_type_vars(), HashSet::unit(String::from("0")));
        assert_eq!(
            Type::Arrow(Box::new(Type::Variable(String::from("0"))), Box::new(Type::Variable(String::from("1")))).free_type_vars(),
            HashSet::unit(String::from("0")).update(String::from("1"))
        );
    }
    #[test]
    fn free_tv_poly() {
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("0")),
                Type::Arrow(Box::new(Type::Variable(String::from("0"))), Box::new(Type::Variable(String::from("1"))))
            )
            .free_type_vars(),
            HashSet::unit(String::from("1"))
        );
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("0")),
                Type::Arrow(Box::new(Type::Variable(String::from("0"))), Box::new(Type::Variable(String::from("1"))))
            )
            .free_type_vars(),
            HashSet::unit(String::from("1"))
        );
    }
    /*
    #[test]
    fn sub_mono() {
        let s = Infer::default();
        s.add_sub(0, Type::Variable(0));
        assert_eq!(Type::Number.apply(&s), Type::Number);
        assert_eq!(
            Type::Variable(String::from("a")).apply(&s),
            Type::Variable(String::from("0")));
        assert_eq!(
            Type::Variable(String::from("b")).apply(&s),
            Type::Variable(String::from("b")));
        assert_eq!( // [a/0] (a -> b) -> a = (0 -> b) -> 0
            Type::Arrow(
                Box::new(Type::Arrow(
                        Box::new(Type::Variable(String::from("a"))),
                        Box::new(Type::Variable(String::from("b"))))),
                Box::new(Type::Variable(String::from("a")))).apply(&s),
            Type::Arrow(
                Box::new(Type::Arrow(
                        Box::new(Type::Variable(String::from("0"))),
                        Box::new(Type::Variable(String::from("b"))))),
                Box::new(Type::Variable(String::from("0")))));
        assert_eq!(
            Type::Record(im::hashmap!{
                String::from("left") => Type::Variable(String::from("a")),
                String::from("right") => Type::Variable(String::from("b"))
            }).apply(&s),
            Type::Record(im::hashmap!{
                String::from("left") => Type::Variable(String::from("0")),
                String::from("right") => Type::Variable(String::from("b"))
            }));
    }
    #[test]
    fn sub_poly() {
        let s = Infer::default();
        s.add_sub(String::from("a"), Type::Variable(String::from("0")));
        let t = Type::Arrow(
            Box::new(Type::Variable(String::from("a"))),
            Box::new(Type::Variable(String::from("b"))));
        assert_eq!(
            Scheme(HashSet::new(), t.clone()).apply(&s),
            Scheme(
                HashSet::new(),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("0"))),
                    Box::new(Type::Variable(String::from("b"))))));
        assert_eq!(
            Scheme(HashSet::unit(String::from("b")), t.clone()).apply(&s),
            Scheme(
                HashSet::unit(String::from("b")),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("0"))),
                    Box::new(Type::Variable(String::from("b"))))));
        assert_eq!(
            Scheme(HashSet::unit(String::from("a")), t.clone()).apply(&s),
            Scheme(HashSet::unit(String::from("a")), t));
    }
    #[test]
    fn gen_type() {
        let i = Infer::default().set_env(String::from("a"), Scheme(HashSet::new(), Type::Number));
        assert_eq!(
            Type::Number.generalize(&i),
            Scheme(HashSet::new(), Type::Number));
        assert_eq!(
            Type::Variable(String::from("a")).generalize(&i),
            Scheme(HashSet::unit(String::from("a")), Type::Variable(String::from("a"))));
        assert_eq!(
            Type::Variable(String::from("b")).generalize(&i),
            Scheme(HashSet::unit(String::from("b")), Type::Variable(String::from("b"))));
    }
    #[test]
    fn inst_scheme() {
        let i = Infer::default();
        assert_eq!(
            Scheme(HashSet::new(), Type::Variable(String::from("a"))).instantiate(&i),
            Type::Variable(String::from("a")));
        let i = Infer::default();
        assert_eq!(
            Scheme(HashSet::unit(String::from("a")), Type::Variable(String::from("a"))).instantiate(&i),
            Type::Variable(String::from("0")));
        assert_eq!(
            Scheme(HashSet::unit(String::from("b")), Type::Variable(String::from("b"))).instantiate(&i),
            Type::Variable(String::from("1")));
    }
    #[test]
    fn unique_type_var_set_prop() {
        let i = 100;

        let mut t = TypeVarSet::new();
        let mut res = HashSet::new();
        for _ in 0..i {
            res.insert(t.fresh());
        }
        assert_eq!(res.len(), i);
    }
    */
}
