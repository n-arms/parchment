#![allow(dead_code)]

use im::hashset::HashSet;
use im::hashmap::HashMap;
use std::fmt;
use super::expr;
use super::infer::Infer;
use std::rc::Rc;
use std::cell::{RefCell, Cell};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeVarSet {
    index: usize
}

impl TypeVarSet {
    pub fn new() -> Self {
        TypeVarSet {
            index: 0
        }
    }

    pub fn fresh(&mut self) -> String {
        self.index += 1;
        self.index.to_string()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Variable(String),
    Arrow(Box<Type>, Box<Type>),
    Number,
    Boolean,
    Record(HashMap<String, Type>),
    Or(Box<Type>, Box<Type>)
}

impl Type {
    /* create a scheme from a type
     * the polymorphic type vars are all the free vars in the type that are not free in Infer
     */
    pub fn generalize(&self, i: &Infer) -> Scheme {
        Scheme(
            self.free_type_vars()
                .relative_complement(i.free_in_env()),
            self.clone())
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Variable(v) => write!(f, "{}", v),
            Type::Arrow(l, r) => write!(f, "({} -> {})", *l, *r),
            Type::Number => write!(f, "Num"),
            Type::Boolean => write!(f, "Bool"),
            Type::Record(r) => write!(f, "{:?}", r),
            Type::Or(l, r) => write!(f, "({} | {})", *l, *r)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme(pub HashSet<String>, pub Type);

impl Scheme {
    pub fn instantiate(&self, i: &Infer) -> Type {
        let new_tvs : Vec<_> = vec![(); self.0.len()]
            .into_iter()
            .map(|()| Type::Variable(i.fresh()))
            .collect();
        self.1.apply(&Infer::new(
            Rc::new(
                      RefCell::new(
                          HashMap::from(self.0
                                        .clone()
                                        .into_iter()
                                        .zip(new_tvs)
                                        .collect::<Vec<_>>()))),
            Rc::new(Cell::new(0)),
            HashMap::new()
        ))
    }
}

pub trait Substable {
    // fn apply(&self, s: &impl Subst) -> Self;
    fn apply(&self, i: &Infer) -> Self;
    fn free_type_vars(&self) -> HashSet<String>;
    fn contains_var(&self, tv: String) -> bool {
        self.free_type_vars().contains(&tv)
    }
}

impl Substable for Type {
    fn apply(&self, i: &Infer) -> Self {
        match self {
            Type::Arrow(l, r) => Type::Arrow(Box::new(l.apply(i)), Box::new(r.apply(i))),
            Type::Variable(v) =>
                i.apply(v), // don't forget to recurse
            Type::Number => Type::Number,
            Type::Boolean => Type::Boolean,
            Type::Record(r) => Type::Record(r.iter().map(|(k, v)| (k.clone(), v.apply(i))).collect()),
            Type::Or(l, r) => Type::Or(Box::new(l.apply(i)), Box::new(r.apply(i))),
        }
    }
    fn free_type_vars(&self) -> HashSet<String> {
        match self {
            Type::Arrow(l, r) => l.free_type_vars().union(r.free_type_vars()),
            Type::Number => HashSet::new(),
            Type::Boolean => HashSet::new(),
            Type::Variable(v) => HashSet::unit(v.clone()),
            Type::Record(r) => 
                r.values()
                    .fold(HashSet::new(), |mut acc, x| {
                        acc.extend(x.free_type_vars());
                        acc
                    }),
            Type::Or(l, r) => l.free_type_vars().union(r.free_type_vars())
        }
    }
}

impl Substable for Scheme {
    fn apply(&self, i: &Infer) -> Self {
        Scheme(
            self.0.clone(),
            self.1.apply(&i.without_sub(self.0.clone())))
    }
    
    fn free_type_vars(&self) -> HashSet<String> {
        self.1.free_type_vars().relative_complement(self.0.clone())
    }
}

impl fmt::Display for Scheme {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Scheme({:?} {})", self.0, self.1)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn free_tv_mono() {
        assert_eq!(Type::Number.free_type_vars(), HashSet::new());
        assert_eq!(Type::Variable(String::from("a")).free_type_vars(), HashSet::unit(String::from("a")));
        assert_eq!(
            Type::Arrow(
                Box::new(Type::Variable(String::from("a"))),
                Box::new(Type::Variable(String::from("b"))))
                .free_type_vars(), HashSet::unit(String::from("a")).update(String::from("b")));
    }
    #[test]
    fn free_tv_poly() {
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("a")),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("a"))),
                    Box::new(Type::Variable(String::from("b")))))
            .free_type_vars(), HashSet::unit(String::from("b")));
        assert_eq!(
            Scheme(
                HashSet::unit(String::from("a")),
                Type::Record(im::hashmap!{
                    String::from("left") => Type::Variable(String::from("a")),
                    String::from("right") => Type::Variable(String::from("b"))}))
            .free_type_vars(), HashSet::unit(String::from("b")));
    }
    #[test]
    fn sub_mono() {
        let s = Infer::default();
        s.add_sub(String::from("a"), Type::Variable(String::from("0")));
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
}
