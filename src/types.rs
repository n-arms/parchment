#![allow(dead_code)]

use im::hashset::HashSet;
use im::hashmap::HashMap;
use std::fmt;
use super::expr;

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
    Record(HashMap<String, Type>)
}

impl Type {
    pub fn generalize(&self, e: &Env) -> Scheme {
        Scheme(
            self.free_type_vars()
                .relative_complement(e.0.keys().collect::<HashSet<_>>()),
            self.clone())
    }
    // in this case env is a map from variable names to types
    #[allow(unused_variables)]
    pub fn type_check(&self, env: &Env, tv: &mut TypeVarSet, e: expr::Expr) -> bool {
        match e {
            expr::Expr::Function(..) => todo!(),
            expr::Expr::Application(..) => todo!(),
            expr::Expr::Number(_) => self == &Type::Number,
            expr::Expr::Boolean(_) => self == &Type::Boolean,
            expr::Expr::Variable(v) => 
                env.0.get(&v).map(|t| &t.instantiate(tv) == self).unwrap_or(false),
            expr::Expr::Let(..) => todo!(),
            expr::Expr::Record(_) => todo!(),
            expr::Expr::If(..) => todo!()
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Variable(v) => write!(f, "{}", v),
            Type::Arrow(l, r) => write!(f, "({} -> {})", *l, *r),
            Type::Number => write!(f, "Num"),
            Type::Boolean => write!(f, "Bool"),
            Type::Record(r) => write!(f, "{:?}", r)
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scheme(pub HashSet<String>, pub Type);

impl Scheme {
    pub fn instantiate(&self, t: &mut TypeVarSet) -> Type {
        let new_tvs : Vec<_> = vec![(); self.0.len()]
            .into_iter()
            .map(|()| Type::Variable(t.fresh()))
            .collect();
        self.1.apply(
            &HashMap::from(
                self.0
                    .clone()
                    .into_iter()
                    .zip(new_tvs)
                    .collect::<Vec<_>>()))
    }
}

pub type Subst = HashMap<String, Type>;

pub trait Substable {
    fn apply(&self, s: &Subst) -> Self;
    fn free_type_vars(&self) -> HashSet<String>;
    fn contains_var(&self, tv: String) -> bool {
        self.free_type_vars().contains(&tv)
    }
}

impl Substable for Type {
    fn apply(&self, s: &Subst) -> Self {
        match self {
            Type::Arrow(l, r) => Type::Arrow(Box::new(l.apply(s)), Box::new(r.apply(s))),
            Type::Variable(v) =>
                s.get(v)
                    .map(|x| x.apply(s)) // dont forget to recurse here, it is a major bug source
                    .unwrap_or(Type::Variable(v.clone())),
            Type::Number => Type::Number,
            Type::Boolean => Type::Boolean,
            Type::Record(r) => Type::Record(r.iter().map(|(k, v)| (k.clone(), v.apply(s))).collect())
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
                    })
        }
    }
}

impl Substable for Scheme {
    fn apply(&self, s: &Subst) -> Self {
        Scheme(
            self.0.clone(), 
            self.1.apply(
                &self.0
                    .iter()
                    .fold(s.clone(), |acc, x| acc.without(x))))

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

// let s' be the substitution of self to every element in other, 
// then take the left biased union s' U self

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Env(pub HashMap<String, Scheme>);
impl Env {
    pub fn new() -> Self {
        Env(HashMap::new())
    }

    pub fn unit(tv: String, t: Scheme) -> Self {
        Env(HashMap::unit(tv, t))
    }
}

impl Substable for Env {
    fn free_type_vars(&self) -> HashSet<String> {
        self.0.values()
            .fold(HashSet::new(), |acc, x| x.free_type_vars().union(acc))
    }

    fn apply(&self, s: &Subst) -> Self {
        Env(self.0.iter()
            .map(|(k, v)| (k.clone(), v.apply(s)))
            .collect())
    }
}

impl fmt::Display for Env {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{ {}}}", self.0.iter()
               .fold(String::new(), |mut acc, (k, v)| {
                   acc.push_str(&format!("{} => {} ", k, v));
                   acc
               }))
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
        let s = HashMap::unit(String::from("a"), Type::Variable(String::from("0")));
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
        let s = HashMap::unit(String::from("a"), Type::Variable(String::from("0")));
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
    fn sub_env() {
        let s = HashMap::unit(String::from("a"), Type::Number);
        let t1 = Type::Variable(String::from("a"));
        let t2 = Type::Variable(String::from("b"));
        let t3 = Type::Arrow(
            Box::new(Type::Variable(String::from("a"))),
            Box::new(Type::Variable(String::from("b"))));
        assert_eq!(Env::new().apply(&s), Env::new());
        assert_eq!(
            Env::unit(String::from("x"), Scheme(HashSet::new(), Type::Number)).apply(&s),
            Env::unit(String::from("x"), Scheme(HashSet::new(), Type::Number)));
        assert_eq!(
            Env::unit(String::from("x"), Scheme(HashSet::new(), t1.clone())).apply(&s),
            Env::unit(String::from("x"), Scheme(HashSet::new(), Type::Number)));
        assert_eq!(
            Env::unit(String::from("x"), Scheme(HashSet::new(), t2.clone())).apply(&s),
            Env::unit(String::from("x"), Scheme(HashSet::new(), t2.clone())));
        assert_eq!(
            Env::unit(String::from("x"), Scheme(HashSet::new(), t3)).apply(&s),
            Env::unit(String::from("x"), Scheme(HashSet::new(), Type::Arrow(
                        Box::new(Type::Number),
                        Box::new(Type::Variable(String::from("b")))))));
    }
    #[test]
    fn gen_type() {
        let e = Env::unit(String::from("a"), Scheme(HashSet::new(), Type::Number));
        assert_eq!(
            Type::Number.generalize(&e),
            Scheme(HashSet::new(), Type::Number));
        assert_eq!(
            Type::Variable(String::from("a")).generalize(&e),
            Scheme(HashSet::new(), Type::Variable(String::from("a"))));
        assert_eq!(
            Type::Variable(String::from("b")).generalize(&e),
            Scheme(HashSet::unit(String::from("b")), Type::Variable(String::from("b"))));
    }
    #[test]
    fn inst_scheme() {
        let mut t = TypeVarSet::new();
        assert_eq!(
            Scheme(HashSet::new(), Type::Variable(String::from("a"))).instantiate(&mut t),
            Type::Variable(String::from("a")));
        assert_eq!(
            Scheme(HashSet::unit(String::from("a")), Type::Variable(String::from("a"))).instantiate(&mut t),
            Type::Variable(String::from("1")));
        assert_eq!(
            Scheme(HashSet::unit(String::from("b")), Type::Variable(String::from("b"))).instantiate(&mut t),
            Type::Variable(String::from("2")));
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
