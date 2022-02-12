use super::expr::*;
use super::types::*;
use im::hashmap::HashMap;
use im::hashset::HashSet;
use std::cell::{Cell, RefCell};
use std::rc::Rc;

pub type InferResult<T> = Result<T, TypeError>;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Infer {
    subs: Rc<RefCell<HashMap<String, Type>>>,
    type_vars: Rc<Cell<usize>>,
    env: HashMap<String, Scheme>,
}

impl Infer {
    pub fn new(
        subs: Rc<RefCell<HashMap<String, Type>>>,
        type_vars: Rc<Cell<usize>>,
        env: HashMap<String, Scheme>,
    ) -> Self {
        Infer {
            subs,
            type_vars,
            env,
        }
    }

    pub fn without_sub(&self, keys: HashSet<String>) -> Self {
        let new_subs = keys
            .iter()
            .fold(self.subs.borrow().clone(), |acc, x| acc.without(x));

        Infer {
            subs: Rc::new(RefCell::new(new_subs)),
            type_vars: self.type_vars.clone(),
            env: HashMap::new(),
        }
    }

    pub fn set_env(&self, var: String, scheme: Scheme) -> Self {
        Infer {
            subs: self.subs.clone(),
            type_vars: self.type_vars.clone(),
            env: self.env.update(var, scheme),
        }
    }

    pub fn apply(&self, type_var: &str) -> Type {
        self.subs
            .borrow()
            .get(type_var)
            .map(|t| t.apply(self))
            .unwrap_or(Type::Variable(type_var.to_string()))
    }

    pub fn add_sub(&self, type_var: String, type_val: Type) {
        self.subs.replace_with(|subs| {
            if subs.contains_key(&type_var) {
                panic!("attempt to overwrite sub");
            }
            subs.update(type_var, type_val)
        });
    }

    pub fn var_type(&self, var: &str) -> InferResult<Scheme> {
        self.env
            .get(var)
            .cloned()
            .ok_or(TypeError::UnknownVar(var.to_string()))
    }

    pub fn free_in_env(&self) -> HashSet<String> {
        self.env.values().fold(HashSet::new(), |mut acc, x| {
            acc.extend(x.free_type_vars().iter());
            acc
        })
    }

    pub fn fresh(&self) -> TypeVar {
        todo!()
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    UnknownVar(String),
    UnificationFail(Type, Type),
    InfiniteType(String, Type),
    MissingRecordField(String),
    IsntRecord(Type),
    EmptyMatch,
}

pub fn unify(i: &Infer, general: Type, specific: Type) -> InferResult<()> {
    match (general.apply(i), specific.apply(i)) {
        (Type::Arrow(lg, rg), Type::Arrow(ls, rs)) => {
            // let s1 = unify(lg.as_ref().clone(), ls.as_ref().clone())?;
            unify(i, *lg, *ls)?;
            unify(i, *rg, *rs)?;
            Ok(())
        }
        (Type::Variable(a), t) | (t, Type::Variable(a)) => {
            if t == Type::Variable(a.clone()) {
                Ok(())
            } else if t.contains_var(a.clone()) {
                Err(TypeError::InfiniteType(a, t))
            } else {
                i.add_sub(a, t);
                Ok(())
            }
        }
        (Type::Number, Type::Number) => Ok(()),
        (Type::Boolean, Type::Boolean) => Ok(()),
        (Type::Record(r1), Type::Record(r2)) => r1
            .iter()
            .map(|(k, t1)| {
                let t2 = r2
                    .get(k)
                    .map(|t2| Ok(t2))
                    .unwrap_or(Err(TypeError::MissingRecordField(k.clone())))?;
                unify(i, t1.clone(), t2.clone())
            })
            .fold(Ok(()), |acc, x| acc.and_then(|_| x)),
        (Type::Or(l, r), t) => {
            unify(i, *l.clone(), t.clone())?;
            unify(i, *r.clone(), t)?;
            unify(i, *l, *r)?;
            Ok(())
        }
        (t1, t2) => Err(TypeError::UnificationFail(t1, t2)),
    }
}

pub fn make_subset(i: &Infer, sub: Type, sup: Type) -> InferResult<()> {
    unify(i, sup, sub)
}

pub fn infer(i: &Infer, e: Expr) -> InferResult<Type> {
    println!("infer on expr {} with env {:?}\n", e, i.env);
    match e {
        Expr::Application(l, r) => {
            let tv = Type::Variable(i.fresh()); // generate a new type var
            let t1 = infer(i, *l)?; // infer the left side
            let t2 = infer(i, *r)?; // infer the right side
                                    // unify the left side with an arrow from a right side to a tv
                                    // if the left side is a (int | bool) -> int and the right side is bool, then thats fine
                                    // the general type is on the left
            unify(i, t1, Type::Arrow(Box::new(t2), Box::new(tv.clone())))?;
            println!("new subs is {:?}", i.subs);
            // combine the 3 subs, then return the new type var
            Ok(tv)
        }
        Expr::Number(_) => Ok(Type::Number),   // fairly trivial
        Expr::Boolean(_) => Ok(Type::Boolean), // fairly trivial
        Expr::Variable(v) => {
            let sigma = i.var_type(&v)?;
            Ok(sigma.instantiate(i)) // again, simple
        }
        /*
        infer the type of (\p -> e) m, forall patterns and expressions e
        take the most general version of both the input and output (and merge all the subs)
        */
        Expr::Match(m, l) => {
            /*
            let mut terms: Vec<Type> = Vec::new();
            for (p, e) in l {
                let t1 = infer(i, Expr::Application(
                        Box::new(Expr::Function(p, Box::new(e))), m.clone()))?;
                terms.push(t1);
            }
            let t_total = terms
                .into_iter()
                .fold(None, |acc, t| Some(match acc {
                    Some(acc) => Type::Or(Box::new(acc), Box::new(t)),
                    None => t
                }));
            Ok(t_total.ok_or(TypeError::EmptyMatch)?)
            */
            let mut inferences = Vec::new();
            for (p, e) in l {
                let i1 = Infer {
                    subs: Rc::new(RefCell::new(i.subs.borrow().clone())),
                    env: i.env.clone(),
                    type_vars: i.type_vars.clone(),
                };
                let t1 = infer(
                    &i1,
                    Expr::Application(Box::new(Expr::Function(p, Box::new(e))), m.clone()),
                )?
                .apply(&i);
                inferences.push((i1, t1));
            }
            println!("inferences {:#?}", inferences);
            if inferences.len() == 1 {
                Ok(inferences[0].1.clone())
            } else {
                todo!();
            }
            /*
            fn x -> match x with
                n -> if n then 3 else 4   -- (n : Bool, Num)
                n -> n + 1   -- (n : Num, Num)

            we want this to spit out (Num | Bool) -> Num

            how do we combine subs?
            first apply each sub to its type (this prevents if n then 3 else 4 from failing on n : Bool | Num)
            the merge the subs. If two conflict, stick them in an or
            use the new sub to rebuild the type of the predicate
            combine the output types (or on conflict) and use them as the output
            */
        }
        Expr::Function(p, b) => {
            let tvs = p.bound_vars();
            let typed_pattern: HashMap<_, _> = tvs
                .iter()
                .map(|tv| {
                    (
                        tv.clone(),
                        Scheme(HashSet::new(), Type::Variable(i.fresh())),
                    )
                })
                .collect();
            let i1 = typed_pattern.iter().fold(i.clone(), |acc, (var, scheme)| {
                acc.set_env(var.clone(), scheme.clone())
            });

            let t1 = infer(&i1, *b)?;
            let t2 = p.into_type(
                &typed_pattern
                    .iter()
                    .map(|(k, v)| (k.clone(), v.instantiate(&i1)))
                    .collect(),
            );
            Ok(Type::Arrow(Box::new(t2), Box::new(t1)))
            /*
             * function free_vars for all patterns
             * add all free vars to env
             * let t1 be the infered type of the body with the new env
             * convert the pattern into a type t2
             * return an arrow from t2 to t1
             */
        }
        /*
                Expr::Let(p, v, e) => {
                    let t1 = infer(i, *v)?;
                    let i2 = p.into_env(i, &t1)?
                        .into_iter()
                        .fold(i.clone(), |acc, (k, v)| acc.set_env(k, v));
                    let t2 = infer(&i2, *e)?;
                    Ok(t2)
                        /* infer the type of the variable's value
                         * apply the resulting sub to env
                         * function that takes in a pattern and a type and returns an env
                         * concat our two envs
                         * infer the type of the body with the new env
                         * combine the subs and return the body type
                        */
                },
        */
        Expr::Block(_) => todo!(),
        Expr::Record(r) => {
            let rt = r
                .iter()
                .map(|(k, v)| (k, infer(i, v.clone()))) // an iter over (String, Result<(Subst, Type), TypeError>)
                .fold(
                    Ok(HashMap::new()),
                    |mut acc: Result<_, TypeError>, (k, v)| {
                        acc.as_mut().map_err(|e| e.clone()).and_then(|acc| {
                            v.map(|t| {
                                acc.insert(k.clone(), t);
                            })
                        })?;
                        acc
                    },
                )?;
            Ok(Type::Record(rt))
        }
        Expr::If(p, e1, e2) => {
            let predicate = infer(i, *p)?;
            let consequent = infer(i, *e1)?;
            let alternative = infer(i, *e2)?;
            unify(i, predicate, Type::Boolean)?;
            if let Ok(()) = unify(i, consequent.clone(), alternative.clone()) {
                Ok(consequent)
            } else {
                Ok(Type::Or(Box::new(consequent), Box::new(alternative)))
            }
        }
    }
}

pub fn infer_type(e: Expr) -> Result<Type, TypeError> {
    let i = Infer::default();
    let t = infer(&i, e)?;
    Ok(t.apply(&i))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::*;
    use crate::parser::*;
    #[test]
    fn infer_num() {
        let i = Infer::default();
        assert_eq!(
            infer(&i, Expr::Number(3.14)).map(|t| t.apply(&i)),
            Ok(Type::Number)
        );
    }
    #[test]
    fn infer_var() {
        use std::matches;
        let i = Infer::default();
        assert!(matches!(
            infer(&i, Expr::Variable(String::from("a"))).map(|t| t.apply(&i)),
            Err(TypeError::UnknownVar(_))
        ));
        let i = Infer::default();
        let i = i.set_env(
            String::from("a"),
            Scheme(
                HashSet::unit(String::from("a")),
                Type::Variable(String::from("a")),
            ),
        );
        let t = infer(&i, Expr::Variable(String::from("a"))).map(|t| t.apply(&i));
        assert!(matches!(t, Ok(Type::Variable(_))));
    }
    #[test]
    fn infer_fn() {
        let t = infer_type(Expr::Function(
            Pattern::Variable(String::from("x")),
            Box::new(Expr::Variable(String::from("x"))),
        ));
        match t {
            Ok(Type::Arrow(l, r)) => {
                assert!(matches!(*l, Type::Variable(_)));
                assert!(matches!(*r, Type::Variable(_)));
            }
            _ => panic!("{:?} :/: a -> a", t),
        }
        let t2 = infer_type(parse_expr(&scan("fn a -> fn b -> a")).unwrap().unwrap().0);
        match t2.clone() {
            Ok(Type::Arrow(a, rest)) => {
                if let Type::Arrow(l, r) = *rest {
                    assert!(matches!(*a, Type::Variable(_)));
                    assert!(matches!(*l, Type::Variable(_)));
                    assert!(matches!(*r, Type::Variable(_)));
                    assert_eq!(a, r);
                } else {
                    panic!("{:?} :/: a -> b -> a", t2)
                }
            }
            _ => panic!("{:?} :/: a -> b -> a", t2),
        }
    }
    #[test]
    fn infer_expr() {
        let (e, _) = parse_expr(&scan("fn a -> a (a 1)")).unwrap().unwrap();
        assert_eq!(
            infer_type(e),
            Ok(Type::Arrow(
                Box::new(Type::Arrow(Box::new(Type::Number), Box::new(Type::Number))),
                Box::new(Type::Number)
            ))
        );
        let t = infer_type(
            parse_expr(&scan("fn f -> fn a -> f (f a)"))
                .unwrap()
                .unwrap()
                .0,
        );
        match t {
            Ok(Type::Arrow(l, r)) => {
                assert_eq!(l, r);
                match *l {
                    Type::Arrow(a, b) => assert_eq!(a, b),
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    #[test]
    fn unify_types() {
        let should_work = vec![
            (Type::Number, Type::Number),
            (
                Type::Variable(String::from("a")),
                Type::Variable(String::from("a")),
            ),
            (Type::Variable(String::from("a")), Type::Number),
            (
                Type::Variable(String::from("a")),
                Type::Variable(String::from("b")),
            ),
            (
                Type::Variable(String::from("a")),
                Type::Record(im::hashmap! {
                    String::from("value") => Type::Number
                }),
            ),
            (
                Type::Variable(String::from("a")),
                Type::Record(HashMap::new()),
            ),
            (
                Type::Arrow(
                    Box::new(Type::Variable(String::from("a"))),
                    Box::new(Type::Variable(String::from("b"))),
                ),
                Type::Variable(String::from("c")),
            ),
        ];
        for (l, r) in should_work {
            let i = Infer::default();
            if let Ok(s) = unify(&i, l.clone(), r.clone()) {
                assert_eq!(l.apply(&i), r.apply(&i));
            }
        }
    }

    #[test]
    fn unify_illegal_types() {
        use std::matches;

        let i = Infer::default();
        assert!(matches!(
            unify(
                &i,
                Type::Number,
                Type::Arrow(Box::new(Type::Number), Box::new(Type::Number))
            ),
            Err(TypeError::UnificationFail(_, _))
        ));

        let i = Infer::default();
        assert!(matches!(
            unify(
                &i,
                Type::Variable(String::from("a")),
                Type::Arrow(
                    Box::new(Type::Variable(String::from("a"))),
                    Box::new(Type::Number)
                )
            ),
            Err(TypeError::InfiniteType(_, _))
        ));

        let i = Infer::default();
        assert!(matches!(
            unify(
                &i,
                Type::Record(im::hashmap! {
                    String::from("value") => Type::Variable(String::from("a"))
                }),
                Type::Variable(String::from("a"))
            ),
            Err(TypeError::InfiniteType(..))
        ));
    }
    #[test]
    fn infer_tricky_expr() {
        let t = infer_type(parse_expr(&scan("fn a b c -> c b a")).unwrap().unwrap().0).unwrap();
        match t {
            Type::Arrow(t1, r) => match *r {
                Type::Arrow(t2, r) => match *r {
                    Type::Arrow(r, t3) => {
                        assert_ne!(t1, t2);
                        assert_ne!(t3, t2);
                        assert_eq!(Type::Arrow(t2, Box::new(Type::Arrow(t1, t3))), *r);
                    }
                    _ => panic!(),
                },
                _ => panic!(),
            },
            _ => panic!(),
        }
    }
    #[test]
    fn infer_let() {
        let t = infer_type(parse_expr(&scan("let x = 3 in x")).unwrap().unwrap().0).unwrap();
        assert_eq!(t, Type::Number);
    }
    #[test]
    fn infer_polymorphic_let() {
        let t = infer_type(
            parse_expr(&scan("let id = fn x -> x in id id"))
                .unwrap()
                .unwrap()
                .0,
        )
        .unwrap();
        match t {
            Type::Arrow(t0, t1) if t0 == t1 => (),
            _ => panic!(),
        }
    }
    #[test]
    fn infer_renaming_let() {
        let t = infer_type(
            parse_expr(&scan("fn x -> let n = x in if n then 3 else 4"))
                .unwrap()
                .unwrap()
                .0,
        )
        .unwrap();
        assert_eq!(
            t,
            Type::Arrow(Box::new(Type::Boolean), Box::new(Type::Number))
        );
    }
    #[test]
    fn infer_record() {
        let t = infer_type(parse_expr(&scan("{}")).unwrap().unwrap().0).unwrap();
        assert_eq!(t, Type::Record(HashMap::new()));

        let t = infer_type(parse_expr(&scan("{a:1, b:2}")).unwrap().unwrap().0).unwrap();
        assert_eq!(
            t,
            Type::Record(im::hashmap! {
                String::from("a") => Type::Number,
                String::from("b") => Type::Number
            })
        );
    }
    #[test]
    fn trivial_infer_match() {
        let t = infer_type(
            parse_expr(&scan("fn x -> match x with x -> x end"))
                .unwrap()
                .unwrap()
                .0,
        )
        .unwrap();
        match t {
            Type::Arrow(l, r) if l == r && matches!(*l, Type::Variable(_)) => (),
            _ => panic!("{:?}", t),
        }

        let t = infer_type(
            parse_expr(&scan("fn x -> match x with n -> if n then 3 else 4 end"))
                .unwrap()
                .unwrap()
                .0,
        )
        .unwrap();
        assert_eq!(
            t,
            Type::Arrow(Box::new(Type::Boolean), Box::new(Type::Number))
        );
    }
    #[test]
    fn infer_match() {
        let t = infer_type(
            parse_expr(&scan(
                "fn x -> match x with n -> if n then 3 else 4, m -> m",
            ))
            .unwrap()
            .unwrap()
            .0,
        )
        .unwrap();
        match t {
            Type::Arrow(l, r) if matches!(*l, Type::Variable(_)) && l == r => (),
            _ => panic!("{:?}", t),
        }
    }
    #[test]
    fn infer_if() {
        let t = infer_type(
            parse_expr(&scan("fn x -> if x then 3 else x"))
                .unwrap()
                .unwrap()
                .0,
        )
        .unwrap();
        match t {
            Type::Arrow(l, r) if *l == Type::Boolean => {
                if let Type::Or(b, n) = *r {
                    assert!(
                        (matches!(*b, Type::Boolean) && matches!(*n, Type::Number))
                            || (matches!(*b, Type::Number) && matches!(*n, Type::Boolean))
                    );
                } else {
                    panic!("{:?}", Type::Arrow(l, r))
                }
            }
            _ => panic!("{:?}", t),
        }
    }
}
