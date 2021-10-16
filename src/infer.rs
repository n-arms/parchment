use super::types::*;
use super::expr::*;
use im::hashmap::HashMap;
use im::hashset::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    UnknownVar(String),
    UnificationFail(Type, Type),
    InfiniteType(String, Type)
}

fn combine(s1: &Subst, s2: &Subst) -> Subst {
    s1.iter()
        .map(|(k, v)| (k.clone(), v.apply(s2)))
        .collect::<Subst>()
        .union(s2.clone())
}

pub fn unify(t1: Type, t2: Type) -> Result<Subst, TypeError> {
    match (t1, t2) { (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            let s1 = unify(l1.as_ref().clone(), l2.as_ref().clone())?;
            // let s1 = unify(*l1, *l2)?;
            let s2 = unify(r1.apply(&s1), r2.apply(&s1))?;
            Ok(combine(&s2, &s1))
        },
        (Type::Variable(a), t) | (t, Type::Variable(a)) =>
            if t == Type::Variable(a.clone()) {
                Ok(HashMap::new())
            } else if t.contains_var(a.clone()) {
                Err(TypeError::InfiniteType(a, t))
            } else {
                Ok(HashMap::unit(a, t))
            },
        (Type::Number, Type::Number) => Ok(HashMap::new()),
        (t1, t2) => Err(TypeError::UnificationFail(t1, t2))
    }
}

pub fn infer(env: &Env, t: &mut TypeVarSet, e: Expr) -> Result<(Subst, Type), TypeError> {
    match e {
        Expr::Application(l, r) => {
            let tv = Type::Variable(t.fresh()); // generate a new type var
            let (s1, t1) = infer(env, t, *l)?; // infer the left side
            let (s2, t2) = infer(&env.apply(&s1), t, *r)?; // infer the right side
            // unify the left side with an arrow from a right side to a tv
            let s3 = unify(t1.apply(&s2), Type::Arrow(Box::new(t2), Box::new(tv.clone())))?;
            // combine the 3 subs, then return the new type var
            Ok((combine(&combine(&s3, &s2), &s1), tv.apply(&s3)))
        },
        Expr::Function(Pattern::Variable(p), b) => {
            let tv = t.fresh(); // generate a new type var
            // tell env that the variable p is of type tv
            let env1 = env.0.update(p, Scheme(HashSet::new(), Type::Variable(tv.clone())));
            // infer the type of the body under the new env
            let (s1, t1) = infer(&Env(env1), t, *b)?;
            // return an arrow from the type var to the infered type
            let t2 = Type::Arrow(Box::new(Type::Variable(tv).apply(&s1)), Box::new(t1));
            Ok((s1, t2))
        },
        Expr::Number(_) => Ok((HashMap::new(), Type::Number)), // fairly trivial
        Expr::Variable(v) => {
            let t = env.0.get(&v)
                .map(|s| s.instantiate(t))
                .ok_or(TypeError::UnknownVar(v))?;
            Ok((HashMap::new(), t)) // again, simple
        },
        Expr::Let(Pattern::Variable(p), v, e) => {
            let (s1, t1)= infer(env, t, *v)?;
            let env1 = env.apply(&s1);
            let t0 = t1.generalize(&env1);
            let (s2, t2) = infer(&Env(env1.0.update(p, t0)), t, *e)?;
            Ok((combine(&s1, &s2), t2))
        }
    }
}

pub fn infer_type(e: Expr) -> Result<Type, TypeError> {
    let (s, t) = infer(&Env(HashMap::new()), &mut TypeVarSet::new(), e)?;
    Ok(t.apply(&s))
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::*;
    use crate::parser::*;
    #[test]
    fn infer_num() {
        assert_eq!(
            infer(
                &Env(HashMap::new()), 
                &mut TypeVarSet::new(), 
                Expr::Number(3.14)), Ok((HashMap::new(), Type::Number)));
    }
    #[test]
    fn infer_var() {
        use std::matches;
        assert!(
            matches!(
                infer(
                    &Env(HashMap::new()), 
                    &mut TypeVarSet::new(), 
                    Expr::Variable(String::from("a"))), 
                Err(TypeError::UnknownVar(_))));
        let t = infer(
            &Env(HashMap::unit(String::from("a"), Scheme(HashSet::unit(String::from("a")), Type::Variable(String::from("a"))))), 
            &mut TypeVarSet::new(), 
            Expr::Variable(String::from("a")));
        match t {
            Ok((h, Type::Variable(_))) if h == HashMap::new() => (),
            _ => panic!("{:?} is not a type variable with empty sub", t)
        }
    }
    #[test]
    fn infer_fn() {
        let t = infer_type(
                    Expr::Function(
                        Pattern::Variable(String::from("x")),
                        Box::new(Expr::Variable(String::from("x")))));
        match t {
            Ok(Type::Arrow(l, r)) => {
                assert!(matches!(*l, Type::Variable(_)));
                assert!(matches!(*r, Type::Variable(_)));
            },
            _ => panic!("{:?} :/: a -> a", t)
        }
        let t2 = infer_type(parse_expr(&scan("fn a -> fn b -> a")).unwrap().0);
        match t2.clone() {
            Ok(Type::Arrow(a, rest)) => 
                if let Type::Arrow(l, r) = *rest {
                    assert!(matches!(*a, Type::Variable(_)));
                    assert!(matches!(*l, Type::Variable(_)));
                    assert!(matches!(*r, Type::Variable(_)));
                    assert_eq!(a, r);
                } else {
                    panic!("{:?} :/: a -> b -> a", t2)
                },
            _ => panic!("{:?} :/: a -> b -> a", t2)
        }
    }
    #[test]
    fn infer_expr() {
        let (e, _) = parse_expr(&scan("fn a -> a (a 1)")).unwrap();
        assert_eq!(
            infer_type(e),
            Ok(Type::Arrow(
                    Box::new(Type::Arrow(
                            Box::new(Type::Number),
                            Box::new(Type::Number))),
                    Box::new(Type::Number))));
        let t = infer_type(parse_expr(&scan("fn f -> fn a -> f (f a)")).unwrap().0);
        match t {
            Ok(Type::Arrow(l, r)) => {
                assert_eq!(l, r);
                match *l {
                    Type::Arrow(a, b) => assert_eq!(a, b),
                    _ => panic!()
                }
            }
            _ => panic!()
        }
    }
    #[test]
    fn combine_subst() {
        assert_eq!(
            combine(&HashMap::new(), &HashMap::new()),
            HashMap::new());
        assert_eq!(
            combine(&HashMap::new(), &HashMap::unit(String::from("a"), Type::Number)),
            HashMap::unit(String::from("a"), Type::Number));
        assert_eq!(
            combine(&HashMap::unit(String::from("a"), Type::Number), &HashMap::new()),
            HashMap::unit(String::from("a"), Type::Number));
        assert_eq!(
            combine(
                &HashMap::unit(String::from("a"), Type::Number), 
                &HashMap::unit(String::from("a"), Type::Variable(String::from("0")))),
            HashMap::unit(String::from("a"), Type::Number));
        assert_eq!(
            combine(
                &HashMap::unit(String::from("a"), Type::Variable(String::from("0"))),
                &HashMap::unit(String::from("a"), Type::Number)), 
            HashMap::unit(String::from("a"), Type::Variable(String::from("0"))));
    }

    #[test]
    fn unify_types() {
        let should_work = vec![
            (Type::Number, Type::Number),
            (Type::Variable(String::from("a")), Type::Variable(String::from("a"))),
            (Type::Variable(String::from("a")), Type::Number),
            (Type::Variable(String::from("a")), Type::Variable(String::from("b"))),
            (Type::Arrow(
                    Box::new(Type::Variable(String::from("a"))),
                    Box::new(Type::Variable(String::from("b")))), Type::Variable(String::from("c")))
        ];
        for (l, r) in should_work {
            if let Ok(s) = unify(l.clone(), r.clone()) {
                assert_eq!(l.apply(&s), r.apply(&s));
            }
        }
    }

    #[test]
    fn unify_illegal_types() {
        use std::matches;
        
        assert!(matches!(
                unify(
                    Type::Number,
                    Type::Arrow(
                        Box::new(Type::Number),
                        Box::new(Type::Number))), Err(TypeError::UnificationFail(_, _))));
        assert!(matches!(
                unify(
                    Type::Variable(String::from("a")),
                    Type::Arrow(
                        Box::new(Type::Variable(String::from("a"))),
                        Box::new(Type::Number))), Err(TypeError::InfiniteType(_, _))));
    }
    #[test]
    fn infer_tricky_expr() {
        let t = infer_type(parse_expr(&scan("fn a b c -> c b a")).unwrap().0).unwrap();
        match t {
            Type::Arrow(t1, r) => match *r {
                Type::Arrow(t2, r) => match *r {
                    Type::Arrow(r, t3) => {
                        assert_ne!(t1, t2);
                        assert_ne!(t3, t2);
                        assert_eq!(Type::Arrow(t2, Box::new(Type::Arrow(t1, t3))), *r);
                    },
                    _ => panic!()
                },
                _ => panic!()
            },
            _ => panic!()
        }
    }
    #[test]
    fn infer_let() {
        let t = infer_type(parse_expr(&scan("let x = 3 in x")).unwrap().0).unwrap();
        assert_eq!(t, Type::Number);
    }
    #[test]
    fn infer_polymorphic_let() {
        let t = infer_type(parse_expr(&scan("let id = fn x -> x in id id")).unwrap().0).unwrap();
        match t {
            Type::Arrow(t0, t1) if t0 == t1 => (),
            _ => panic!()
        }
    }
}
