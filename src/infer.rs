use super::types::*;
use super::expr::*;
use im::hashmap::HashMap;
use im::hashset::HashSet;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum TypeError {
    UnknownVar(String),
    UnificationFail(Type, Type),
    InfiniteType(String, Type),
    MissingRecordField(String),
    IsntRecord(Type)
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
        (Type::Boolean, Type::Boolean) => Ok(HashMap::new()),
        (Type::Record(r1), Type::Record(r2)) => 
            r1.iter()
                .map(|(k, t1)| {
                    let t2 = r2.get(k).map(|t2| Ok(t2)).unwrap_or(Err(TypeError::MissingRecordField(k.clone())))?;
                    unify(t1.clone(), t2.clone())
                })
                .fold(Ok(HashMap::new()), |mut acc, x| {
                    let x = x?;
                    acc
                        .as_mut()
                        .map(|acc| acc.extend(x))
                        .map_err(|e| e.clone())?;
                    acc
                }),
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
        Expr::Number(_) => Ok((HashMap::new(), Type::Number)), // fairly trivial
        Expr::Boolean(_) => Ok((HashMap::new(), Type::Boolean)), // fairly trivial
        Expr::Variable(v) => {
            let t = env.0.get(&v)
                .map(|s| s.instantiate(t))
                .ok_or(TypeError::UnknownVar(v))?;
            Ok((HashMap::new(), t)) // again, simple
        },
        Expr::Let(p, v, e) => {
            let (s1, t1) = infer(env, t, *v)?;
            let env1 = env.apply(&s1);
            let env2 = p.into_env(env, &t1)?;
            let (s2, t2) = infer(&Env(env2.0.union(env1.0)), t, *e)?;
            Ok((combine(&s1, &s2), t2))
                /* infer the type of the variable's value
                 * apply the resulting sub to env
                 * function that takes in a pattern and a type and returns an env
                 * concat our two envs
                 * infer the type of the body with the new env
                 * combine the subs and return the body type
                */
        },
        Expr::Function(p, b) => {
            let tvs = p.bound_vars();
            let typed_pattern : HashMap<_, _> = tvs.iter().map(|tv| (tv.clone(), Scheme(HashSet::new(), Type::Variable(t.fresh())))).collect();
            let env1 = Env(typed_pattern.clone().union(env.0.clone()));
            let (s1, t1) = infer(&env1, t, *b)?;
            let t2 = p.into_type(&typed_pattern.iter().map(|(k, v)| (k.clone(), v.apply(&s1).instantiate(t))).collect());
            Ok((s1.clone(), Type::Arrow(Box::new(t2.apply(&s1)), Box::new(t1.apply(&s1)))))
            /*
             * function free_vars for all patterns
             * add all free vars to env
             * let t1 be the infered type of the body with the new env
             * convert the pattern into a type t2
             * return an arrow from t2 to t1
            */
        },
        Expr::Record(r) => {
            let (s, rt) = r.iter()
                .map(|(k, v)| (k, infer(env, t, v.clone()))) // an iter over (String, Result<(Subst, Type), TypeError>)
                .fold(Ok((HashMap::new(), HashMap::new())), |mut acc : Result<_, TypeError>, (k, v)| {
                    acc
                        .as_mut()
                        .map_err(|e| e.clone())
                        .and_then(|acc| {
                            v.map(|(st, t)| {
                                acc.0.extend(st);
                                acc.1.insert(k.clone(), t);
                            })
                        })?;
                    acc
                })?;
            Ok((s, Type::Record(rt)))
        },
        Expr::If(p, e1, e2) => {
            let (sp, tp) = infer(env, t, *p)?;
            let (s1, t1) = infer(env, t, *e1)?;
            let (s2, t2) = infer(env, t, *e2)?;
            let s3 = unify(tp.apply(&sp), Type::Boolean)?;
            if let Ok(s4) = unify(t1.apply(&s1), t2.apply(&s2)) {
                Ok((combine(&combine(&combine(&combine(&sp, &s1), &s2), &s3), &s4), t1))
            } else {
                Ok((combine(&combine(&combine(&sp, &s1), &s2), &s3), Type::Or(Box::new(t1), Box::new(t2))))
            }
        },
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
            (Type::Variable(String::from("a")), Type::Record(im::hashmap!{
                String::from("value") => Type::Number
            })),
            (Type::Variable(String::from("a")), Type::Record(HashMap::new())),
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
        assert!(matches!(
                unify(
                    Type::Record(im::hashmap!{
                        String::from("value") => Type::Variable(String::from("a"))
                    }),
                    Type::Variable(String::from("a"))),
                    Err(TypeError::InfiniteType(..))));
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
    #[test]
    fn infer_record() {
        let t = infer_type(parse_expr(&scan("{}")).unwrap().0).unwrap();
        assert_eq!(t, Type::Record(HashMap::new()));

        let t = infer_type(parse_expr(&scan("{a:1, b:2}")).unwrap().0).unwrap();
        assert_eq!(t, Type::Record(im::hashmap!{
            String::from("a") => Type::Number,
            String::from("b") => Type::Number
        }));
    }
}
