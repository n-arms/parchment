use super::gen::{Constraint, TypeError};
use super::types::{apply, combine, Substitution, Type};
use im::HashMap;

pub fn solve(cs: Vec<Constraint>) -> Result<Substitution, TypeError> {
    let mut global = Substitution::default();
    for c in cs {
        match c {
            Constraint::Equality(l, r) => {
                global = combine(unify(&apply(&l, &global), &apply(&r, &global))?, global)
            }
            _ => todo!(),
        }
    }
    Ok(global)
}

fn unify(t1: &Type, t2: &Type) -> Result<Substitution, TypeError> {
    match (t1, t2) {
        (Type::Variable(v), t) | (t, Type::Variable(v)) => {
            if t.contains(v) {
                Err(TypeError::InfiniteType(t.clone(), v.clone()))
            } else {
                Ok(HashMap::unit(v.clone(), t.clone()))
            }
        }
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            let s1 = unify(l1, l2)?;
            let s2 = unify(&apply(r1, &s1), &apply(r2, &s1))?;

            Ok(combine(s2, s1))
        }
        (Type::Constructor(c1), Type::Constructor(c2)) => {
            if c1 == c2 {
                Ok(HashMap::new())
            } else {
                Err(TypeError::ConstructorMismatch(c1.clone(), c2.clone()))
            }
        }
        (l, r) => todo!("({}, {})", l, r),
    }
}

#[cfg(test)]
mod test {
    use super::super::types::{Constructor, TypeEnv, TypeVarSet};
    use super::super::{
        expr::{Expr, Pattern},
        gen::*,
        lexer::scan,
        parser::parse_expr,
    };
    use super::*;
    use rand::{thread_rng, Rng};

    fn infer(s: &str) -> Type {
        let e = parse_expr(&scan(s)).unwrap().unwrap().0;
        let (c, t1) = generate(&e, &TypeVarSet::new(), TypeEnv::new()).unwrap();
        apply(&t1, &solve(c).unwrap())
    }

    // during early stages of testing I noticed some non-determainism arising from solving thanks
    // to the underlying hash map.
    #[test]
    fn det_solve() {
        let mut last = None;
        for _ in 0..1000 {
            let v1 = thread_rng().gen::<usize>().to_string();
            let v2 = thread_rng().gen::<usize>().to_string();
            let e = Expr::Application(
                Box::new(Expr::Function(
                    Pattern::Variable(v1.clone()),
                    Box::new(Expr::Variable(v1.clone())),
                )),
                Box::new(Expr::Function(
                    Pattern::Variable(v2.clone()),
                    Box::new(Expr::Variable(v2.clone())),
                )),
            );
            let (c, t) =
                generate(&e, &TypeVarSet::new(), TypeEnv::new()).unwrap();
            println!("{} where", t);
            for c in &c {
                println!("    {}", c);
            }

            let new_t = apply(&t, &solve(c.clone()).unwrap());
            if let Some(l) = last.as_ref() {
                assert_eq!(l, &new_t);
            }
            last = Some(new_t);
        }
    }

    #[test]
    fn complex_exprs() {
        let t = infer("(fn x -> x)(fn x -> x)");

        if let Type::Arrow(l, r) = t {
            assert_eq!(l, r);
        } else {
            panic!("the type {} should be an arrow function", t);
        }

        let t = infer("fn x -> if x then 1 else 0");

        if let Type::Arrow(l, r) = t {
            assert_eq!(l.as_ref(), &Type::Constructor(Constructor::Boolean));
            assert_eq!(r.as_ref(), &Type::Constructor(Constructor::Number));
        } else {
            panic!("the type {} should be an arrow function", t);
        }
    }

    #[test]
    #[should_panic]
    fn infinite_type() {
        infer("(fn x -> x x)");
    }

    #[test]
    #[should_panic]
    fn type_mismatch() {
        infer("fn x -> if x then x else 42");
    }

    #[test]
    fn unify_arrow() {
        let v0 = Box::new(Type::Variable(String::from("0")));
        let v1 = Box::new(Type::Variable(String::from("1")));
        let v2 = Box::new(Type::Variable(String::from("2")));

        let t1 = Type::Arrow(v0.clone(), v0.clone());
        let t2 = Type::Arrow(v1.clone(), v2.clone());

        let s = unify(&t1, &t2).unwrap();

        assert_eq!(apply(&v1, &s), apply(&v2, &s));
    }
}
