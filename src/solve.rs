use super::gen::{Constraint, Result, TypeError};
use super::types::{combine, Apply, Free, Substitution, Type, TypeVar, TypeVarSet};
use im::{HashMap, HashSet};

/// solve a given set of constraints, producing a substitution
///
/// runs in O(n^2) time though there is probably an O(n log n) algorithm
pub fn solve(cs: HashSet<Constraint>, t: &TypeVarSet) -> Result<Substitution> {
    if cs.is_empty() {
        return Ok(Substitution::default());
    }
    let (c, cs) = next_solvable(cs)?;

    match c {
        Constraint::Equality(t1, t2) => {
            let su1 = unify(&t1, &t2)?;
            let su2 = solve(cs.into_iter().map(|c| c.apply(su1.clone())).collect(), t)?;
            Ok(combine(su2, su1))
        }
        Constraint::InstanceOf(sub, m, sup) => {
            let sup_gen = sup.generalize(m).instantiate(t);
            solve(cs.update(Constraint::Equality(sub, sup_gen)), t)
        }
    }
}

fn next_solvable(cs: HashSet<Constraint>) -> Result<(Constraint, HashSet<Constraint>)> {
    for c in &cs {
        match c {
            Constraint::Equality(..) => {
                return Ok((c.clone(), cs.without(c)))
            }
            Constraint::InstanceOf(_, m, sup) => {
                let cs_active = cs
                    .iter()
                    .map(active_type_vars)
                    .fold(HashSet::new(), HashSet::union);
                let is_solvable = sup
                    .free_type_vars()
                    .relative_complement(m.clone())
                    .intersection(cs_active)
                    .is_empty();
                if is_solvable {
                    return Ok((c.clone(), cs.without(c)));
                }
            }
        }
    }

    Err(TypeError::NoSolvableConstraints)
}

fn active_type_vars(c: &Constraint) -> HashSet<TypeVar> {
    match c {
        Constraint::Equality(t1, t2) => {
            t1.free_type_vars().union(t2.free_type_vars())
        }
        Constraint::InstanceOf(sub, m, sup) => sub
            .free_type_vars()
            .union(m.clone().intersection(sup.free_type_vars())),
    }
}

fn unify(t1: &Type, t2: &Type) -> Result<Substitution> {
    match (t1, t2) {
        (Type::Variable(v), t) | (t, Type::Variable(v)) => {
            if t == &Type::Variable(v.clone()) {
                Ok(HashMap::new())
            } else if t.contains(v) {
                Err(TypeError::InfiniteType(t.clone(), v.clone()))
            } else {
                Ok(HashMap::unit(v.clone(), t.clone()))
            }
        }
        (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            let s1 = unify(l1, l2)?;
            let s2 = unify(&r1.apply(s1.clone()), &r2.apply(s1.clone()))?;

            Ok(combine(s2, s1))
        }
        (Type::Constructor(c1), Type::Constructor(c2)) => {
            if c1 == c2 {
                Ok(HashMap::new())
            } else {
                Err(TypeError::ConstructorMismatch(c1.clone(), c2.clone()))
            }
        }
        (Type::Record(r1), Type::Record(r2)) => {
            let mut s = Substitution::new();
            for (var, t1) in r1 {
                let t2 = r2
                    .get(var)
                    .ok_or_else(|| TypeError::MissingField(var.clone()))?;
                s = combine(unify(&t1.apply(s.clone()), &t2.apply(s.clone()))?, s);
            }
            Ok(s)
        }
        (Type::Tuple(ts1), Type::Tuple(ts2)) => {
            let mut s = Substitution::new();
            for (t1, t2) in ts1.iter().zip(ts2.iter()) {
                s = combine(unify(&t1.apply(s.clone()), &t2.apply(s.clone()))?, s);
            }
            Ok(s)
        }
        (Type::Defined(dt1), Type::Defined(dt2)) if dt1 == dt2 =>
            Ok(Substitution::new()),
        (l, r) => Err(TypeError::TypeMismatch(l.clone(), r.clone())),
    }
}

#[cfg(test)]
mod test {
    use super::super::types::{Constructor, TypeVarSet};
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
        let tvs = TypeVarSet::default();
        let (a, c, t1) = generate(&e, &tvs, HashSet::new()).unwrap();
        assert!(a.is_empty());
        t1.apply(solve(c.into_iter().collect(), &tvs).unwrap())
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
            let tvs = TypeVarSet::default();
            let (a, c, t) = generate(&e, &tvs, HashSet::new()).unwrap();
            assert!(a.is_empty());
            println!("{} where", t);
            for c in &c {
                println!("    {}", c);
            }

            let new_t = t.apply(solve(c.into_iter().collect(), &tvs).unwrap());
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

        assert_eq!(v1.apply(s.clone()), v2.apply(s));
    }

    #[test]
    fn record() {
        infer("{x:1, y:2,}");
    }
}
