use expr::types::{Apply, Constraint, Fresh, Kind, Substitution, Type, TypeError, Var};
use im::{HashMap, HashSet};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct State {
    fresh_type_vars: Fresh,
}

impl State {
    pub fn new(fresh_type_vars: Fresh) -> Self {
        State { fresh_type_vars }
    }

    pub fn fresh(&self) -> Var {
        Rc::new(self.fresh_type_vars.fresh().to_string())
    }
}

fn combine(s1: Substitution, s2: Substitution) -> Substitution {
    let mut s3: Substitution = s2
        .into_iter()
        .map(|(u, t)| (u, t.apply(&s1.clone())))
        .collect();
    s3.extend(s1);

    s3
}

/// Solves a set of constraints, producing a substitution
///
/// # Errors
/// Will return a type error if a substitution cannot be constructed from the given constraints.
/// This includes infinite types, type mismatches, kind mismatches, etc
///
/// runs in O(n^2) time though there is probably an O(n log n) algorithm
pub fn solve(cs: &HashSet<Constraint>, type_vars: State) -> Result<Substitution, TypeError> {
    if cs.is_empty() {
        return Ok(Substitution::default());
    }
    let (solvable, rest) = next_solvable(cs)?;

    match solvable {
        Constraint::Equality(t1, t2) => {
            let su1 = unify(&t1, &t2)?;
            let su2 = solve(
                &rest.into_iter().map(|c| c.apply(&su1)).collect(),
                type_vars,
            )?;
            Ok(combine(su2, su1))
        }
        Constraint::InstanceOf(sub_type, m, super_type) => {
            let super_gen = super_type.apply(
                &super_type
                    .variables()
                    .relative_complement(m)
                    .into_iter()
                    .map(|var| (var, Type::Variable(type_vars.fresh(), Kind::Star)))
                    .collect(),
            );
            solve(
                &rest.update(Constraint::Equality(sub_type, super_gen)),
                type_vars,
            )
        }
    }
}

fn next_solvable(cs: &HashSet<Constraint>) -> Result<(Constraint, HashSet<Constraint>), TypeError> {
    for c in cs {
        match c {
            Constraint::Equality(..) => return Ok((c.clone(), cs.without(c))),
            Constraint::InstanceOf(_, m, sup) => {
                let cs_active = cs
                    .iter()
                    .map(active_type_vars)
                    .fold(HashSet::new(), HashSet::union);
                let is_solvable = sup
                    .variables()
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

fn active_type_vars(c: &Constraint) -> HashSet<Var> {
    match c {
        Constraint::Equality(t1, t2) => t1.variables().union(t2.variables()),
        Constraint::InstanceOf(sub_type, m, super_type) => sub_type
            .variables()
            .union(m.clone().intersection(super_type.variables())),
    }
}

fn unify(t1: &Type, t2: &Type) -> Result<Substitution, TypeError> {
    match (t1, t2) {
        (Type::Variable(v, k), t) | (t, Type::Variable(v, k)) => {
            if t == &Type::Variable(Rc::clone(v), k.clone()) {
                Ok(HashMap::new())
            } else if t.variables().contains(v) {
                Err(TypeError::InfiniteType(
                    t.clone(),
                    Type::Variable(Rc::clone(v), k.clone()),
                ))
            } else {
                Ok(HashMap::unit(Rc::clone(v), t.clone()))
            }
        }
        (Type::Application(l1, r1), Type::Application(l2, r2))
        | (Type::Arrow(l1, r1), Type::Arrow(l2, r2)) => {
            let s1 = unify(l1, l2)?;
            let s2 = unify(&r1.apply(&s1), &r2.apply(&s1))?;

            Ok(combine(s2, s1))
        }
        (Type::Constant(c1, k1), Type::Constant(c2, k2)) => {
            if k1 != k2 {
                Err(TypeError::KindMismatch(
                    Type::Constant(Rc::clone(c1), k1.clone()),
                    Type::Constant(Rc::clone(c2), k2.clone()),
                ))
            } else if c1 == c2 {
                Ok(HashMap::new())
            } else {
                Err(TypeError::ConstructorMismatch(Rc::clone(c1), Rc::clone(c2)))
            }
        }
        (Type::Record(record1), Type::Record(record2)) => {
            let mut s = Substitution::new();
            for (var, value1) in record1 {
                let value2 = record2
                    .get(var)
                    .ok_or_else(|| TypeError::MissingField(var.clone()))?;
                s = combine(unify(&value1.apply(&s), &value2.apply(&s))?, s);
            }
            Ok(s)
        }
        (Type::Tuple(tuple1), Type::Tuple(tuple2)) => {
            let mut s = Substitution::new();
            for (elem1, elem2) in tuple1.iter().zip(tuple2.iter()) {
                s = combine(unify(&elem1.apply(&s), &elem2.apply(&s))?, s);
            }
            Ok(s)
        }
        (l, r) => Err(TypeError::TypeMismatch(l.clone(), r.clone())),
    }
}

#[cfg(test)]
mod test {
    use expr::types::{bool_type, num_type};
    use expr::{
        expr::{Expr, Pattern},
        lexer::scan,
        parser::parse,
    };
    use super::super::generate;
    use super::*;
    use rand::{thread_rng, Rng};

    fn infer(s: &str) -> Expr<Type> {
        let e = parse(&scan(s)).unwrap();
        let st = generate::State::default();
        let (a, e1) = st.generate(&e).unwrap();
        assert!(a.is_empty());

        let (type_vars, constraints) = st.extract();

        e1.apply(&solve(&constraints, State::new(type_vars)).unwrap())
    }

    // during early stages of testing I noticed some non-determainism arising from solving thanks
    // to the underlying hash map.
    #[test]
    fn det_solve() {
        let mut last: Option<Expr<Type>> = None;
        for _ in 0..1000 {
            let v1 = thread_rng().gen::<usize>().to_string();
            let v2 = thread_rng().gen::<usize>().to_string();
            let e = Expr::Application(
                Box::new(Expr::Function(
                    Pattern::Variable(v1.clone()),
                    Box::new(Expr::Variable(v1.clone(), ())),
                    (),
                )),
                Box::new(Expr::Function(
                    Pattern::Variable(v2.clone()),
                    Box::new(Expr::Variable(v2.clone(), ())),
                    (),
                )),
                (),
            );
            let st = generate::State::default();
            let (a, t) = st.generate(&e).unwrap();
            assert!(a.is_empty());

            println!("base type {}", t.get_type());
            println!("{}", t);

            let (type_vars, constraints) = st.extract();

            let new_t = t.apply(&solve(&constraints, State::new(type_vars)).unwrap());
            if let Some(l) = last.as_ref() {
                assert_eq!(l.get_type(), new_t.get_type());
            }
            last = Some(new_t);
        }
    }

    #[test]
    fn complex_exprs() {
        let t = infer("(fn x -> x)(fn x -> x)").get_type();

        if let Type::Arrow(l, r) = t {
            assert_eq!(l, r);
        } else {
            panic!("the type {} should be an arrow function", t);
        }

        let t = infer("fn x -> if x then 1 else 0").get_type();

        if let Type::Arrow(l, r) = t {
            assert_eq!(l.as_ref(), &bool_type());
            assert_eq!(r.as_ref(), &num_type());
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
        let v0 = Rc::new(Type::Variable(Rc::new(String::from("0")), Kind::Star));
        let v1 = Rc::new(Type::Variable(Rc::new(String::from("1")), Kind::Star));
        let v2 = Rc::new(Type::Variable(Rc::new(String::from("2")), Kind::Star));

        let t1 = Type::Arrow(v0.clone(), v0.clone());
        let t2 = Type::Arrow(v1.clone(), v2.clone());

        let s = unify(&t1, &t2).unwrap();

        assert_eq!(v1.apply(&s), v2.apply(&s));
    }

    #[test]
    fn record() {
        infer("{x:1, y:2}");
    }
}
