use super::expr::*;
use super::types::*;
use im::{HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Constraint {
    Equality(Type, Type),
    /// t1 must be an instance of t2 when generalized over all the variables not in M
    InstanceOf(Type, HashSet<TypeVar>, Type),
}

pub type Assumption = (String, TypeVar);

impl Apply for HashSet<String> {
    fn apply(&self, s: Substitution) -> Self {
        self.iter()
            .flat_map(|tv| Type::Variable(tv.clone()).apply(s.clone()).free_type_vars())
            .collect()
    }
}

impl Apply for Constraint {
    fn apply(&self, s: Substitution) -> Self {
        match self {
            Self::Equality(t1, t2) => Self::Equality(t1.apply(s.clone()), t2.apply(s)),
            Self::InstanceOf(sub, m, sup) => {
                Self::InstanceOf(sub.apply(s.clone()), m.apply(s.clone()), sup.apply(s))
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeError {
    InfiniteType(Type, String),
    ConstructorMismatch(Constructor, Constructor),
    MissingField(String),
    NoSolvableConstraints,
}

pub type Result<A> = std::result::Result<A, TypeError>;

pub fn generate(
    e: &Expr<String>,
    t: &TypeVarSet,
    m: HashSet<TypeVar>,
) -> Result<(HashSet<Assumption>, Vec<Constraint>, Type)> {
    match e {
        Expr::Variable(v) => {
            let b = t.fresh();
            Ok((
                HashSet::unit((v.clone(), b.clone())),
                Vec::new(),
                Type::Variable(b),
            ))
        }
        Expr::Application(e1, e2) => {
            let b = t.fresh();
            let (a1, c1, t1) = generate(e1, t, m.clone())?;
            let (a2, c2, t2) = generate(e2, t, m)?;

            let mut c3 = vec![Constraint::Equality(
                t1,
                Type::Arrow(Box::new(t2), Box::new(Type::Variable(b.clone()))),
            )];
            c3.extend(c1);
            c3.extend(c2);

            Ok((a1.union(a2), c3, Type::Variable(b)))
        }
        // construct a fresh type var for every variable in the pattern
        // run generation for the body
        // for every variable-type pair in the assumption set where the variable is present in the
        // pattern, add an equality constraint between the type in the assumption set and the type
        // generated by the pattern
        //
        // find some way to take the type vars generated by the pattern and produce the "type" of
        // the pattern
        //
        // some kind of "gen bindings" function on patterns that produces both a map from variables
        // to type vars, as well as the overall type of the pattern
        Expr::Function(p, b) => {
            let (bindings, tp) = p.type_pattern(t);
            let (mut a1, c1, t1) = generate(b, t, m.union(bindings.values().collect()))?;
            let mut c2: Vec<_> = a1
                .iter()
                .filter_map(|(var, tv1)| {
                    let tv2 = bindings.get(var)?;
                    Some(Constraint::Equality(
                        Type::Variable(tv1.clone()),
                        Type::Variable(tv2.clone()),
                    ))
                })
                .collect();
            c2.extend(c1);
            a1.retain(|(var, _)| !bindings.contains_key(var));
            Ok((a1, c2, Type::Arrow(Box::new(tp), Box::new(t1))))
        }
        Expr::Block(b) => gen_block(&b[..], t, m),
        Expr::Number(_) => Ok((
            HashSet::new(),
            Vec::new(),
            Type::Constructor(Constructor::Number),
        )),
        Expr::Boolean(_) => Ok((
            HashSet::new(),
            Vec::new(),
            Type::Constructor(Constructor::Boolean),
        )),
        Expr::If(pred, cons, altr) => {
            let (a1, c1, t1) = generate(pred, t, m.clone())?;
            let (a2, c2, t2) = generate(cons, t, m.clone())?;
            let (a3, c3, t3) = generate(altr, t, m)?;

            let mut c4 = vec![
                Constraint::Equality(t2, t3.clone()),
                Constraint::Equality(t1, Type::Constructor(Constructor::Boolean)),
            ];
            c4.extend(c1);
            c4.extend(c2);
            c4.extend(c3);

            let mut a4 = a1;
            a4.extend(a2);
            a4.extend(a3);

            Ok((a4, c4, t3))
        }
        Expr::Record(r) => {
            let mut a = HashSet::new();
            let mut cs = Vec::new();
            let mut ts = HashMap::new();

            for (var, val) in r {
                let (a1, c1, t1) = generate(val, t, m.clone())?;
                a.extend(a1);
                cs.extend(c1);
                ts.insert(var.clone(), t1);
            }

            Ok((a, cs, Type::Record(ts)))
        }
        Expr::Operator(o) => Ok((HashSet::new(), Vec::new(), gen_op(o))),
        Expr::Match(_, _) => todo!(),
    }
}

fn gen_op(o: &Operator) -> Type {
    let num = Box::new(Type::Constructor(Constructor::Number));
    let boolean = Box::new(Type::Constructor(Constructor::Boolean));
    match o {
        Operator::Equals => Type::Arrow(num.clone(), Box::new(Type::Arrow(num, boolean))),
        Operator::Minus | Operator::Times | Operator::Plus => {
            Type::Arrow(num.clone(), Box::new(Type::Arrow(num.clone(), num)))
        }
    }
}

pub fn gen_block(
    b: &[Statement<String>],
    t: &TypeVarSet,
    m: HashSet<String>,
) -> Result<(HashSet<Assumption>, Vec<Constraint>, Type)> {
    if let Some(fst) = b.get(0) {
        match fst {
            Statement::Let(p, body) => {
                let (bindings, tp) = p.type_pattern(t);
                let (mut a1, c1, t1) = generate(body, t, m.clone())?;
                a1.retain(|(var, _)| !bindings.contains_key(var));
                let (mut a2, c2, t2) = gen_block(&b[1..], t, m.clone())?;

                let mut c3: Vec<_> = a2
                    .iter()
                    .filter_map(|(var, tv1)| {
                        let tv2 = bindings.get(var)?;
                        Some(Constraint::InstanceOf(
                            Type::Variable(tv1.clone()),
                            m.clone(),
                            Type::Variable(tv2.clone()),
                        ))
                    })
                    .collect();
                c3.extend(c1);
                c3.extend(c2);
                c3.push(Constraint::Equality(tp, t1));

                a2.retain(|(var, _)| !bindings.contains_key(var));
                a2.extend(a1);

                Ok((a2, c3, t2))
            }
            Statement::Raw(r) => {
                let (a1, c1, t1) = generate(r, t, m.clone())?;
                if b.len() == 1 {
                    Ok((a1, c1, t1))
                } else {
                    let (mut a2, mut c2, t2) = gen_block(&b[1..], t, m)?;
                    c2.extend(c1);
                    a2.extend(a1);

                    Ok((a2, c2, t2))
                }
            }
        }
    } else {
        Ok((
            HashSet::new(),
            Vec::new(),
            Type::Constructor(Constructor::Unit),
        ))
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Equality(l, r) => write!(f, "{} = {}", l, r),
            Constraint::InstanceOf(sub, m, sup) => write!(f, "{} < {} [{:?}]", sub, sup, m),
        }
    }
}
