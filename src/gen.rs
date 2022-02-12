use super::expr::*;
use super::types::*;
use im::{hashset, HashMap, HashSet};
use std::fmt;

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum Constraint {
    Equality(Type, Type),
    InstanceOf(Type, std::marker::PhantomData<()>, Type),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum TypeError {
    InfiniteType(Type, String),
    ConstructorMismatch(Constructor, Constructor),
    UnknownVariable(String),
}

pub fn generate(
    e: &Expr<String>,
    t: &TypeVarSet,
    env: TypeEnv,
) -> Result<(Vec<Constraint>, Type), TypeError> {
    match e {
        Expr::Variable(v) => {
            let s = env.get(v).ok_or(TypeError::UnknownVariable(v.clone()))?;
            Ok((Vec::new(), s.instantiate(t)))
        }
        Expr::Application(e1, e2) => {
            let b = t.fresh();
            let (c1, t1) = generate(e1, t, env.clone())?;
            let (c2, t2) = generate(e2, t, env)?;

            let mut c = vec![Constraint::Equality(
                t1,
                Type::Arrow(Box::new(t2), Box::new(Type::Variable(b.clone()))),
            )];
            c.extend(c1);
            c.extend(c2);
            Ok((c, Type::Variable(b.clone())))
        }
        Expr::Function(Pattern::Variable(v), e) => {
            let b = t.fresh();
            let (c1, t1) = generate(
                e,
                t,
                env.update(v.clone(), Scheme(HashSet::new(), Type::Variable(b.clone()))),
            )?;
            Ok((c1, Type::Arrow(Box::new(Type::Variable(b)), Box::new(t1))))
        }
        Expr::Number(_) => Ok((Vec::new(), Type::Constructor(Constructor::Number))),
        Expr::Boolean(_) => Ok((Vec::new(), Type::Constructor(Constructor::Boolean))),
        Expr::If(e1, e2, e3) => {
            let (c1, t1) = generate(e1, t, env.clone())?;
            let (c2, t2) = generate(e2, t, env.clone())?;
            let (c3, t3) = generate(e3, t, env)?;
            println!(
                "constraint {} should constrain the consequent and alternative to be the same",
                Constraint::Equality(t2.clone(), t3.clone())
            );

            let mut c = vec![
                Constraint::Equality(t2.clone(), t3),
                Constraint::Equality(Type::Constructor(Constructor::Boolean), t1),
            ];
            c.extend(c1);
            c.extend(c2);
            c.extend(c3);

            Ok((c, t2))
        }
        Expr::Function(..) => todo!(),
        Expr::Record(_) => todo!(),
        Expr::Match(_, _) => todo!(),
        Expr::Block(_) => todo!(),
    }
}

impl fmt::Display for Constraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Constraint::Equality(l, r) => write!(f, "{} = {}", l, r),
            Constraint::InstanceOf(sub, _, sup) => write!(f, "{} < {} [{}]", sub, sup, "env"),
        }
    }
}
