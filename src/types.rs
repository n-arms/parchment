use super::expr::{Expr, Pattern, Statement};
use im::{HashMap, HashSet};
use std::cell::Cell;
use std::fmt::Display;
use std::rc::Rc;

pub type Var = Rc<String>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Constant(Var, Kind),
    Variable(Var, Kind),
    Arrow(Rc<Type>, Rc<Type>),
    Tuple(Vec<Type>),
    Record(HashMap<String, Type>),
    Application(Rc<Type>, Rc<Type>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub constructor: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef {
    pub polymorphic_vars: Vec<(Var, Kind)>,
    pub variants: HashSet<Variant>,
}

impl Type {
    pub fn variables(&self) -> HashSet<Var> {
        match self {
            Type::Constant(_, _) => HashSet::new(),
            Type::Variable(var, _) => HashSet::unit(var.clone()),
            Type::Application(left, right) | Type::Arrow(left, right) => {
                left.variables().union(right.variables())
            }
            Type::Tuple(tuple) => tuple.iter().flat_map(Type::variables).collect(),
            Type::Record(record) => record.values().flat_map(Type::variables).collect(),
        }
    }
}

/// a supply for fresh type variables
#[derive(Default, Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Fresh(Rc<Cell<usize>>);

impl Fresh {
    pub fn fresh(&self) -> usize {
        let old = self.0.take();
        self.0.set(old + 1);
        old
    }
}

pub fn num_type() -> Type {
    Type::Constant(Rc::new(String::from("Num")), Kind::Star)
}

pub fn bool_type() -> Type {
    Type::Constant(Rc::new(String::from("Bool")), Kind::Star)
}

pub fn unit_type() -> Type {
    Type::Constant(Rc::new(String::from("Unit")), Kind::Star)
}

pub type Substitution = HashMap<Var, Type>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// t1 == t2
    Equality(Type, Type),
    /// t1 is an instance of t2
    InstanceOf(Type, HashSet<Var>, Type),
}

pub trait Apply {
    fn apply(&self, s: &Substitution) -> Self;
}

impl Apply for Type {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Type::Constant(_, _) => self.clone(),
            Type::Variable(var, kind) => {
                if let Some(t) = s.get(var) {
                    t.clone()
                } else {
                    self.clone()
                }
            }
            Type::Arrow(left, right) => {
                Type::Arrow(Rc::new(left.apply(s)), Rc::new(right.apply(s)))
            }
            Type::Tuple(tuple) => Type::Tuple(tuple.iter().map(|t| t.apply(s)).collect()),
            Type::Record(record) => Type::Record(
                record
                    .iter()
                    .map(|(var, t)| (var.clone(), t.apply(s)))
                    .collect(),
            ),
            Type::Application(left, right) => {
                Type::Application(Rc::new(left.apply(s)), Rc::new(right.apply(s)))
            }
        }
    }
}

impl Apply for Constraint {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Self::Equality(left, right) => Self::Equality(left.apply(s), right.apply(s)),
            Self::InstanceOf(sub, m, sup) => Self::InstanceOf(
                sub.apply(s),
                m.iter()
                    .map(|v| Type::Variable(v.clone(), Kind::Star).apply(&s))
                    .flat_map(|t| t.variables())
                    .collect(),
                sup.apply(s),
            ),
        }
    }
}

impl Apply for Expr<Type> {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Expr::Function(pattern, body, pattern_type) => Expr::Function(
                pattern.clone(),
                Box::new(body.apply(s)),
                pattern_type.apply(s),
            ),
            Expr::Application(left, right, app_type) => Expr::Application(
                Box::new(left.apply(s)),
                Box::new(right.apply(s)),
                app_type.apply(s),
            ),
            Expr::Number(n) => Expr::Number(*n),
            Expr::Boolean(b) => Expr::Boolean(*b),
            Expr::Operator(operator, operator_type) => {
                Expr::Operator(*operator, operator_type.apply(s))
            }
            Expr::Variable(var, var_type) => Expr::Variable(var.clone(), var_type.apply(s)),
            Expr::Record(record) => Expr::Record(
                record
                    .iter()
                    .map(|(var, val)| (var.clone(), val.apply(s)))
                    .collect(),
            ),
            Expr::Tuple(tuple) => Expr::Tuple(tuple.iter().map(|val| val.apply(s)).collect()),
            Expr::Constructor(cons, cons_type) => {
                Expr::Constructor(cons.clone(), cons_type.apply(s))
            }
            Expr::If(pred, cons, altr) => Expr::If(
                Box::new(pred.apply(s)),
                Box::new(cons.apply(s)),
                Box::new(altr.apply(s)),
            ),
            Expr::Block(block) => {
                Expr::Block(block.iter().map(|statement| statement.apply(s)).collect())
            }
            Expr::Match(_, _) => todo!(),
        }
    }
}

impl Apply for Statement<Type> {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Statement::Let(pattern, body, pattern_type) => {
                Statement::Let(pattern.clone(), body.apply(s), pattern_type.apply(s))
            }
            Statement::Raw(expr) => Statement::Raw(expr.apply(s)),
            Statement::TypeDef(name, tvs, body) => {
                Statement::TypeDef(name.clone(), tvs.clone(), body.clone())
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct KindError {
    got_type: Type,
    type_kind: Kind,
    expected_kind: Kind,
}

/// this is a 100% unironic function name.
/// all_star checks if all of the kinds in an iterator are *
fn all_star<'a>(i: impl Iterator<Item = &'a Type>) -> Result<(), KindError> {
    for t in i {
        let k = Kind::check(&t)?;
        if k != Kind::Star {
            return Err(KindError {
                got_type: t.clone(),
                type_kind: k,
                expected_kind: Kind::Star,
            });
        }
    }
    Ok(())
}

impl Kind {
    /// infer the kind of a type, returning a KindError if the type is invalid
    pub fn check(t: &Type) -> Result<Kind, KindError> {
        match t {
            Type::Variable(var, kind) => Ok(kind.clone()),
            Type::Constant(var, kind) => Ok(kind.clone()),
            Type::Application(left, right) => match Kind::check(left)? {
                Kind::Star => Err(KindError {
                    got_type: left.as_ref().clone(),
                    type_kind: Kind::Star,
                    expected_kind: Kind::Arrow(Box::new(Kind::check(right)?), Box::new(Kind::Star)),
                }),
                Kind::Arrow(k1, k2) => {
                    let k3 = Kind::check(right)?;
                    if &k3 == k1.as_ref() {
                        Ok(*k2)
                    } else {
                        Err(KindError {
                            got_type: left.as_ref().clone(),
                            type_kind: Kind::Arrow(k1, k2),
                            expected_kind: Kind::Arrow(Box::new(k3), Box::new(Kind::Star)),
                        })
                    }
                }
            },
            Type::Tuple(tuple) => {
                let () = all_star(tuple.iter())?;
                Ok(Kind::Star)
            }
            Type::Record(record) => {
                let () = all_star(record.values())?;
                Ok(Kind::Star)
            }
            Type::Arrow(left, right) => {
                let () = all_star(vec![left.as_ref(), right.as_ref()].into_iter())?;
                Ok(Kind::Star)
            }
        }
    }
}

#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
pub enum TypeError {
    InfiniteType(Type, Type),
    Kind(KindError),
    KindMismatch(Type, Type),
    ConstructorMismatch(Var, Var),
    TypeMismatch(Type, Type),
    MissingField(String),
    UnknownVariant(String),
    NoSolvableConstraints,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constant(s, _) | Type::Variable(s, _) => s.fmt(f),
            Type::Arrow(left, right) => write!(f, "({} -> {})", left, right),
            Type::Tuple(tuple) => {
                write!(f, "(")?;
                for val in tuple {
                    write!(f, "{},", val)?;
                }
                write!(f, ")")
            }
            Type::Record(record) => {
                write!(f, "{{")?;
                for (var, val) in record {
                    write!(f, "{}: {},", var, val)?;
                }
                write!(f, "}}")
            }
            Type::Application(left, right) => write!(f, "({} {})", left, right),
        }
    }
}
