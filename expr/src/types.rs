use super::expr::{Expr, Pattern, Statement};
use super::kind::{KindError, Kind};
use im::{HashMap, HashSet};
use std::cell::Cell;
use std::fmt::{Debug, Display};
use std::hash::Hash;
use std::rc::Rc;

pub type Var = Rc<String>;

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<K: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash> {
    Constant(Var, K),
    Variable(Var, K),
    Arrow(Rc<Type<K>>, Rc<Type<K>>),
    Tuple(Vec<Type<K>>),
    Record(HashMap<String, Type<K>>),
    Application(Rc<Type<K>>, Rc<Type<K>>, K),
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant<K: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash> {
    pub constructor: String,
    pub fields: Vec<Type<K>>,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDef<K: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash> {
    pub polymorphic_vars: Vec<(Var, K)>,
    pub variants: HashSet<Variant<K>>,
}

impl Type<Kind> {
    pub fn get_kind(&self) -> Kind {
        match self {
            Type::Application(_, _, k) | Type::Variable(_, k) | Type::Constant(_, k) => k.clone(),
            Type::Tuple(_) | Type::Record(_) | Type::Arrow(_, _) => Kind::default(),
        }
    }
}

impl<K: Clone + Debug + PartialEq + Eq + PartialOrd + Ord + Hash> Type<K> {
    pub fn variables(&self) -> HashSet<Var> {
        match self {
            Type::Constant(_, _) => HashSet::new(),
            Type::Variable(var, _) => HashSet::unit(Rc::clone(var)),
            Type::Application(left, right, _) | Type::Arrow(left, right) => {
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

pub fn num_type() -> Type<Kind> {
    Type::Constant(Rc::new(String::from("Num")), Kind::default())
}

pub fn bool_type() -> Type<Kind> {
    Type::Constant(Rc::new(String::from("Bool")), Kind::default())
}

pub fn unit_type() -> Type<Kind> {
    Type::Constant(Rc::new(String::from("Unit")), Kind::default())
}

pub type Substitution = HashMap<Var, Type<Kind>>;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Constraint {
    /// t1 == t2
    Equality(Type<Kind>, Type<Kind>),
    /// t1 is an instance of t2
    InstanceOf(Type<Kind>, HashSet<Var>, Type<Kind>),
}

pub trait Apply {
    fn apply(&self, s: &Substitution) -> Self;
}

impl Apply for Type<Kind> {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Type::Constant(_, _) => self.clone(),
            Type::Variable(var, ..) => {
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
            Type::Application(left, right, kind) => Type::Application(
                Rc::new(left.apply(s)),
                Rc::new(right.apply(s)),
                kind.clone(),
            ),
        }
    }
}

impl Apply for Constraint {
    fn apply(&self, s: &Substitution) -> Self {
        match self {
            Self::Equality(left, right) => Self::Equality(left.apply(s), right.apply(s)),
            Self::InstanceOf(sub_type, m, super_type) => Self::InstanceOf(
                sub_type.apply(s),
                m.iter()
                    .map(|v| Type::Variable(Rc::clone(v), Kind::default()).apply(s))
                    .flat_map(|t| t.variables())
                    .collect(),
                super_type.apply(s),
            ),
        }
    }
}

impl Apply for Expr<Type<Kind>> {
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
            Expr::Match(matchand, arms, match_type) => Expr::Match(
                Box::new(matchand.apply(s)),
                arms.iter()
                    .map(|(pat, expr)| (pat.clone(), expr.apply(s)))
                    .collect(),
                match_type.apply(s),
            ),
        }
    }
}

impl Apply for Statement<Type<Kind>> {
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

//#[derive(Clone, Debug, PartialOrd, Ord, PartialEq, Eq, Hash)]
#[derive(Clone, Debug)]
pub enum TypeError {
    InfiniteType(Type<Kind>, Type<Kind>),
    Kind(KindError),
    KindMismatch(Type<Kind>, Type<Kind>),
    ConstructorMismatch(Var, Var),
    TypeMismatch(Type<Kind>, Type<Kind>),
    MissingField(String),
    UnknownVariant(String),
    RefutablePattern(Pattern<()>),
    FieldMismatch(Variant<Kind>, Pattern<()>),
    NoSolvableConstraints,
}

impl<K: Debug + Clone + PartialEq + Eq + PartialOrd + Ord + Hash> Display for Type<K> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Constant(s, _) => write!(f, "`{}`", s),
            Type::Variable(s, _) => write!(f, "{}", s),
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
            Type::Application(left, right, _) => write!(f, "({} {})", left, right),
        }
    }
}

impl From<KindError> for TypeError {
    fn from(k: KindError) -> Self {
        TypeError::Kind(k)
    }
}
