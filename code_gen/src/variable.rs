use expr::expr::Operator;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag {
    tag: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    tag: Tag,
    arguments: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefinition {
    variants: Vec<Variant>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primitive {
    Number,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>),
    Tuple(Rc<TypeDefinition>, Vec<Type>),
    Primitive(Primitive),
    Type,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    id: usize,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    var: Identifier,
    var_type: Type,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Builtin {
    Operator(Operator),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.id)
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.var, self.var_type)
    }
}
