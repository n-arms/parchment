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
    Void,
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

impl Identifier {
    pub fn new(id: usize) -> Identifier {
        Identifier { id }
    }
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    variable_name: Identifier,
    variable_type: Type,
}

impl Variable {
    pub fn identifier(&self) -> Identifier {
        self.variable_name
    }

    pub fn new(variable_name: Identifier, variable_type: Type) -> Variable {
        Variable {
            variable_name,
            variable_type,
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Builtin {
    Operator(Operator),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
}

#[derive(Default)]
pub struct IdentifierSource {
    highest: usize,
}

impl IdentifierSource {
    pub fn new(highest: usize) -> Self {
        IdentifierSource { highest }
    }

    pub fn fresh(&mut self) -> Identifier {
        let id = self.highest;
        self.highest += 1;
        Identifier::new(id)
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.id)
    }
}

impl Debug for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.variable_name, self.variable_type)
    }
}
