use expr::expr::Operator;
use im::HashMap;
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tag {
    tag: usize,
}

impl Tag {
    pub fn new(tag: usize) -> Tag {
        Tag { tag }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variant {
    pub tag: Tag,
    pub arguments: Vec<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefinition {
    pub variants: Vec<Variant>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primitive {
    Number,
    Void,
}

pub fn boolean_type() -> Type {
    Type::Tuple(Rc::new(TypeDefinition {
        variants: vec![
            Variant {
                tag: Tag::new(0),
                arguments: Vec::new(),
            },
            Variant {
                tag: Tag::new(1),
                arguments: Vec::new(),
            },
        ],
    }))
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type {
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>),
    Tuple(Rc<TypeDefinition>),
    Primitive(Primitive),
    Type,
}

impl Type {
    pub fn unify(general: Type, specialized: Type) -> HashMap<Identifier, Type> {
        match (general, specialized) {
            (Type::Variable(id), specialized) => HashMap::unit(id, specialized),
            (
                Type::Function(general_arguments, general_return),
                Type::Function(specialized_arguments, specialized_return),
            ) => {
                let mut unification = HashMap::new();
                let arguments = general_arguments
                    .into_iter()
                    .zip(specialized_arguments.into_iter());
                for (general_argument, specialized_argument) in arguments {
                    unification.extend(Type::unify(general_argument, specialized_argument));
                }
                unification.extend(Type::unify(*general_return, *specialized_return));
                unification
            }
            (Type::Primitive(_), Type::Primitive(_)) => HashMap::new(),
            (Type::Type, Type::Type) => HashMap::new(),
            (Type::Tuple(general_type_definition), Type::Tuple(specialized_type_definition)) => {
                let mut unification = HashMap::new();
                let fields = general_type_definition
                    .variants
                    .iter()
                    .cloned()
                    .zip(specialized_type_definition.variants.iter().cloned());
                for (general_variant, specialized_variant) in fields {
                    unification.extend(Variant::unify(general_variant, specialized_variant));
                }
                unification
            }
            (general, specialized) => {
                panic!("cannot unify types {:?} and {:?}", general, specialized)
            }
        }
    }
}

impl Variant {
    pub fn unify(general: Self, specialized: Self) -> HashMap<Identifier, Type> {
        assert_eq!(general.tag, specialized.tag);

        let mut unification = HashMap::new();
        let fields = general
            .arguments
            .into_iter()
            .zip(specialized.arguments.into_iter());

        for (general_field, specialized_field) in fields {
            unification.extend(Type::unify(general_field, specialized_field));
        }

        unification
    }
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
    Boolean(bool),
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

#[derive(Default)]
pub struct VariableSource(IdentifierSource);

impl VariableSource {
    pub fn fresh(&mut self, variable_type: Type) -> Variable {
        Variable::new(self.0.fresh(), variable_type)
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

pub trait Typeable {
    fn get_type(&self) -> Type;
}

impl Typeable for Variable {
    fn get_type(&self) -> Type {
        self.variable_type.clone()
    }
}

impl Builtin {
    pub fn result_type(&self) -> Type {
        match self {
            Builtin::Operator(operator) => match operator {
                Operator::Plus | Operator::Minus | Operator::Times => {
                    Type::Primitive(Primitive::Number)
                }
                Operator::And
                | Operator::Or
                | Operator::Equals
                | Operator::LessThan
                | Operator::LessThanEqual
                | Operator::GreaterThan
                | Operator::GreaterThanEqual => boolean_type(),
            },
        }
    }
}