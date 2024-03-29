use std::fmt::{Debug, Formatter};
use bumpalo::Bump;

use expr::expr::Operator;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'t> {
    Generic(Identifier),
    Tuple(&'t TypeDefinition<'t>, &'t [Type<'t>]),
    Primitive(Primitive),
    Function(&'t [Type<'t>], &'t Type<'t>),
}

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
pub struct Variant<'t> {
    pub tag: Tag,
    pub arguments: &'t [Type<'t>],
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TypeDefinition<'t> {
    pub variants: &'t [Variant<'t>],
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Primitive {
    Number,
    Void,
    Type
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    id: usize,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable<'t> {
    pub id: Identifier,
    pub variable_type: &'t Type<'t>,
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
    pub fn fresh<'t>(&mut self, variable_type: &'t Type<'t>) -> Variable<'t> {
        Variable {
            id: self.fresh_id(),
            variable_type,
        }
    }

    pub fn fresh_id(&mut self) -> Identifier {
        self.0.fresh()
    }
}

impl Debug for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.id)
    }
}

impl Debug for Variable<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} : {:?}", self.id, self.variable_type)
    }
}
impl Identifier {
    fn new(id: usize) -> Identifier {
        Identifier { id }
    }
}
#[derive(Copy, Clone)]
pub struct Allocator<'e, 't> {
    expr_arena: &'e Bump,
    type_arena: &'t Bump,
}

impl<'e, 't> Allocator<'e, 't> {
    pub fn new(expr_arena: &'e Bump, type_arena: &'t Bump) -> Self {
        Allocator {
            expr_arena,
            type_arena,
        }
    }

    pub fn alloc_type<T>(&self, value: T) -> &'t mut T {
        self.type_arena.alloc(value)
    }

    pub fn alloc_expr<T>(&self, value: T) -> &'e mut T {
        self.expr_arena.alloc(value)
    }

    pub fn alloc_expr_iter<T>(
        &self,
        iter: impl Iterator<Item = T> + ExactSizeIterator,
    ) -> &'e mut [T] {
        self.expr_arena.alloc_slice_fill_iter(iter)
    }

    pub fn alloc_type_iter<T>(
        &self,
        iter: impl Iterator<Item = T> + ExactSizeIterator
    ) -> &'t mut [T] {
        self.type_arena.alloc_slice_fill_iter(iter)
    }
}

pub trait Typeable<'t> {
    fn get_type(&self, arenas: Allocator<'_, 't>) -> Type<'t>;
}

impl<'t> Typeable<'t> for Variable<'t> {
    fn get_type(&self, arenas: Allocator<'_, 't>) -> Type<'t> {
        self.variable_type.clone()
    }
}
