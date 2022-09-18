use expr::expr::{Operator, Pattern};

use im::HashMap;
use std::rc::Rc;

/// the main function of `desugar::Expr` is to make unique names and isolate the complexities of
/// compiling pattern matching

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
pub enum Type {
    Variable(Identifier),
    Function(Vec<Type>, Box<Type>),
    Tuple(Rc<TypeDefinition>, Vec<Type>),
    Type,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Identifier {
    id: usize,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Variable {
    var: Identifier,
    var_type: Type,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionPointer {
    id: Identifier,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinition {
    arguments: Vec<Variable>,
    body: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Program {
    definitions: HashMap<FunctionPointer, FunctionDefinition>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Literal {
    Number(f64),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Builtin {
    Operator(Operator),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Function(FunctionPointer),
    Tuple(Tag, Vec<Expr>),
    Variable(Variable),
    Literal(Literal),
    Switch(Box<Expr>, Vec<(Tag, Expr)>),
    CallClosure(Box<Expr>, Vec<Expr>),
    CallBuiltin(Builtin, Vec<Expr>),
}
