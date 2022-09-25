use crate::variable::*;
use im::HashMap;
use std::rc::Rc;

/// the main function of `desugar::Expr` is to make unique names and isolate the complexities of
/// compiling pattern matching

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
    main: Expr,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Closure(FunctionPointer, Vec<Expr>),
    Tuple(Tag, Rc<TypeDefinition>, Vec<Expr>),
    Variable(Variable),
    Literal(Literal),
    Switch(Box<Expr>, Vec<(Tag, Expr)>),
    CallClosure(Box<Expr>, Vec<Expr>),
    CallBuiltin(Builtin, Vec<Expr>),
    Let(Variable, Box<Expr>, Box<Expr>),
}
