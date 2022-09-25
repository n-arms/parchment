use crate::variable::*;
use std::rc::Rc;

// An expression with no lexical overlap at the expression or type level
// and explicit type arguments
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Function(Vec<Variable>, Box<Expr>),
    Tuple(Tag, Rc<TypeDefinition>, Vec<Expr>),
    Specialized(Variable, Vec<Variable>),
    Variable(Variable),
    Literal(Literal),
    Switch(Box<Expr>, Vec<(Tag, Expr)>),
    CallFunction(Box<Expr>, Vec<Expr>),
    CallBuiltin(Builtin, Vec<Expr>),
    Let(Vec<Identifier>, Variable, Box<Expr>, Box<Expr>),
}
