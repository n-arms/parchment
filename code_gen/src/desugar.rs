use expr::{
    expr::{Operator, Pattern},
    kind::Kind,
    types::{Fresh, Type, TypeDef},
};

use im::HashMap;

/// the main function of `desugar::Expr` is to make unique names and isolate the complexities of
/// compiling pattern matching

pub struct Variable {
    var: usize,
    var_type: Type<Kind>,
}

pub struct Constructor {
    id: usize,
}

pub enum Expr {
    Function(Variable, Box<Expr>),
    Application(Box<Expr>, Box<Expr>, Type<Kind>),
    Variable(Variable),
    Number(f64),
    Boolean(bool),
    Operator(Operator),
    Record(HashMap<Variable, Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Switch(Box<Expr>, Vec<(Constructor, Expr)>),
}
