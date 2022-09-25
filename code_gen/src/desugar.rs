use crate::variable::*;
use expr::expr::Pattern;
use expr::kind::Kind;
use expr::types::{self, Var};
use im::HashMap;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Binding {
    variable: Variable,
    value: Expr,
}

// An expression with no lexical overlap at the expression or type level
// and explicit type arguments
#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Function(Vec<Variable>, Box<Expr>),
    Tuple(Tag, Rc<TypeDefinition>, Vec<Expr>),
    TupleIndex(Box<Expr>, usize),
    Specialize(Variable, Vec<Identifier>),
    Variable(Variable),
    Literal(Literal),
    Switch(Box<Expr>, Vec<(Tag, Expr)>),
    CallFunction(Box<Expr>, Vec<Expr>),
    CallBuiltin(Builtin, Vec<Expr>),
    Let(Vec<Identifier>, Box<Binding>, Box<Expr>),
}

#[derive(Clone)]
pub struct SpecializeTemplate {
    polymorphic_type: Type,
    type_arguments: Vec<Identifier>,
}

#[derive(Clone, Default)]
pub struct VariableEnvironment<'a> {
    variable_bindings: HashMap<&'a str, Variable>,
    type_variable_bindings: HashMap<Var, Identifier>,
    polymorphic_variables: HashMap<Identifier, SpecializeTemplate>,
}

impl<'a> VariableEnvironment<'a> {
    /// Takes the irrefutable pattern `pattern` and expands it out into a series of tuple accesses,
    /// updating the `VariableEnvironment` as it goes.
    pub fn bind_pattern(
        &self,
        value: Variable,
        pattern: &'a Pattern<types::Type<Kind>>,
    ) -> (VariableEnvironment<'a>, Vec<Binding>) {
        match pattern {
            Pattern::Variable(variable, _) => (self.bind_variable(variable, value), Vec::new()),
            Pattern::Record(_) => todo!(),
            Pattern::Tuple(_) => todo!(),
            Pattern::Construction(_, _, _) => todo!(),
        }
    }

    pub fn bind_variable(
        &self,
        old_variable: &'a str,
        new_variable: Variable,
    ) -> VariableEnvironment<'a> {
        VariableEnvironment {
            variable_bindings: self.variable_bindings.update(old_variable, new_variable),
            type_variable_bindings: self.type_variable_bindings.clone(),
            polymorphic_variables: self.polymorphic_variables.clone(),
        }
    }

    pub fn type_variable_binding(&self, variable: &Var) -> Option<Identifier> {
        self.type_variable_bindings.get(variable).copied()
    }

    pub fn lookup_variable(&self, variable_name: &'a str) -> Expr {
        let desugared_variable = self.variable_bindings.get(variable_name).unwrap();
        let template = self
            .polymorphic_variables
            .get(&desugared_variable.identifier());

        if let Some(template) = template {
            todo!()
        } else {
            Expr::Variable(desugared_variable.clone())
        }
    }
}

#[derive(Default)]
pub struct VariableSource(IdentifierSource);

impl VariableSource {
    pub fn fresh(&mut self, variable_type: Type) -> Variable {
        Variable::new(self.0.fresh(), variable_type)
    }
}

pub fn desugar_type(r#type: &types::Type<Kind>, environment: VariableEnvironment) -> Type {
    match r#type {
        types::Type::Constant(constant, _) => {
            if constant.as_ref() == "Num" {
                Type::Primitive(Primitive::Number)
            } else {
                panic!("unknown type constant")
            }
        }
        types::Type::Variable(variable, _) => {
            if let Some(desugared_variable) = environment.type_variable_binding(&variable) {
                Type::Variable(desugared_variable)
            } else {
                // if types are unknown, we are dealing with a polymorphic function that is never called, so use the unmakable void type
                Type::Primitive(Primitive::Void)
            }
        }
        types::Type::Arrow(argument, return_type) => Type::Function(
            vec![desugar_type(argument, environment.clone())],
            Box::new(desugar_type(return_type, environment)),
        ),
        types::Type::Tuple(_) => todo!(),
        types::Type::Record(_) => todo!(),
        types::Type::Application(_, _, _) => todo!(),
    }
}

pub fn desugar_expr<'a>(
    expr: &'a expr::expr::Expr<types::Type<Kind>>,
    variable_source: &mut VariableSource,
    environment: VariableEnvironment<'a>,
) -> Expr {
    match expr {
        expr::expr::Expr::Function(pattern, body, _) => {
            let argument =
                variable_source.fresh(desugar_type(&pattern.get_type(), environment.clone()));
            let (body_environment, bindings) = environment.bind_pattern(argument.clone(), pattern);
            let mut desugared_body = desugar_expr(body.as_ref(), variable_source, body_environment);
            for binding in bindings.into_iter().rev() {
                desugared_body = Expr::Let(Vec::new(), Box::new(binding), Box::new(desugared_body));
            }
            Expr::Function(vec![argument], Box::new(desugared_body))
        }
        expr::expr::Expr::Application(function, argument, _) => {
            let desugared_function =
                desugar_expr(function.as_ref(), variable_source, environment.clone());
            let desugared_argument = desugar_expr(argument.as_ref(), variable_source, environment);
            Expr::CallFunction(Box::new(desugared_function), vec![desugared_argument])
        }
        expr::expr::Expr::Variable(variable_name, _) => {
            // consult the env, instatiating the variable if necessary
            environment.lookup_variable(variable_name)
        }
        expr::expr::Expr::Number(number) => Expr::Literal(Literal::Number(*number)),
        expr::expr::Expr::Block(_) => todo!(),
        expr::expr::Expr::Boolean(_) => todo!(),
        expr::expr::Expr::Operator(_, _) => todo!(),
        expr::expr::Expr::Record(_) => todo!(),
        expr::expr::Expr::If(_, _, _) => todo!(),
        expr::expr::Expr::Tuple(_) => todo!(),
        expr::expr::Expr::Constructor(_, _) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
    }
}
