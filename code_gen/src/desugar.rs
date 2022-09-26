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
    Specialize(Variable, Vec<Type>),
    Variable(Variable),
    Literal(Literal),
    Switch(Box<Expr>, Vec<(Tag, Expr)>),
    CallFunction(Box<Expr>, Vec<Expr>),
    CallBuiltin(Builtin, Vec<Expr>),
    Let(Vec<Identifier>, Box<Binding>, Box<Expr>),
}

#[derive(Clone, Debug)]
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
        variable_source: &mut VariableSource,
    ) -> (VariableEnvironment<'a>, Vec<Binding>) {
        match pattern {
            Pattern::Variable(variable, _) => (self.bind_variable(variable, value), Vec::new()),
            Pattern::Tuple(fields) => {
                let mut bindings = Vec::new();
                let mut inner_environment = self.clone();

                for (index, field) in fields.into_iter().enumerate() {
                    let field_value =
                        variable_source.fresh(desugar_type(&field.get_type(), self.clone()));
                    let binding = Binding {
                        variable: field_value.clone(),
                        value: Expr::TupleIndex(Box::new(Expr::Variable(value.clone())), index),
                    };
                    bindings.push(binding);
                    let (field_environment, field_bindings) =
                        inner_environment.bind_pattern(field_value, field, variable_source);
                    bindings.extend(field_bindings);
                    inner_environment = field_environment;
                }

                (inner_environment, bindings)
            }
            Pattern::Record(fields) => {
                let mut sorted_fields: Vec<_> = fields.into_iter().collect();
                sorted_fields.sort_by_key(|(field_name, _)| *field_name);
                let mut bindings = Vec::new();
                let mut inner_environment = self.clone();

                for (index, (_, field)) in sorted_fields.into_iter().enumerate() {
                    let field_value =
                        variable_source.fresh(desugar_type(&field.get_type(), self.clone()));
                    let binding = Binding {
                        variable: field_value.clone(),
                        value: Expr::TupleIndex(Box::new(Expr::Variable(value.clone())), index),
                    };
                    bindings.push(binding);
                    let (field_environment, field_bindings) =
                        inner_environment.bind_pattern(field_value, field, variable_source);
                    bindings.extend(field_bindings);
                    inner_environment = field_environment;
                }

                (inner_environment, bindings)
            }
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

    pub fn lookup_variable(&self, variable_name: &'a str, variable_type: Type) -> Expr {
        let desugared_variable = self.variable_bindings.get(variable_name).unwrap();
        let template = self
            .polymorphic_variables
            .get(&desugared_variable.identifier());

        if let Some(template) = template {
            let general_type = template.polymorphic_type.clone();
            let unification = Type::unify(general_type, variable_type);
            println!("generated unification\n\t{:#?}", unification);

            let mut type_arguments = Vec::new();

            for type_variable in &template.type_arguments {
                type_arguments.push(unification.get(type_variable).unwrap().clone());
            }

            Expr::Specialize(desugared_variable.clone(), type_arguments)
        } else {
            Expr::Variable(desugared_variable.clone())
        }
    }

    fn bind_polymorphic_variable(
        &self,
        variable_name: &'a str,
        variable: Variable,
        template: SpecializeTemplate,
    ) -> Self {
        let mut environment = self.bind_variable(variable_name, variable.clone());
        environment
            .polymorphic_variables
            .insert(variable.identifier(), template);
        environment
    }

    fn qualify_type_vars(
        &self,
        type_vars: im::HashSet<Var>,
        variable_source: &mut VariableSource,
    ) -> (Self, Vec<Identifier>) {
        let mut environment = self.clone();
        let mut qualified_vars = Vec::new();
        for type_var in type_vars {
            let variable = variable_source.fresh(Type::Type);
            environment
                .type_variable_bindings
                .insert(type_var, variable.identifier());
            qualified_vars.push(variable.identifier());
        }

        (environment, qualified_vars)
    }
}

#[derive(Default)]
pub struct VariableSource(IdentifierSource);

impl VariableSource {
    pub fn fresh(&mut self, variable_type: Type) -> Variable {
        Variable::new(self.0.fresh(), variable_type)
    }
}

fn boolean_type() -> Type {
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

pub fn desugar_type(r#type: &types::Type<Kind>, environment: VariableEnvironment) -> Type {
    match r#type {
        types::Type::Constant(constant, _) => match constant.as_str() {
            "Num" => Type::Primitive(Primitive::Number),
            "Bool" => boolean_type(),
            _ => panic!("unknown type constant"),
        },
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
        types::Type::Tuple(fields) => {
            let desugared_fields: Vec<_> = fields
                .into_iter()
                .map(|field| desugar_type(field, environment.clone()))
                .collect();
            let type_definition = TypeDefinition {
                variants: vec![Variant {
                    tag: Tag::new(0),
                    arguments: desugared_fields,
                }],
            };
            Type::Tuple(Rc::new(type_definition))
        }
        types::Type::Record(fields) => {
            let mut sorted_fields: Vec<_> = fields.into_iter().collect();
            sorted_fields.sort_by_key(|(field_name, _)| *field_name);
            let desugared_fields: Vec<_> = sorted_fields
                .into_iter()
                .map(|(_, field)| desugar_type(field, environment.clone()))
                .collect();
            let type_definition = TypeDefinition {
                variants: vec![Variant {
                    tag: Tag::new(0),
                    arguments: desugared_fields,
                }],
            };
            Type::Tuple(Rc::new(type_definition))
        }
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
            let (body_environment, bindings) =
                environment.bind_pattern(argument.clone(), pattern, variable_source);
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
        expr::expr::Expr::Variable(variable_name, variable_type) => {
            // consult the env, instatiating the variable if necessary
            environment.lookup_variable(
                variable_name,
                desugar_type(variable_type, environment.clone()),
            )
        }
        expr::expr::Expr::Number(number) => Expr::Literal(Literal::Number(*number)),
        expr::expr::Expr::Boolean(boolean) => Expr::Literal(Literal::Boolean(*boolean)),
        expr::expr::Expr::Block(block) => {
            desugar_block(block.as_slice(), variable_source, environment)
        }
        expr::expr::Expr::Tuple(fields) => {
            desugar_tuple(fields.into_iter(), variable_source, environment)
        }
        expr::expr::Expr::Operator(operator, _) => {
            let argument_type = desugar_type(&operator.argument_type(), environment.clone());
            let left_argument = variable_source.fresh(argument_type.clone());
            let right_argument = variable_source.fresh(argument_type);
            Expr::Function(
                vec![left_argument.clone()],
                Box::new(Expr::Function(
                    vec![right_argument.clone()],
                    Box::new(Expr::CallBuiltin(
                        Builtin::Operator(*operator),
                        vec![
                            Expr::Variable(left_argument),
                            Expr::Variable(right_argument),
                        ],
                    )),
                )),
            )
        }
        expr::expr::Expr::Record(fields) => {
            let mut sorted_fields: Vec<_> = fields.into_iter().collect();
            sorted_fields.sort_by_key(|(field_name, _)| *field_name);
            desugar_tuple(
                sorted_fields.into_iter().map(|(_, field)| field),
                variable_source,
                environment,
            )
        }
        expr::expr::Expr::If(predicate, branch_if, branch_else) => {
            let desugared_predicate =
                desugar_expr(predicate.as_ref(), variable_source, environment.clone());
            let desugared_branch_if =
                desugar_expr(branch_if.as_ref(), variable_source, environment.clone());
            let desugared_branch_else =
                desugar_expr(branch_else.as_ref(), variable_source, environment.clone());

            Expr::Switch(
                Box::new(desugared_predicate),
                vec![
                    (Tag::new(1), desugared_branch_if),
                    (Tag::new(0), desugared_branch_else),
                ],
            )
        }
        expr::expr::Expr::Constructor(_, _) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
    }
}

fn desugar_tuple<'a>(
    fields: impl Iterator<Item = &'a expr::expr::Expr<types::Type<Kind>>>,
    variable_source: &mut VariableSource,
    environment: VariableEnvironment<'a>,
) -> Expr {
    let mut desugared_fields = Vec::new();
    let mut type_fields = Vec::new();

    for field in fields {
        let desugared_field = desugar_expr(field, variable_source, environment.clone());
        desugared_fields.push(desugared_field);
        type_fields.push(desugar_type(&field.get_type(), environment.clone()));
    }

    let type_definition = TypeDefinition {
        variants: vec![Variant {
            tag: Tag::new(0),
            arguments: type_fields.clone(),
        }],
    };

    Expr::Tuple(Tag::new(0), Rc::new(type_definition), desugared_fields)
}

fn desugar_block<'a>(
    block: &'a [expr::expr::Statement<types::Type<Kind>>],
    variable_source: &mut VariableSource,
    environment: VariableEnvironment<'a>,
) -> Expr {
    match block {
        [expr::expr::Statement::Raw(expr)] => desugar_expr(expr, variable_source, environment),
        [expr::expr::Statement::Raw(expr), rest @ ..] => {
            let result_variable =
                variable_source.fresh(desugar_type(&expr.get_type(), environment.clone()));
            let binding = Binding {
                variable: result_variable,
                value: desugar_expr(expr, variable_source, environment.clone()),
            };
            Expr::Let(
                Vec::new(),
                Box::new(binding),
                Box::new(desugar_block(rest, variable_source, environment)),
            )
        }
        [expr::expr::Statement::Let(Pattern::Variable(variable, variable_type), expr, _), rest @ ..] =>
        {
            let (environment, type_arguments) =
                environment.qualify_type_vars(variable_type.type_vars(), variable_source);
            let expr_type = desugar_type(&expr.get_type(), environment.clone());
            let result_variable = variable_source.fresh(expr_type.clone());
            let template = SpecializeTemplate {
                polymorphic_type: expr_type,
                type_arguments: type_arguments.clone(),
            };
            let environment =
                environment.bind_polymorphic_variable(variable, result_variable.clone(), template);
            let binding = Binding {
                variable: result_variable,
                value: desugar_expr(expr, variable_source, environment.clone()),
            };
            Expr::Let(
                type_arguments,
                Box::new(binding),
                Box::new(desugar_block(rest, variable_source, environment)),
            )
        }
        [expr::expr::Statement::Let(pattern, expr, _), rest @ ..] => {
            let result_variable =
                variable_source.fresh(desugar_type(&expr.get_type(), environment.clone()));
            let (inner_environment, bindings) =
                environment.bind_pattern(result_variable.clone(), pattern, variable_source);
            let mut desugared_block = desugar_block(rest, variable_source, inner_environment);
            for binding in bindings.into_iter().rev() {
                desugared_block =
                    Expr::Let(Vec::new(), Box::new(binding), Box::new(desugared_block))
            }
            let binding = Binding {
                variable: result_variable,
                value: desugar_expr(expr, variable_source, environment),
            };
            Expr::Let(Vec::new(), Box::new(binding), Box::new(desugared_block))
        }
        [expr::expr::Statement::TypeDef(..), rest @ ..] => {
            desugar_block(rest, variable_source, environment)
        }
        [] => panic!("cannot compile a block that doesn't produce a result"),
    }
}
