use core::slice;
use std::rc::Rc;

use crate::variable::*;
use expr::kind::Kind;
use expr::types;
use im::HashMap;

#[derive(Debug)]
pub enum Expr<'e, 't> {
    Function(&'e [Variable<'t>], &'e Expr<'e, 't>),
    CallFunction(&'e Expr<'e, 't>, &'e [Expr<'e, 't>], &'t Type<'t>),
    CallBuiltin(Builtin, &'e [Expr<'e, 't>]),
    Literal(Literal),
    Tuple(Tag, &'t TypeDefinition<'t>, &'e [Expr<'e, 't>]),
    Variable(Variable<'t>),
    Let(&'e [Identifier], Binding<'e, 't>, &'e Expr<'e, 't>),
}

#[derive(Debug)]
pub struct Binding<'e, 't> {
    pub variable: Variable<'t>,
    pub value: &'e Expr<'e, 't>,
}

#[derive(Clone, Debug, Default)]
pub struct Environment<'t, 'a> {
    variable_bindings: HashMap<&'a str, Variable<'t>>,
    type_variable_bindings: HashMap<Rc<String>, Identifier>
}


pub fn desugar_expr<'e, 't, 'a>(
    expr: &'a expr::expr::Expr<types::Type<Kind>>,
    env: Environment<'t, 'a>,
    variables: &mut VariableSource,
    arenas: Allocator<'e, 't>,
) -> Expr<'e, 't> {
    match expr {
        expr::expr::Expr::Function(pattern, body, _) => {
            let argument = variables.fresh(env.desugar_type(&pattern.get_type(), arenas));
            let (env, preamble) = env.bind_pattern(pattern, argument, variables, arenas);
            let mut desugared_body = desugar_expr(body, env, variables, arenas);

            for binding in preamble.into_iter().rev() {
                desugared_body = Expr::Let(&[], binding, arenas.alloc_expr(desugared_body));
            }

            let arguments = arenas.alloc_expr_iter([argument].into_iter());

            Expr::Function(arguments, arenas.alloc_expr(desugared_body))
        }
        expr::expr::Expr::Application(function, argument, result_type) => {
            let desugared_function =
                desugar_expr(function.as_ref(), env.clone(), variables, arenas);
            let desugared_argument =
                desugar_expr(argument.as_ref(), env.clone(), variables, arenas);

            let arguments = arenas.alloc_expr_iter([desugared_argument].into_iter());

            Expr::CallFunction(
                arenas.alloc_expr(desugared_function),
                arguments,
                env.desugar_type(result_type, arenas),
            )
        }
        expr::expr::Expr::Number(number) => Expr::Literal(Literal::Number(*number)),
        expr::expr::Expr::Boolean(boolean) => Expr::Literal(Literal::Boolean(*boolean)),
        expr::expr::Expr::Operator(operator, _) => {
            let arg_a = variables.fresh(env.desugar_type(&operator.argument_type(), arenas));
            let args_a = slice::from_ref(arenas.alloc_expr(arg_a));
            let arg_b = variables.fresh(env.desugar_type(&operator.argument_type(), arenas));
            let args_b = slice::from_ref(arenas.alloc_expr(arg_b));

            let builtin_args =
                arenas.alloc_expr_iter([arg_a, arg_b].into_iter().map(Expr::Variable));

            let body_b = Expr::Function(
                args_b,
                arenas.alloc_expr(Expr::CallBuiltin(
                    Builtin::Operator(*operator),
                    builtin_args,
                )),
            );

            Expr::Function(args_a, arenas.alloc_expr(body_b))
        }
        expr::expr::Expr::Variable(variable, variable_type) => {
            let id = env.lookup_variable(variable).id;
            Expr::Variable(Variable {
                id,
                variable_type: env.desugar_type(variable_type, arenas)
            })
        }
        expr::expr::Expr::Block(block) => desugar_block(block.as_slice(), env, variables, arenas),
        expr::expr::Expr::Record(_) => todo!(),
        expr::expr::Expr::If(_, _, _) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
        expr::expr::Expr::Tuple(_) => todo!(),
        expr::expr::Expr::Constructor(_, _) => todo!(),
    }
}

fn desugar_block<'e, 't, 'a>(
    block: &'a [expr::expr::Statement<types::Type<Kind>>],
    env: Environment<'t, 'a>,
    variables: &mut VariableSource,
    arenas: Allocator<'e, 't>,
) -> Expr<'e, 't> {
    match block {
        [expr::expr::Statement::Raw(expr)] => desugar_expr(expr, env, variables, arenas),
        [expr::expr::Statement::Raw(expr), rest @ ..] => {
            let raw_variable = variables.fresh(env.desugar_type(&expr.get_type(), arenas));
            let raw_binding = Binding {
                variable: raw_variable,
                value: arenas.alloc_expr(desugar_expr(expr, env.clone(), variables, arenas)),
            };

            Expr::Let(
                &[],
                raw_binding,
                arenas.alloc_expr(desugar_block(rest, env, variables, arenas)),
            )
        }
        [expr::expr::Statement::Let(pattern, expr, _), rest @ ..] => {
            let (env, type_arguments) =
                env.qualify_type_vars(expr.get_type().type_vars(), variables, arenas);

            let expr_type = env.desugar_type(&expr.get_type(), arenas);
            let binding_variable = variables.fresh(expr_type);

            let (env, bindings) = env.bind_pattern(pattern, binding_variable, variables, arenas);

            let mut desugared_rest = desugar_block(rest, env.clone(), variables, arenas);

            for binding in bindings.into_iter().rev() {
                desugared_rest = Expr::Let(
                    type_arguments,
                    binding,
                    arenas.alloc_expr(desugared_rest)
                );
            }

            let desugared_expr = desugar_expr(expr, env.clone(), variables, arenas);

            Expr::Let(
                type_arguments,
                Binding {
                    variable: binding_variable,
                    value: arenas.alloc_expr(desugared_expr),
                },
                arenas.alloc_expr(desugared_rest),
            )
        }
        [expr::expr::Statement::TypeDef(..), rest @ ..] => {
            desugar_block(rest, env, variables, arenas)
        }
        [] => panic!("Block should always end with an expression"),
    }
}

impl<'t, 'a> Environment<'t, 'a> {
    fn desugar_type<'e>(
        &self,
        r#type: &types::Type<Kind>,
        arenas: Allocator<'e, 't>,
    ) -> &'t Type<'t> {
        match r#type {
            types::Type::Constant(constant, _) => match constant.as_str() {
                "Num" => arenas.alloc_type(Type::Primitive(Primitive::Number)),
                _ => panic!("unknown type constant {}", constant),
            },
            types::Type::Variable(variable, _) => {
                let desugared_variable = self.type_variable_bindings.get(variable);
                let desugared_type = if let Some(desugared_variable) = desugared_variable {
                    Type::Generic(*desugared_variable)
                } else {
                    Type::Primitive(Primitive::Void)
                };
                arenas.alloc_type(desugared_type)
            }
            types::Type::Arrow(domain, range) => {
                let desugared_domain = self.desugar_type(&domain, arenas);
                let desugared_range = self.desugar_type(&range, arenas);

                arenas.alloc_type(Type::Function(
                    slice::from_ref(desugared_domain),
                    desugared_range,
                ))
            }
            types::Type::Tuple(_) => todo!(),
            types::Type::Record(_) => todo!(),
            types::Type::Application(_, _, _) => todo!(),
        }
    }

    fn bind_pattern<'e>(
        &self,
        pattern: &'a expr::expr::Pattern<types::Type<Kind>>,
        pattern_variable: Variable<'t>,
        variables: &mut VariableSource,
        arenas: Allocator<'e, 't>,
    ) -> (Environment<'t, 'a>, Vec<Binding<'e, 't>>) {
        match pattern {
            expr::expr::Pattern::Variable(variable, _) => {
                (self.bind_variable(variable, pattern_variable), Vec::new())
            }
            expr::expr::Pattern::Record(_) => todo!(),
            expr::expr::Pattern::Tuple(_) => todo!(),
            expr::expr::Pattern::Construction(_, _, _) => todo!(),
        }
    }

    fn lookup_variable(&self, variable: &str) -> Variable<'t> {
        *self.variable_bindings.get(variable).unwrap()
    }

    fn bind_variable(
        &self,
        variable: &'a str,
        desugared_variable: Variable<'t>,
    ) -> Environment<'t, 'a> {
        let mut env = self.clone();
        env.variable_bindings.insert(variable, desugared_variable);
        env
    }

    fn qualify_type_vars<'e>(
        &self,
        type_vars: im::HashSet<std::rc::Rc<String>>,
        variables: &mut VariableSource,
        arenas: Allocator<'e, 't>,
    ) -> (Environment<'t, 'a>, &'e [Identifier]) {
        if type_vars.is_empty() {
            (self.clone(), &[])
        } else {
            let mut env = self.clone();
            let type_arguments = arenas.alloc_expr_iter(type_vars
                .into_iter()
                .map(|type_var| {
                    let id = variables.fresh_id();
                    env.type_variable_bindings.insert(type_var, id);
                    id
                })
            );
            (env, type_arguments)
        }
    }
}

impl<'t> Typeable<'t> for Expr<'_, 't> {
    fn get_type(&self, arenas: Allocator<'_, 't>) -> Type<'t> {
        match self {
            Expr::Function(arguments, body) => {
                Type::Function(arenas.alloc_type_iter(arguments.iter().map(|arg| arg.get_type(arenas))), arenas.alloc_type(body.get_type(arenas)))
            }
            Expr::CallFunction(_, _, result_type) => (*result_type).clone(),
            Expr::CallBuiltin(builtin, _) => builtin.get_type(arenas),
            Expr::Literal(literal) => literal.get_type(arenas),
            Expr::Variable(variable) => variable.get_type(arenas),
            Expr::Let(_, _, body) => body.get_type(arenas),
            Expr::Tuple(_, _, _) => todo!(),
        }
    }
}

impl<'t> Typeable<'t> for Builtin {
    fn get_type(&self, arenas: Allocator<'_, 't>) -> Type<'t> {
        match self {
            Builtin::Operator(operator) => Type::Primitive(Primitive::Number)
        }
    }
}

impl<'t> Typeable<'t> for Literal {
    fn get_type(&self, arenas: Allocator<'_, 't>) -> Type<'t> {
        match self {
            Literal::Number(_) => Type::Primitive(Primitive::Number),
            Literal::Boolean(_) => todo!(),
        }
    }
}
