use crate::variable::*;
use expr::expr::Pattern;
use expr::kind::Kind;
use expr::types::{self, Var};
use im::{HashMap, HashSet};
use std::fmt::{Debug, Formatter};
use std::rc::Rc;

#[derive(Default)]
pub struct FunctionPointerSource(IdentifierSource);

impl FunctionPointerSource {
    pub fn fresh(&mut self) -> FunctionPointer {
        FunctionPointer { id: self.0.fresh() }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionPointer {
    id: Identifier,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinition<Instr> {
    pub arguments: Vec<Variable>,
    pub body: Block<Instr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Program<Instr: Clone> {
    pub definitions: HashMap<FunctionPointer, FunctionDefinition<Instr>>,
    pub main: Block<Instr>,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Block<Instr> {
    pub instructions: Vec<Instr>,
    pub result: Variable,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr<Instr> {
    Closure(FunctionPointer, Vec<Variable>),
    Tuple(Tag, Vec<Variable>),
    TupleIndex(Variable, usize),
    Variable(Variable),
    Literal(Literal),
    Switch(Variable, Vec<(Tag, Block<Instr>)>),
    CallClosure(Variable, Vec<Variable>),
    CallBuiltin(Builtin, Vec<Variable>),
    Type(Type),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Binding {
    pub variable: Variable,
    pub value: Expr<Binding>,
}

#[derive(Clone, Debug)]
pub enum VariableTemplate {
    Monomorphic(Variable),
    PolymorphicValue {
        variable: Variable,
        generic_arguments: Vec<Identifier>,
        generic_type: Type,
    },
    PolymorphicFunction {
        function: FunctionPointer,
        generic_arguments: Vec<Identifier>,
        generic_type: Type,
    },
}

#[derive(Clone, Debug, Default)]
pub struct Environment<'a> {
    variable_bindings: HashMap<&'a str, VariableTemplate>,
    type_variable_bindings: HashMap<Var, Identifier>,
}

impl<'a> Environment<'a> {
    /// Takes the irrefutable pattern `pattern` and expands it out into a series of tuple accesses,
    /// updating the `VariableEnvironment` as it goes.
    pub fn bind_pattern(
        &self,
        value: Variable,
        pattern: &'a Pattern<types::Type<Kind>>,
        variable_source: &mut VariableSource,
    ) -> (Environment<'a>, Vec<Binding>) {
        match pattern {
            Pattern::Variable(variable, _) => (self.bind_variable(variable, value), Vec::new()),
            Pattern::Tuple(fields) => {
                let mut bindings = Vec::new();
                let mut inner_environment = self.clone();

                for (index, field) in fields.into_iter().enumerate() {
                    let field_value = variable_source.fresh(self.desugar_type(&field.get_type()));
                    let binding = Binding {
                        variable: field_value.clone(),
                        value: Expr::TupleIndex(value.clone(), index),
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
                    let field_value = variable_source.fresh(self.desugar_type(&field.get_type()));
                    let binding = Binding {
                        variable: field_value.clone(),
                        value: Expr::TupleIndex(value.clone(), index),
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

    pub fn bind_variable(&self, old_variable: &'a str, new_variable: Variable) -> Environment<'a> {
        Environment {
            variable_bindings: self
                .variable_bindings
                .update(old_variable, VariableTemplate::Monomorphic(new_variable)),
            type_variable_bindings: self.type_variable_bindings.clone(),
        }
    }

    fn lookup_variable(
        &self,
        variable_name: &'a str,
        variable_type: Type,
        variable_source: &mut VariableSource,
    ) -> Block<Binding> {
        match self.variable_bindings.get(variable_name).unwrap() {
            VariableTemplate::Monomorphic(variable) => Block {
                instructions: Vec::new(),
                result: variable.clone(),
            },
            VariableTemplate::PolymorphicValue {
                variable,
                generic_arguments,
                generic_type,
            } => {
                let unification = Type::unify(generic_type.clone(), variable_type.clone());
                let mut type_arguments = Vec::new();
                let mut instructions = Vec::new();

                for generic in generic_arguments {
                    let type_variable = variable_source.fresh(Type::Type);
                    instructions.push(Binding {
                        value: Expr::Type(unification.get(generic).unwrap().clone()),
                        variable: type_variable.clone(),
                    });
                    type_arguments.push(type_variable);
                }

                let result = variable_source.fresh(variable_type);

                let stmt = Binding {
                    variable: result.clone(),
                    value: Expr::CallClosure(variable.clone(), type_arguments),
                };

                instructions.push(stmt);

                Block {
                    instructions,
                    result,
                }
            }
            VariableTemplate::PolymorphicFunction {
                function,
                generic_arguments,
                generic_type,
            } => todo!(),
        }
    }

    fn type_variable_binding(&self, variable: &String) -> Option<Identifier> {
        self.type_variable_bindings.get(variable).copied()
    }

    fn desugar_type(&self, r#type: &types::Type<Kind>) -> Type {
        match r#type {
            types::Type::Constant(constant, _) => match constant.as_str() {
                "Num" => Type::Primitive(Primitive::Number),
                "Bool" => boolean_type(),
                _ => panic!("unknown type constant"),
            },
            types::Type::Variable(variable, _) => {
                if let Some(desugared_variable) = self.type_variable_binding(&variable) {
                    Type::Variable(desugared_variable)
                } else {
                    // if types are unknown, we are dealing with a polymorphic function that is never called, so use the unmakable void type
                    Type::Primitive(Primitive::Void)
                }
            }
            types::Type::Arrow(argument, return_type) => Type::Function(
                vec![self.desugar_type(&argument)],
                Box::new(self.desugar_type(&return_type)),
            ),
            types::Type::Tuple(fields) => {
                let desugared_fields: Vec<_> = fields
                    .into_iter()
                    .map(|field| self.desugar_type(field))
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
                    .map(|(_, field)| self.desugar_type(field))
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

    fn qualify_type_vars(
        &self,
        type_vars: im::HashSet<Rc<String>>,
        variable_source: &mut VariableSource,
    ) -> (Environment<'a>, Vec<Identifier>) {
        let mut env = self.clone();
        let mut type_arguments = Vec::new();

        for type_var in type_vars {
            let converted_type_var = variable_source.fresh_id();
            env.type_variable_bindings
                .insert(type_var, converted_type_var);
            type_arguments.push(converted_type_var);
        }

        (env, type_arguments)
    }

    fn bind_polymorphic_variable(
        &self,
        variable: &'a str,
        template: VariableTemplate,
    ) -> Environment<'a> {
        let mut env = self.clone();
        env.variable_bindings.insert(variable, template);
        env
    }
}

#[derive(Default)]
pub struct Definitions {
    definitions: HashMap<FunctionPointer, FunctionDefinition<Binding>>,
}

pub fn convert_expr<'a>(
    expr: &'a expr::expr::Expr<types::Type<Kind>>,
    environment: Environment<'a>,
    variable_source: &mut VariableSource,
    function_pointer_source: &mut FunctionPointerSource,
    definitions: &mut Definitions,
) -> Block<Binding> {
    let expr_type = environment.desugar_type(&expr.get_type());
    match expr {
        expr::expr::Expr::Function(pattern, body, _) => {
            let function_argument =
                variable_source.fresh(environment.desugar_type(&pattern.get_type()));

            let (mut inner_environment, function_preamble) =
                environment.bind_pattern(function_argument.clone(), &pattern, variable_source);

            // todo: captures should also contain captured and uncaptured type variables
            let captures = body.free_vars().relative_complement(pattern.bound_vars());

            let mut arguments = Vec::new();
            let mut converted_captures = Vec::new();
            let mut closure_preamble = Vec::new();

            for (variable, variable_type) in captures {
                let converted_variable_type = environment.desugar_type(&variable_type);
                let converted_variable = environment.lookup_variable(
                    &variable,
                    converted_variable_type.clone(),
                    variable_source,
                );
                converted_captures.push(converted_variable.result);
                closure_preamble.extend(converted_variable.instructions);

                let argument = variable_source.fresh(converted_variable_type);

                arguments.push(argument.clone());

                inner_environment = inner_environment.bind_variable(&variable, argument);
            }

            arguments.push(function_argument);

            let mut converted_body = convert_expr(
                body,
                inner_environment,
                variable_source,
                function_pointer_source,
                definitions,
            );

            let function_pointer = function_pointer_source.fresh();

            let mut body_instructions = Vec::new();
            body_instructions.extend(function_preamble);
            body_instructions.extend(converted_body.instructions);
            converted_body.instructions = body_instructions;

            let definition = FunctionDefinition {
                arguments,
                body: converted_body,
            };

            definitions.insert(function_pointer, definition);

            let closure_variable = variable_source.fresh(expr_type);

            let mut instructions = closure_preamble;
            instructions.push(Binding {
                variable: closure_variable.clone(),
                value: Expr::Closure(function_pointer, converted_captures),
            });

            Block {
                instructions,
                result: closure_variable,
            }
        }
        expr::expr::Expr::Number(number) => {
            let result = variable_source.fresh(Type::Primitive(Primitive::Number));
            let stmt = Binding {
                variable: result.clone(),
                value: Expr::Literal(Literal::Number(*number)),
            };

            let instructions = vec![stmt];

            Block {
                instructions,
                result,
            }
        }
        expr::expr::Expr::Variable(variable_name, variable_type) => environment.lookup_variable(
            variable_name,
            environment.desugar_type(&variable_type),
            variable_source,
        ),
        expr::expr::Expr::Block(block) => convert_block(
            block,
            environment,
            variable_source,
            function_pointer_source,
            definitions,
        ),

        expr::expr::Expr::Application(_, _, _) => todo!(),
        expr::expr::Expr::Boolean(_) => todo!(),
        expr::expr::Expr::Operator(_, _) => todo!(),
        expr::expr::Expr::Record(_) => todo!(),
        expr::expr::Expr::If(_, _, _) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
        expr::expr::Expr::Tuple(_) => todo!(),
        expr::expr::Expr::Constructor(_, _) => todo!(),
    }
}
/*
fn lift_closure<'a>(
    formal_arguments: Vec<Variable>,
    source_arguments: HashSet<(&'a str, types::Type<Kind>)>,
    body: &'a expr::expr::Expr<types::Type<Kind>>,
    environment: Environment<'a>,
    variable_source: &mut VariableSource,
    function_pointer_source: &mut FunctionPointerSource,
    definitions: &mut Definitions,
) -> Block<Binding> {
    let captures = body.free_vars().relative_complement(source_arguments);

    let mut arguments = Vec::new();
    let mut converted_captures = Vec::new();
    let mut closure_preamble = Vec::new();
    let mut inner_environment = environment.clone();

    for (variable, variable_type) in captures {
        let converted_variable_type = environment.desugar_type(&variable_type);
        let converted_variable =
            environment.lookup_variable(variable, converted_variable_type.clone(), variable_source);
        if !formal_arguments.contains(&converted_variable.result) {
            converted_captures.push(converted_variable.result);
            closure_preamble.extend(converted_variable.instructions);
        }

        let argument = variable_source.fresh(converted_variable_type);

        arguments.push(argument.clone());

        inner_environment = inner_environment.bind_variable(&variable, argument);
    }

    arguments.extend(formal_arguments);

    let mut converted_body = convert_expr(
        body,
        inner_environment,
        variable_source,
        function_pointer_source,
        definitions,
    );

    todo!()
}
    */

fn convert_block<'a>(
    block: &'a [expr::expr::Statement<types::Type<Kind>>],
    environment: Environment<'a>,
    variable_source: &mut VariableSource,
    function_pointer_source: &mut FunctionPointerSource,
    definitions: &mut Definitions,
) -> Block<Binding> {
    match block {
        [expr::expr::Statement::Raw(value)] => convert_expr(
            value,
            environment,
            variable_source,
            function_pointer_source,
            definitions,
        ),
        [expr::expr::Statement::Let(
            expr::expr::Pattern::Variable(variable, variable_type),
            value,
            _,
        ), rest @ ..] => {
            let (environment, type_arguments) =
                environment.qualify_type_vars(variable_type.type_vars(), variable_source);
            let value_type = environment.desugar_type(variable_type);
            let mut converted_value = convert_expr(
                value,
                environment.clone(),
                variable_source,
                function_pointer_source,
                definitions,
            );
            let template = if type_arguments.is_empty() {
                VariableTemplate::Monomorphic(converted_value.result.clone())
            } else {
                let captures: HashSet<_> = value
                    .free_vars()
                    .into_iter()
                    .map(|(var_name, var_type)| {
                        let converted_type = environment.desugar_type(&var_type);
                        let variable_code = environment.lookup_variable(
                            var_name,
                            converted_type.clone(),
                            variable_source,
                        );
                        (variable_code, converted_type)
                    })
                    .collect();

                let mut arguments = Vec::new();

                let function_pointer = function_pointer_source.fresh();
                let definition = FunctionDefinition {
                    arguments: type_arguments
                        .iter()
                        .map(|id| Variable::new(id.clone(), Type::Type))
                        .collect(),
                    body: converted_value,
                };
                let closure_variable = variable_source.fresh(definition.get_type());
                definitions.insert(function_pointer, definition);
                converted_value = Block {
                    instructions: vec![Binding {
                        variable: closure_variable.clone(),
                        value: Expr::Closure(function_pointer, Vec::new()),
                    }],
                    result: closure_variable,
                };
                VariableTemplate::PolymorphicValue {
                    variable: converted_value.result.clone(),
                    generic_arguments: type_arguments,
                    generic_type: value_type,
                }
            };
            let environment = environment.bind_polymorphic_variable(variable, template);

            let mut converted_rest = convert_block(
                rest,
                environment,
                variable_source,
                function_pointer_source,
                definitions,
            );

            let mut instructions = converted_value.instructions;
            instructions.extend(converted_rest.instructions);

            converted_rest.instructions = instructions;

            converted_rest
        }
        [expr::expr::Statement::Let(..), ..] => todo!(),
        [expr::expr::Statement::Raw(value), rest @ ..] => {
            let mut instructions = convert_expr(
                value,
                environment.clone(),
                variable_source,
                function_pointer_source,
                definitions,
            )
            .instructions;
            let converted_rest = convert_block(
                rest,
                environment,
                variable_source,
                function_pointer_source,
                definitions,
            );

            instructions.extend(converted_rest.instructions);

            Block {
                instructions,
                result: converted_rest.result,
            }
        }
        [expr::expr::Statement::TypeDef(..), rest @ ..] => convert_block(
            rest,
            environment,
            variable_source,
            function_pointer_source,
            definitions,
        ),
        [] => panic!("blocks should always end with a raw expression"),
    }
}

impl Definitions {
    fn insert(
        &mut self,
        function_pointer: FunctionPointer,
        definition: FunctionDefinition<Binding>,
    ) {
        self.definitions.insert(function_pointer, definition);
    }
}

impl Debug for Definitions {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (func, def) in &self.definitions {
            write!(f, "{:?} = {:#?}", func.id, def)?;
        }
        Ok(())
    }
}

impl<Instr> Typeable for FunctionDefinition<Instr> {
    fn get_type(&self) -> Type {
        let arguments = self.arguments.iter().map(Typeable::get_type).collect();
        let result = self.body.get_type();

        Type::Function(arguments, Box::new(result))
    }
}

impl<Instr> Typeable for Block<Instr> {
    fn get_type(&self) -> Type {
        self.result.get_type()
    }
}
