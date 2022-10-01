use crate::desugar;
use crate::variable::*;
use im::HashMap;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionPointer {
    id: Identifier,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinition {
    arguments: Vec<Variable>,
    body: Block,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Program {
    definitions: HashMap<FunctionPointer, FunctionDefinition>,
    main: Block,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Block {
    instructions: Vec<Binding>,
    result: Variable,
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr {
    Closure(FunctionPointer, Vec<Variable>),
    Tuple(Tag, Vec<Variable>),
    TupleIndex(Variable, usize),
    Variable(Variable),
    Literal(Literal),
    Switch(Variable, Vec<(Tag, Block)>),
    SpecializeClosure(Variable, Vec<Type>),
    CallClosure(Variable, Vec<Variable>),
    CallBuiltin(Builtin, Vec<Variable>),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Binding {
    variable: Variable,
    value: Expr,
}

#[derive(Clone, Default)]
pub struct Environment {
    let_bindings: HashMap<Identifier, Variable>,
}

impl Environment {
    pub fn lookup_variable(&self, variable: Identifier) -> Option<Variable> {
        self.let_bindings.get(&variable).cloned()
    }

    pub fn bind_variable(&self, old_variable: Identifier, new_variable: Variable) -> Self {
        Environment {
            let_bindings: self.let_bindings.update(old_variable, new_variable),
        }
    }
}

#[derive(Default)]
pub struct FunctionPointerSource(IdentifierSource);

impl FunctionPointerSource {
    fn fresh(&mut self) -> FunctionPointer {
        FunctionPointer { id: self.0.fresh() }
    }
}

pub fn lift_expr(
    expr: desugar::Expr,
    environment: Environment,
    variable_source: &mut VariableSource,
    function_pointer_source: &mut FunctionPointerSource,
) -> Program {
    let expr_type = expr.get_type();
    match expr {
        desugar::Expr::Function(arguments, body) => {
            let closure_pointer = function_pointer_source.fresh();
            let captured_variables: Vec<_> = body
                .free_variables()
                .relative_complement(arguments.iter().cloned().collect())
                .into_iter()
                .collect();
            let mut lifted_arguments = captured_variables.clone();
            lifted_arguments.extend(arguments);

            let Program {
                mut definitions,
                mut main,
            } = lift_expr(*body, environment, variable_source, function_pointer_source);

            let definition = FunctionDefinition {
                arguments: lifted_arguments,
                body: main,
            };

            definitions.insert(closure_pointer.clone(), definition);

            let closure_result_variable = variable_source.fresh(expr_type);
            main = Block::new(closure_result_variable.clone()).with_instruction(
                closure_result_variable,
                Expr::Closure(closure_pointer, captured_variables),
            );

            Program { definitions, main }
        }
        desugar::Expr::Variable(variable) => Program {
            definitions: HashMap::new(),
            main: Block::new(
                environment
                    .lookup_variable(variable.identifier())
                    .unwrap_or(variable),
            ),
        },
        desugar::Expr::CallFunction(function, arguments) => {
            let Program {
                mut main,
                mut definitions,
            } = lift_expr(
                *function,
                environment.clone(),
                variable_source,
                function_pointer_source,
            );

            let mut lifted_arguments = Vec::new();

            for argument in arguments {
                let argument_program = lift_expr(
                    argument,
                    environment.clone(),
                    variable_source,
                    function_pointer_source,
                );
                main.instructions.extend(argument_program.main.instructions);
                definitions.extend(argument_program.definitions);
                lifted_arguments.push(argument_program.main.result);
            }
            let call_result = variable_source.fresh(expr_type);
            let call_expr = Expr::CallClosure(main.result.clone(), lifted_arguments);
            main = main.with_instruction(call_result.clone(), call_expr);
            main.result = call_result;

            Program { main, definitions }
        }
        desugar::Expr::Let(_, binding, tail) => {
            let Program {
                mut definitions,
                mut main,
            } = lift_expr(
                binding.value,
                environment.clone(),
                variable_source,
                function_pointer_source,
            );

            let inner_environment =
                environment.bind_variable(binding.variable.identifier(), main.result);

            let lifted_tail = lift_expr(
                *tail,
                inner_environment,
                variable_source,
                function_pointer_source,
            );

            main.instructions.extend(lifted_tail.main.instructions);
            main.result = lifted_tail.main.result;
            definitions.extend(lifted_tail.definitions);

            Program { main, definitions }
        }
        desugar::Expr::Tuple(tag, tuple_type, fields) => {
            let mut definitions = HashMap::new();
            let mut instructions = Vec::new();
            let mut field_variables = Vec::new();

            for field in fields {
                let lifted_field = lift_expr(
                    field,
                    environment.clone(),
                    variable_source,
                    function_pointer_source,
                );
                definitions.extend(lifted_field.definitions);
                instructions.extend(lifted_field.main.instructions);
                field_variables.push(lifted_field.main.result);
            }

            let tuple_variable = variable_source.fresh(Type::Tuple(tuple_type));
            instructions.push(Binding {
                variable: tuple_variable.clone(),
                value: Expr::Tuple(tag, field_variables),
            });

            let main = Block {
                instructions,
                result: tuple_variable,
            };

            Program { definitions, main }
        }
        desugar::Expr::TupleIndex(tuple, index, _) => {
            let Program {
                definitions,
                mut main,
            } = lift_expr(
                *tuple,
                environment,
                variable_source,
                function_pointer_source,
            );

            let index_variable = variable_source.fresh(expr_type);

            main.instructions.push(Binding {
                variable: index_variable.clone(),
                value: Expr::TupleIndex(main.result, index),
            });

            main.result = index_variable;

            Program { definitions, main }
        }
        desugar::Expr::Specialize(target, generic_arguments, _) if generic_arguments.is_empty() => {
            let main = Block::new(environment.lookup_variable(target.identifier()).unwrap());
            Program {
                main,
                definitions: HashMap::new(),
            }
        }
        desugar::Expr::Specialize(target, generic_arguments, specialized_type) => {
            let target_closure = environment.lookup_variable(target.identifier()).unwrap();

            let expr = Expr::SpecializeClosure(target_closure, generic_arguments);
            let specialize_variable = variable_source.fresh(specialized_type);
            let main =
                Block::new(specialize_variable.clone()).with_instruction(specialize_variable, expr);

            Program {
                main,
                definitions: HashMap::new(),
            }
        }
        desugar::Expr::Literal(literal) => Program {
            definitions: HashMap::new(),
            main: lift_literal(literal, variable_source),
        },
        desugar::Expr::Switch(_, _) => todo!(),
        desugar::Expr::CallBuiltin(builtin, arguments) => {
            assert_eq!(arguments.len(), 2);

            let mut definitions = HashMap::new();
            let mut instructions = Vec::new();
            let mut lifted_arguments = Vec::new();

            for argument in arguments {
                let lifted_argument = lift_expr(
                    argument,
                    environment.clone(),
                    variable_source,
                    function_pointer_source,
                );

                definitions.extend(lifted_argument.definitions);
                instructions.extend(lifted_argument.main.instructions);
                lifted_arguments.push(lifted_argument.main.result);
            }

            let builtin_variable = variable_source.fresh(expr_type);

            instructions.push(Binding {
                variable: builtin_variable.clone(),
                value: Expr::CallBuiltin(builtin, lifted_arguments),
            });

            let main = Block {
                instructions,
                result: builtin_variable,
            };

            Program { main, definitions }
        }
    }
}

fn lift_literal(literal: Literal, variable_source: &mut VariableSource) -> Block {
    let literal_result = variable_source.fresh(literal.get_type());
    Block::new(literal_result.clone()).with_instruction(literal_result, Expr::Literal(literal))
}

impl Block {
    fn new(result: Variable) -> Block {
        Block {
            result,
            instructions: Vec::new(),
        }
    }

    fn with_instruction(mut self, variable: Variable, value: Expr) -> Block {
        self.instructions.push(Binding { value, variable });
        self
    }
}
