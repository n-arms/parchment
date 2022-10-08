use core::slice;

use crate::desugar;
use crate::variable::*;
use im::{HashSet, HashMap};

// the main focus of this pass is to make specialization explicit and perform closure conversion
// the question is whether we want to explicitly name each node

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Block<'e, 't, Instr> {
    pub instructions: &'e [Instr],
    pub result: Variable<'t>
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub enum Expr<'e, 't> {
    Closure(FunctionPointer, &'e [Variable<'t>]),
    Tuple(Tag, &'e [Variable<'t>]),
    Variable(Variable<'t>),
    Literal(Literal),
    Type(Type<'t>),
    CallClosure(Variable<'t>, &'e [Variable<'t>]),
    CallBuiltin(Builtin, &'e [Variable<'t>]),
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Binding<'e, 't> {
    pub variable: Variable<'t>,
    pub value: Expr<'e, 't>
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FunctionPointer {
    index: Identifier
}

#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct FunctionDefinition<'e, 't, Instr> {
    pub arguments: &'e [Variable<'t>],
    pub body: Block<'e, 't, Instr>
}



#[derive(Clone, Debug, PartialEq, PartialOrd)]
pub struct Definitions<'e, 't, Instr: Clone> {
    definitions: HashMap<FunctionPointer, FunctionDefinition<'e, 't, Instr>>
}

impl<'e, 't, Instr: Clone> Default for Definitions<'e, 't, Instr> {
    fn default() -> Self {
        Definitions {
            definitions: HashMap::new()
        }
    }
}

impl<'e, 't, Instr: Clone> Definitions<'e, 't, Instr> {
    pub fn define(&mut self, function: FunctionPointer, definition: FunctionDefinition<'e, 't, Instr>) {
        self.definitions.insert(function, definition);
    }
}

#[derive(Clone, Debug)]
pub struct VariableTemplate<'old, 't> {
    generics: &'old [Identifier],
    generic_type: &'t Type<'t>
}

#[derive(Clone, Debug, Default)]
pub struct Environment<'old, 't> {
    let_bindings: HashMap<Identifier, VariableTemplate<'old, 't>>,
    variable_bindings: HashMap<Identifier, Variable<'t>>
}

impl<'old, 't> Environment<'old, 't> {
    fn bind_variable(&self, old: Identifier, new: Variable<'t>) -> Environment<'old, 't> {
        let mut env = self.clone();
        env.variable_bindings.insert(old, new);
        env
    }

    fn lookup_variable<'e>(&self, variable: Variable<'t>) -> Block<'e, 't, Binding<'e, 't>> {
        if let Some(new_variable) = self.variable_bindings.get(&variable.id) {
            Block {
                instructions: &[],
                result: *new_variable
            }
        } else if let Some(template) = self.let_bindings.get(&variable.id) {
            todo!()
        } else {
            panic!("looking up unknown variable")
        }
    }
}

fn free_variables<'old, 't>(
    expr: &'old desugar::Expr<'old, 't>,
    mut except: HashSet<Identifier>
) -> HashSet<Variable<'t>> {
    match expr {
        desugar::Expr::Function(arguments, body) => {
            for arg in *arguments {
                except.insert(arg.id);
            }
            free_variables(body, except)
        },
        desugar::Expr::CallFunction(function, arguments, _) => {
            let mut captures = free_variables(*function, except.clone());
            
            for arg in *arguments {
                captures.extend(free_variables(arg, except.clone()));
            }

            captures
        },
        desugar::Expr::Tuple(_, _, arguments) | 
        desugar::Expr::CallBuiltin(_, arguments) => arguments.into_iter().flat_map(|arg| free_variables(arg, except.clone())).collect(),
        desugar::Expr::Literal(_) => HashSet::new(),
        desugar::Expr::Variable(variable) => if except.contains(&variable.id) {
            HashSet::new()
        } else {
            HashSet::unit(*variable)
        },
        desugar::Expr::Let(_, binding, rest) => {
            except.insert(binding.variable.id);

            free_variables(rest, except)
        }
    }
}

#[derive(Default)]
pub struct FunctionPointerSource(IdentifierSource);

impl FunctionPointerSource {
    fn fresh(&mut self) -> FunctionPointer {
        FunctionPointer {
            index: self.0.fresh()
        }
    }
}

pub fn lift_expr<'old, 'e, 't>(
    expr: &'old desugar::Expr<'old, 't>,
    env: Environment<'old, 't>,
    definitions: &mut Definitions<'e, 't, Binding<'e, 't>>,
    variables: &mut VariableSource,
    function_pointers: &mut FunctionPointerSource,
    arenas: Allocator<'e, 't>,
) -> Block<'e, 't, Binding<'e, 't>> {
    let expr_type = expr.get_type(arenas);
    match expr {
        desugar::Expr::Function(formal_arguments, unlifted_body) => {
            let captures = free_variables(unlifted_body, formal_arguments.into_iter().map(|var| var.id).collect());

            let mut arguments = Vec::new();
            let mut lifted_captures = Vec::new();
            let mut preamble = Vec::new();
            let mut env = env;

            for capture in captures {
                let argument = variables.fresh(capture.variable_type);
                env = env.bind_variable(capture.id, argument.clone());
                arguments.push(argument.clone());

                let lifted_capture = env.lookup_variable(capture);
                lifted_captures.push(lifted_capture.result);
                preamble.extend(lifted_capture.instructions.iter().cloned());
            }

            arguments.extend(*formal_arguments);

            for arg in *formal_arguments {
                env = env.bind_variable(arg.id, *arg);
            }

            let lifted_body = lift_expr(*unlifted_body, env, definitions, variables, function_pointers, arenas);

            let definition = FunctionDefinition {
                arguments: arenas.alloc_expr_iter(arguments.into_iter()),
                body: lifted_body,
            };

            let ptr = function_pointers.fresh();

            definitions.define(ptr, definition);

            let closure_variable = variables.fresh(arenas.alloc_type(expr_type));

            let mut instructions = preamble;
            instructions.push(Binding {
                variable: closure_variable.clone(),
                value: Expr::Closure(ptr, arenas.alloc_expr_iter(lifted_captures.into_iter()))
            });

            Block {
                instructions: arenas.alloc_expr_iter(instructions.into_iter()),
                result: closure_variable
            }
        },
        desugar::Expr::Literal(literal) => {
            let result = variables.fresh(arenas.alloc_type(expr_type));
            let instr = Binding {
                variable: result.clone(),
                value: Expr::Literal(literal.clone())
            };
            let instructions = slice::from_ref(arenas.alloc_expr(instr));
            
            Block {
                instructions,
                result
            }
        }
        desugar::Expr::Variable(variable) => env.lookup_variable(*variable),
        desugar::Expr::CallFunction(_, _, _) => todo!(),
        desugar::Expr::CallBuiltin(_, _) => todo!(),
        desugar::Expr::Tuple(_, _, _) => todo!(),
        desugar::Expr::Let(_, _, _) => todo!(),
    }
}

// as we traverse the tree we are constantly allocating intermediate variables so that
// every node has an explict name. This would normally copy a lot of types,
// but since `Type` is always held by reference, we don't need to worry about that.
// the environment should give us the information we need to recognize innocent seeming
// variables for the evil polymorphic little monsters that they are, and then repalce the 
// variable with a call to a function with the appropriate type. This means that we need to 
// keep track of all the let bound variables, instructions for how to instantiate them.
//
// Additionally, this pass is in charge of closure conversion. This should actually be fairly
// straightforwards, the only tricky bit is going to be finding all the free variables in an
// expression so that we can capture them.
