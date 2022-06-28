use expr::{
    expr::{Operator, Pattern},
    kind::Kind,
    types::{bool_type, num_type, Fresh, Type, TypeDef},
};
use im::{HashMap, HashSet};
use std::cell::Cell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    identifier: usize,
}

#[derive(Clone, Debug, Default, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Environment {
    captures: HashMap<Variable, Symbol>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    Boolean(bool),
    Closure(Symbol, Environment),
}

#[derive(Clone, Debug)]
pub enum Predicate {
    Raw(Symbol),
    Tag(Symbol),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    Local(Symbol),
    Captured(Symbol),
}

#[derive(Clone, Debug)]
pub enum Expr {
    Variable(Variable),
    Literal(Literal),
    Application(Symbol, Symbol),
    BinaryOperation(Operator, Symbol, Symbol),
    TaggedTuple(usize, Vec<Symbol>),
    TupleIndex(Symbol, usize),
    Switch(Predicate, Vec<Block>),
}

#[derive(Clone, Debug)]
pub struct Statement {
    result: Symbol,
    result_type: Type<Kind>,
    expr: Expr,
}

#[derive(Clone, Debug)]
pub struct Block {
    statements: Vec<Statement>,
    result: Symbol,
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    parameter: Symbol,
    parameter_type: Type<Kind>,
    body: Block,
}

#[derive(Clone, Debug)]
pub struct Program {
    functions: HashMap<Symbol, FunctionDef>,
    main: Block,
}

impl Program {
    pub fn new(main: Block) -> Program {
        Program {
            main,
            functions: HashMap::new(),
        }
    }

    pub fn with_function(mut self, name: Symbol, function: FunctionDef) -> Program {
        if let Some(_) = self.functions.insert(name.clone(), function.clone()) {
            panic!(
                "internal compiler error: redefinition of function {:?} with name {:?}",
                function, name
            )
        }
        self
    }
}

impl Block {
    pub fn unit(result: Symbol) -> Block {
        Block {
            result,
            statements: Vec::new(),
        }
    }

    pub fn with_statement(mut self, stmt: Statement) -> Block {
        self.statements.push(stmt);
        self
    }
}

pub struct SymbolSupply {
    lowest: Cell<usize>,
}

impl SymbolSupply {
    pub fn fresh(&self) -> Symbol {
        let identifier = self.lowest.take();
        self.lowest.set(identifier + 1);
        Symbol { identifier }
    }

    pub fn new() -> Self {
        SymbolSupply {
            lowest: Cell::new(1),
        }
    }
}

pub fn deconstruct_pattern(
    pattern: &Pattern<Type<Kind>>, 
    binding: Variable, 
    variable_supply: &SymbolSupply
) -> (HashMap<String, Variable>, Vec<Statement>) {
    match pattern {
        Pattern::Variable(var_name, var_type) => {
            let var = variable_supply.fresh();
            let bindings = HashMap::unit(var_name.clone(), Variable::Local(var));
            let stmt = Statement {
                result: var,
                result_type: var_type.clone(),
                expr: Expr::Variable(binding)
            };
            (bindings, vec![stmt])
        },
        Pattern::Record(_) => todo!(),
        Pattern::Tuple(_) => todo!(),
        Pattern::Construction(_, _, _) => todo!(),
    }
}

pub fn lift(
    expr: &expr::expr::Expr<Type<Kind>>,
    variables: HashMap<String, Variable>,
    variable_supply: &SymbolSupply,
    function_supply: &SymbolSupply,
) -> Program {
    match expr {
        expr::expr::Expr::Number(number) => {
            let result = variable_supply.fresh();
            let block = Block::unit(result).with_statement(Statement {
                result,
                result_type: num_type(),
                expr: Expr::Literal(Literal::Number(*number)),
            });
            Program::new(block)
        }
        expr::expr::Expr::Application(func, param, result_type) => {
            let result = variable_supply.fresh();

            let func_prog = lift(func, variables.clone(), variable_supply, function_supply);
            let param_prog = lift(param, variables.clone(), variable_supply, function_supply);

            let mut block = Block::unit(result);
            let statements = func_prog
                .main
                .statements
                .into_iter()
                .chain(param_prog.main.statements.into_iter());

            for stmt in statements {
                block = block.with_statement(stmt);
            }

            block = block.with_statement(Statement {
                result,
                result_type: result_type.clone(),
                expr: Expr::Application(func_prog.main.result, param_prog.main.result),
            });

            let defs = func_prog
                .functions
                .into_iter()
                .chain(param_prog.functions.into_iter());

            let mut prog = Program::new(block);

            for (name, def) in defs {
                prog = prog.with_function(name, def);
            }

            prog
        }
        expr::expr::Expr::Variable(var_name, var_type) => {
            if let Some(var) = variables.get(var_name) {
                match var {
                    Variable::Local(symbol) => Program::new(Block::unit(*symbol)),
                    Variable::Captured(symbol) => {
                        let result = variable_supply.fresh();
                        let block = Block::unit(result).with_statement(Statement {
                            result_type: var_type.clone(),
                            result,
                            expr: Expr::Variable(Variable::Captured(*symbol)),
                        });
                        Program::new(block)
                    }
                }
            } else {
                panic!("internal compiler error: unknown variable {}", var_name)
            }
        }
        expr::expr::Expr::Function(param_pat, body, _) => {
            let body_var_supply = SymbolSupply::new();
            let param = body_var_supply.fresh();

            let (captures, mut body_vars) = free(body)
                .iter()
                .map(|var_name| (var_name, body_var_supply.fresh()))
                .filter_map(|(var_name, body_sym)| Some(((variables.get(var_name)?.clone(), body_sym), (var_name.clone(), Variable::Captured(body_sym)))))
                .unzip::<_, _, _, HashMap<String, Variable>>();

            let env = Environment { captures };

            let (pattern_locals, header) = deconstruct_pattern(param_pat, Variable::Local(param), &body_var_supply);
            body_vars.extend(pattern_locals);

            let mut body_prog = lift(body, body_vars, &body_var_supply, function_supply);

            let mut statements = header;
            statements.extend(body_prog.main.statements);
            body_prog.main.statements = statements;

            let function_symbol = function_supply.fresh();
            let definition = FunctionDef {
                body: body_prog.main,
                parameter: param,
                parameter_type: param_pat.get_type()
            };

            let closure_var = variable_supply.fresh();
            let closure_stmt = Statement {
                result: closure_var,
                result_type: Type::Arrow(Rc::new(param_pat.get_type()), Rc::new(body.get_type())),
                expr: Expr::Literal(Literal::Closure(function_symbol, env))
            };

            let block = Block::unit(closure_var)
                .with_statement(closure_stmt);

            let mut prog = Program::new(block).with_function(function_symbol, definition);

            for (name, def) in body_prog.functions {
                prog = prog.with_function(name, def);
            }

            prog
        }
        expr::expr::Expr::Boolean(boolean) => {
            let result = variable_supply.fresh();
            let block = Block::unit(result).with_statement(Statement {
                result,
                result_type: bool_type(),
                expr: Expr::Literal(Literal::Boolean(*boolean)),
            });
            Program::new(block)
        }
        expr::expr::Expr::If(predicate, consequent, alternative) => {
            let pred_prog = lift(predicate, variables.clone(), variable_supply, function_supply);
            let cons_prog = lift(consequent, variables.clone(), variable_supply, function_supply);
            let altr_prog = lift(alternative, variables, variable_supply, function_supply);

            let result = variable_supply.fresh();
            let if_stmt = Statement {
                result,
                result_type: consequent.get_type(),
                expr: Expr::Switch(
                    Predicate::Raw(pred_prog.main.result),
                    vec![
                        altr_prog.main,
                        cons_prog.main,
                    ]
                )
            };

            let mut block = pred_prog.main.with_statement(if_stmt);
            block.result = result;

            let mut prog = Program::new(block);

            let funcs = pred_prog.functions.into_iter().chain(cons_prog.functions.into_iter()).chain(altr_prog.functions.into_iter());

            for (name, def) in funcs {
                prog = prog.with_function(name, def);
            }

            prog
        }
        expr::expr::Expr::Operator(op, _) => {
            let supply = SymbolSupply::new();
            let supply = SymbolSupply::new();

            let inner_symbol = function_supply.fresh();
            let outer_symbol = function_supply.fresh();

            let outer_param = supply.fresh();
            let captured_param = supply.fresh();
            let inner_closure = supply.fresh();

            let outer_stmt = Statement {
                result: inner_closure,
                result_type: op.half_applied_type(),
                expr: Expr::Literal(Literal::Closure(
                    inner_symbol,
                    Environment {
                        captures: HashMap::unit(Variable::Local(outer_param), captured_param)
                    }
                ))
            };
            let outer = Block::unit(inner_closure)
                .with_statement(outer_stmt);

            let outer_def = FunctionDef {
                body: outer,
                parameter: outer_param,
                parameter_type: op.first_arg_type()
            };

            let inner_param = supply.fresh();
            let localized_capture = supply.fresh();
            let final_result = supply.fresh();

            let localize_stmt = Statement {
                result: localized_capture,
                result_type: op.first_arg_type(),
                expr: Expr::Variable(Variable::Captured(captured_param))
            };

            let operation_stmt = Statement {
                result: final_result,
                result_type: op.second_arg_type(),
                expr: Expr::BinaryOperation(*op, inner_param, localized_capture)
            };

            let inner = Block::unit(final_result)
                .with_statement(localize_stmt)
                .with_statement(operation_stmt);

            let inner_def = FunctionDef {
                body: inner,
                parameter: inner_param,
                parameter_type: op.second_arg_type()
            };

            let outer_closure = variable_supply.fresh();
            let global_stmt = Statement {
                result: outer_closure,
                result_type: op.get_type(),
                expr: Expr::Literal(Literal::Closure(outer_symbol, Environment::default()))
            };
            let global = Block::unit(outer_closure)
                .with_statement(global_stmt);

            Program::new(global)
                .with_function(outer_symbol, outer_def)
                .with_function(inner_symbol, inner_def)
        },
        expr::expr::Expr::Tuple(_) => todo!(),
        expr::expr::Expr::Constructor(_, _) => todo!(),
        expr::expr::Expr::Record(_) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
        expr::expr::Expr::Block(_) => todo!(),
    }
}

fn free<A: Clone>(e: &expr::expr::Expr<A>) -> HashSet<String> {
    match e {
        expr::expr::Expr::Function(pattern, body, _) => {
            free(body).relative_complement(pattern.bound_vars())
        }
        expr::expr::Expr::Application(e1, e2, _) => free(e1).union(free(e2)),
        expr::expr::Expr::Constructor(..)
        | expr::expr::Expr::Operator(..)
        | expr::expr::Expr::Number(_)
        | expr::expr::Expr::Boolean(_) => HashSet::new(),
        expr::expr::Expr::Variable(var, _) => HashSet::unit(var.clone()),
        expr::expr::Expr::Record(r) => r.values().flat_map(free).collect(),
        expr::expr::Expr::Tuple(es) => es.iter().flat_map(free).collect(),
        expr::expr::Expr::If(p, c, a) => free(p).union(free(c)).union(free(a)),
        expr::expr::Expr::Block(b) => free_block(b),
        expr::expr::Expr::Match(matchand, arms, _) => free(matchand).union(
            arms.iter()
                .flat_map(|(pattern, expr)| free(expr).relative_complement(pattern.bound_vars()))
                .collect(),
        ),
    }
}

fn free_block<A: Clone>(b: &[expr::expr::Statement<A>]) -> HashSet<String> {
    match b {
        [expr::expr::Statement::Let(pattern, body, _), rest @ ..] => free(body)
            .union(free_block(rest))
            .relative_complement(pattern.bound_vars()),
        [expr::expr::Statement::Raw(e), rest @ ..] => free(e).union(free_block(rest)),
        [expr::expr::Statement::TypeDef(..), rest @ ..] => free_block(rest),
        [] => HashSet::new(),
    }
}
