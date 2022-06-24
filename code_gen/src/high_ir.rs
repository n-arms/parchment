use expr::{
    expr::{Operator, Pattern},
    kind::Kind,
    types::{Fresh, Type, TypeDef},
};
use im::HashMap;
use std::cell::Cell;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Symbol {
    value: usize,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variable {
    Local(Symbol),
    Captured(Symbol),
}

#[derive(Clone, Debug)]
pub struct Environment {
    captures: Vec<Variable>,
}

#[derive(Clone, Debug)]
pub enum Literal {
    Number(f64),
    Boolean(bool),
    Variable(Variable),
    Closure(Symbol, Environment),
}

#[derive(Clone, Debug)]
pub enum Statement {
    Assign(Symbol, Literal),
    CallAssign(Symbol, Literal, Literal),
}

#[derive(Clone, Debug)]
pub struct Block {
    statements: Vec<Statement>,
    result: Literal,
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    parameter: Symbol,
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
        if let Some(_) = self.functions.insert(name, function.clone()) {
            panic!(
                "internal compiler error: redefinition of function {:?} with name {:?}",
                function, name
            )
        }
        self
    }
}

impl Block {
    pub fn unit(result: Literal) -> Block {
        Block {
            result,
            statements: Vec::new(),
        }
    }
}

#[derive(Default)]
pub struct SymbolSupply {
    lowest: Cell<usize>,
}

impl SymbolSupply {
    pub fn fresh(&self) -> Symbol {
        let value = self.lowest.take();
        self.lowest.set(value + 1);
        Symbol { value }
    }
}

pub fn lift(
    expr: &expr::expr::Expr<Type<Kind>>,
    variables: HashMap<String, Variable>,
    variable_supply: &SymbolSupply,
    function_supply: &SymbolSupply,
) -> Program {
    match expr {
        expr::expr::Expr::Number(num) => Program::new(Block::unit(Literal::Number(*num))),
        expr::expr::Expr::Variable(var, _) => {
            let main = if let Some(var) = variables.get(var) {
                Literal::Variable(*var)
            } else {
                panic!("internal compiler error: unknown variable {}", var)
            };
            Program::new(Block::unit(main))
        }
        expr::expr::Expr::Application(function, parameter, _) => {
            let function_prog = lift(
                &function,
                variables.clone(),
                variable_supply,
                function_supply,
            );
            let parameter_prog = lift(
                &parameter,
                variables.clone(),
                variable_supply,
                function_supply,
            );

            let mut statements = function_prog.main.statements;
            statements.extend(parameter_prog.main.statements);

            let result_var = variable_supply.fresh();

            statements.push(Statement::CallAssign(
                result_var,
                function_prog.main.result,
                parameter_prog.main.result,
            ));

            let block = Block {
                result: Literal::Variable(Variable::Local(result_var)),
                statements,
            };

            let mut application_prog = Program::new(block);

            for (var, func) in function_prog
                .functions
                .into_iter()
                .chain(parameter_prog.functions.into_iter())
            {
                application_prog = application_prog.with_function(var, func);
            }
            application_prog
        }
        expr::expr::Expr::Function(Pattern::Variable(var, _), body, _) => {
            let body_var = variable_supply.fresh();

            let new_vars = variables
                .iter()
                .map(|(name, var)| (name.clone(), match var {
                    Variable::Local(var) => Variable::Captured(*var),
                    Variable::Captured(_) => *var,
                }))
                .collect::<HashMap<_, _>>()
                .update(var.clone(), Variable::Local(body_var));

            let body_prog = lift(
                &body,
                new_vars,
                variable_supply,
                function_supply,
            );

            let function = function_supply.fresh();

            let def = FunctionDef {
                parameter: body_var,
                body: body_prog.main,
            };

            let captures = variables.values().copied().collect();

            let env = Environment { captures };

            let mut closure_prog = Program::new(Block::unit(Literal::Closure(function, env)))
                .with_function(function, def);

            for (var, func) in body_prog.functions {
                closure_prog = closure_prog.with_function(var, func);
            }

            closure_prog
        }

        expr::expr::Expr::Function(_, _, _) => todo!(),
        expr::expr::Expr::Boolean(_) => todo!(),
        expr::expr::Expr::Operator(_, _) => todo!(),
        expr::expr::Expr::Record(_) => todo!(),
        expr::expr::Expr::If(_, _, _) => todo!(),
        expr::expr::Expr::Match(_, _, _) => todo!(),
        expr::expr::Expr::Block(_) => todo!(),
        expr::expr::Expr::Tuple(_) => todo!(),
        expr::expr::Expr::Constructor(_, _) => todo!(),
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{{")?;
        for statement in &self.statements {
            match statement {
                Statement::Assign(var, val) => writeln!(f, "\t{} = {}", var, val)?,
                Statement::CallAssign(var, func, arg) => writeln!(f, "\t{} = {} {}", var, func, arg)?,
            }
        }
        writeln!(f, "\t{}", self.result)?;
        write!(f, "}}")
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.value)
    }
}

impl Display for Environment {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "[ ")?;

        for capture in &self.captures {
            write!(f, "{} ", capture)?;
        }

        write!(f, "]")
    }
}

impl Display for Variable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Local(var) => write!(f, "{}", var),
            Variable::Captured(var) => write!(f, "%{}", var)
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Number(num) => write!(f, "{}", num),
            Literal::Boolean(b) => write!(f, "{}", b),
            Literal::Variable(var) => write!(f, "{}", var),
            Literal::Closure(name, env) => write!(f, "({}, {})", name.value, env)
        }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for (name, func) in &self.functions {
            writeln!(f, "let {} = fn {} -> {}", name, func.parameter, func.body)?;
        }

        writeln!(f, "{}", self.main)
    }
}
