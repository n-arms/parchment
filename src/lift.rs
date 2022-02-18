use super::expr::{self, Pattern, Statement};
use super::types::VarSet;
use im::{hashmap, HashMap, HashSet};

type FunSet = VarSet<usize>;

#[derive(Clone, Debug)]
pub enum Expr {
    /// evaluate the expression on the left to a closure, then call it with the expression on the
    /// right
    Application(Box<Expr>, Box<Expr>),
    /// a variable introduced by a function call or a local variable assignment
    Variable(String),
    Number(f64),
    Boolean(bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// a closure containing the function pointer and the set of variables to be put into an env
    Closure(usize, HashSet<String>),
    /// look up a variable from an env
    EnvLookup(usize),
    /// evaluate all the expressions
    All(Vec<Expr>),
    /// ignore the result of an expression
    Ignore(Box<Expr>),
    /// assign the results of the expression to the given local variables. If an expression returns
    /// multiple values (using Expr::All), the nth value is assigned to the nth variable.
    /// Otherwise, the first variable has the value assigned to it
    Assign(String, Box<Expr>),
    /// evaluate all the expressions then pack them into a record on the heap
    Record(Vec<Expr>),
    /// look up the nth value in the record produced by evaluating e
    RecordLookup(Box<Expr>, usize)
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    arg: String,
    body: Expr,
    env: Vec<String>
}

#[derive(Clone, Debug)]
pub struct Program {
    defs: Vec<FunctionDef>,
    main: Expr,
}

fn free(e: &expr::Expr<String>) -> HashSet<String> {
    match e {
        expr::Expr::Function(p, b) => free(b).relative_complement(p.bound_vars()),
        expr::Expr::Application(e1, e2) => free(e1).union(free(e2)),
        expr::Expr::Number(_) | expr::Expr::Boolean(_) => HashSet::new(),
        expr::Expr::Variable(v) => HashSet::unit(v.clone()),
        expr::Expr::Record(r) => r.values().flat_map(free).collect(),
        expr::Expr::If(p, c, a) => free(p).union(free(c)).union(free(a)),
        expr::Expr::Block(b) => free_block(b),
        expr::Expr::Match(_, _) => todo!(),
    }
}

fn free_block(b: &[Statement<String>]) -> HashSet<String> {
    match b {
        [Statement::Let(p, b), rest @ ..] => free(b)
            .relative_complement(p.bound_vars())
            .union(free_block(rest)),
        [Statement::Raw(e), rest @ ..] => free(e).union(free_block(rest)),
        [] => HashSet::new(),
    }
}

pub fn to_lookup(p: &Pattern<String>, base: Expr) -> Vec<Expr> {
    match p {
        Pattern::Variable(v) => vec![
            Expr::Assign(v.clone(), Box::new(base))
        ],
        Pattern::Record(r) => {
            let mut fields: Vec<_> = r.iter().collect();
            fields.sort_by_key(|(name, _)| *name);
            fields.into_iter().enumerate().flat_map(|(i, (_, p))| to_lookup(p, Expr::RecordLookup(Box::new(base.clone()), i))).collect()
        }
    }
}

pub fn lift(
    e: &expr::Expr<String>,
    in_env: HashSet<String>,
    v: &VarSet<String>,
    fun: &FunSet,
) -> Result<Program, ()> {
    match e {
        expr::Expr::Function(p, b) => {
            let f_id = fun.fresh();
            let unbound = free(b).relative_complement(p.bound_vars());

            let mut body = lift(b, unbound.clone(), v, fun)?;

            let arg = v.fresh();

            let mut lookup = to_lookup(p, Expr::Variable(arg.clone()));
            lookup.push(body.main);

            let mut env: Vec<_> = unbound.iter().cloned().collect();
            env.sort();

            body.defs.insert(
                0,
                FunctionDef {
                    arg,
                    body: Expr::All(lookup),
                    env
                },
            );
            body.main = Expr::Closure(f_id, unbound);
            Ok(body)
        }
        expr::Expr::Application(e1, e2) => {
            let mut p1 = lift(e1, in_env.clone(), v, fun)?;
            let Program { defs, main } = lift(e2, in_env, v, fun)?;

            p1.defs.extend(defs);
            p1.main = Expr::Application(Box::new(p1.main), Box::new(main));
            Ok(p1)
        }
        expr::Expr::Number(n) => Ok(Program {
            defs: Vec::new(),
            main: Expr::Number(*n),
        }),
        expr::Expr::Boolean(b) => Ok(Program {
            defs: Vec::new(),
            main: Expr::Boolean(*b),
        }),
        expr::Expr::Variable(v) => {
            if in_env.contains(v) {
                let mut sorted: Vec<_> = in_env.into_iter().collect();
                sorted.sort();
                Ok(Program {
                    defs: Vec::new(),
                    main: Expr::EnvLookup(sorted.binary_search(v).unwrap()),
                })
            } else {
                Ok(Program {
                    defs: Vec::new(),
                    main: Expr::Variable(v.clone()),
                })
            }
        }
        expr::Expr::Record(r) => {
            let mut terms: Vec<(String, Program)> = r
                .iter()
                .map(|(var, e)| Ok((var.clone(), lift(e, in_env.clone(), v, fun)?)))
                .collect::<Result<_, _>>()?;
            terms.sort_by_key(|(var, _)| var.clone());
            let mut defs = Vec::new();
            let mut main = Vec::new();
            for (_, term) in terms {
                defs.extend(term.defs);
                main.push(term.main);
            }
            Ok(Program {
                defs,
                main: Expr::Record(main),
            })
        }
        expr::Expr::If(p, c, a) => {
            let mut p1 = lift(p, in_env.clone(), v, fun)?;
            let p2 = lift(c, in_env.clone(), v, fun)?;
            let p3 = lift(a, in_env, v, fun)?;
            p1.defs.extend(p2.defs);
            p1.defs.extend(p3.defs);
            p1.main = Expr::If(Box::new(p1.main), Box::new(p2.main), Box::new(p3.main));
            Ok(p1)
        }
        expr::Expr::Block(b) => {
            let mut defs = Vec::new();
            let mut exprs = Vec::new();
            for (i, stmt) in b.iter().enumerate() {
                match stmt {
                    Statement::Let(p, b) => {
                        let temp = v.fresh();
                        let lookup = to_lookup(p, Expr::Variable(temp.clone()));
                        let Program { main, defs: d } = lift(b, in_env.clone(), v, fun)?;
                        exprs.push(Expr::Assign(temp, Box::new(main)));
                        exprs.extend(lookup);
                        defs.extend(d);
                    }
                    Statement::Raw(e) => {
                        let Program { main, defs: d } = lift(e, in_env.clone(), v, fun)?;
                        exprs.push(if i == b.len() - 1 {
                            main
                        } else {
                            Expr::Ignore(Box::new(main))
                        });
                        defs.extend(d);
                    }
                }
            }
            Ok(Program {
                defs,
                main: Expr::All(exprs),
            })
        }
        expr::Expr::Match(_, _) => todo!(),
    }
}
