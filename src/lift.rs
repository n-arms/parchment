use super::expr::{self, Operator, Pattern, Statement};
use super::types::VarSet;
use im::HashSet;

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
    /// a closure containing the function pointer and the set of values to be put into an env
    Closure(usize, Vec<Expr>),
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
    RecordLookup(Box<Expr>, usize),
    /// call the primative operator b with the two expressions
    BinaryPrimitive(Operator, Box<Expr>, Box<Expr>),
}

#[derive(Clone, Debug)]
pub struct FunctionDef {
    pub arg: String,
    pub body: Expr,
    pub env: Vec<String>,
    pub locals: HashSet<String>,
}

#[derive(Clone, Debug)]
pub struct Program {
    pub defs: Vec<FunctionDef>,
    pub main: Expr,
}

fn free(e: &expr::Expr<String>) -> HashSet<String> {
    match e {
        expr::Expr::Function(p, b) => free(b).relative_complement(p.bound_vars()),
        expr::Expr::Application(e1, e2) => free(e1).union(free(e2)),
        expr::Expr::Operator(_) | expr::Expr::Number(_) | expr::Expr::Boolean(_) => HashSet::new(),
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
            .union(free_block(rest))
            .relative_complement(p.bound_vars()),
        [Statement::Raw(e), rest @ ..] => free(e).union(free_block(rest)),
        [] => HashSet::new(),
    }
}

pub fn to_lookup(p: &Pattern<String>, base: Expr) -> Vec<Expr> {
    match p {
        Pattern::Variable(v) => vec![Expr::Assign(v.clone(), Box::new(base))],
        Pattern::Record(r) => {
            let mut fields: Vec<_> = r.iter().collect();
            fields.sort_by_key(|(name, _)| *name);
            fields
                .into_iter()
                .enumerate()
                .flat_map(|(i, (_, p))| to_lookup(p, Expr::RecordLookup(Box::new(base.clone()), i)))
                .collect()
        }
    }
}

pub fn lift(
    e: &expr::Expr<String>,
    current_env: &[String],
    v: &VarSet<String>,
    fun: &FunSet,
) -> Result<Program, ()> {
    match e {
        expr::Expr::Function(p, b) => {
            let f_id = fun.fresh();
            let mut env: Vec<_> = free(b)
                .relative_complement(p.bound_vars())
                .into_iter()
                .collect();
            env.sort();

            let mut body = lift(b, &env, v, fun)?;

            let arg = v.fresh();

            let mut lookup = to_lookup(p, Expr::Variable(arg.clone()));
            lookup.push(body.main);

            let full_body = Expr::All(lookup);
            let locals = locals(&full_body);

            body.defs.insert(
                0,
                FunctionDef {
                    arg,
                    body: full_body,
                    env: env.clone(),
                    locals,
                },
            );
            let new_env = env
                .into_iter()
                .map(|name| {
                    if let Ok(i) = current_env.binary_search(&name) {
                        Expr::EnvLookup(i)
                    } else {
                        Expr::Variable(name)
                    }
                })
                .collect();
            body.main = Expr::Closure(f_id, new_env);
            Ok(body)
        }
        expr::Expr::Application(e1, e2) => {
            if let expr::Expr::Application(o, e1) = e1.as_ref() {
                if let expr::Expr::Operator(o) = o.as_ref() {
                    let mut p1 = lift(e1, current_env, v, fun)?;
                    let Program { defs, main } = lift(e2, current_env, v, fun)?;
                    p1.defs.extend(defs);
                    p1.main = Expr::BinaryPrimitive(*o, Box::new(p1.main), Box::new(main));
                    return Ok(p1);
                }
            }
            let mut p1 = lift(e1, current_env, v, fun)?;
            let Program { defs, main } = lift(e2, current_env, v, fun)?;

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
            if let Ok(i) = current_env.binary_search(v) {
                Ok(Program {
                    defs: Vec::new(),
                    main: Expr::EnvLookup(i),
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
                .map(|(var, e)| Ok((var.clone(), lift(e, current_env, v, fun)?)))
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
            let mut p1 = lift(p, current_env, v, fun)?;
            let p2 = lift(c, current_env, v, fun)?;
            let p3 = lift(a, current_env, v, fun)?;
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
                        let Program { main, defs: d } = lift(b, current_env, v, fun)?;
                        exprs.push(Expr::Assign(temp, Box::new(main)));
                        exprs.extend(lookup);
                        defs.extend(d);
                    }
                    Statement::Raw(e) => {
                        let Program { main, defs: d } = lift(e, current_env, v, fun)?;
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
        expr::Expr::Operator(o) => {
            let id1 = fun.fresh();
            let id2 = fun.fresh();

            let f1 = FunctionDef {
                arg: String::from("b"),
                body: Expr::BinaryPrimitive(
                    *o,
                    Box::new(Expr::EnvLookup(0)),
                    Box::new(Expr::Variable(String::from("b"))),
                ),
                env: vec![String::from("a")],
                locals: HashSet::new(),
            };

            let f2 = FunctionDef {
                arg: String::from("a"),
                body: Expr::Closure(id1, vec![Expr::Variable(String::from("a"))]),
                env: Vec::new(),
                locals: HashSet::new(),
            };

            Ok(Program {
                defs: vec![f1, f2],
                main: Expr::Closure(id2, Vec::new()),
            })
        }
        expr::Expr::Match(_, _) => todo!(),
    }
}

pub fn locals(e: &self::Expr) -> HashSet<String> {
    match e {
        Expr::Variable(_)
        | Expr::Number(_)
        | Expr::Boolean(_)
        | Expr::RecordLookup(_, _)
        | Expr::EnvLookup(_) => HashSet::new(),
        Expr::Application(e1, e2) => locals(e1).union(locals(e2)),
        Expr::All(es) | Expr::Record(es) | Expr::Closure(_, es) => {
            es.iter().flat_map(locals).collect()
        }
        Expr::Ignore(e) => locals(e),
        Expr::Assign(v, e) => locals(e).update(v.clone()),
        Expr::If(e1, e2, e3) => locals(e1).union(locals(e2)).union(locals(e3)),
        Expr::BinaryPrimitive(_, e1, e2) => locals(e1).union(locals(e2)),
    }
}
