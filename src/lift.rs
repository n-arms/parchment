use super::expr::{self, Operator, Pattern, Statement};
use super::types::{Type, VarSet};
use im::{HashMap, HashSet};

type FunSet = VarSet<usize>;

#[derive(Clone, Debug)]
pub enum Expr {
    /// evaluate the expression on the left to a closure, then call it with the expression on the
    /// right
    Application(Box<Expr>, Box<Expr>),
    /// a variable introduced by a function call or a local variable assignment
    Variable(String),
    Number(f64),
    Integer(i64),
    Boolean(bool),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    /// a closure containing the function pointer and the env
    Closure(usize, Box<Expr>),
    /// evaluate all the expressions
    All(Vec<Expr>),
    /// ignore the result of an expression
    Ignore(Box<Expr>),
    /// assign the results of the expression to the given local variables. If an expression returns
    /// multiple values (using Expr::All), the nth value is assigned to the nth variable.
    /// Otherwise, the first variable has the value assigned to it
    Assign(String, Box<Expr>),
    /// evaluate all the expressions then pack them into a record on the heap. The same expression
    /// is used for building envs and ADT constructors
    Record(Vec<Expr>),
    /// look up the nth value in the record produced by evaluating e
    RecordLookup(Box<Expr>, usize),
    /// call the primative operator b with the two expressions
    BinaryPrimitive(Operator, Box<Expr>, Box<Expr>),
}

pub fn get_env() -> Expr {
    Expr::Variable(String::from("env"))
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

fn free<A>(e: &expr::Expr<A>) -> HashSet<String> {
    match e {
        expr::Expr::Function(pattern, body, _) => free(body).relative_complement(pattern.bound_vars()),
        expr::Expr::Application(e1, e2, _) => free(e1).union(free(e2)),
        expr::Expr::Constructor(..) | expr::Expr::Operator(..) | expr::Expr::Number(_) | expr::Expr::Boolean(_) => HashSet::new(),
        expr::Expr::Variable(var, _) => HashSet::unit(var.clone()),
        expr::Expr::Record(r) => r.values().flat_map(free).collect(),
        expr::Expr::Tuple(es) => {
            es.iter().flat_map(free).collect()
        }
        expr::Expr::If(p, c, a) => free(p).union(free(c)).union(free(a)),
        expr::Expr::Block(b) => free_block(b),
        expr::Expr::Match(_, _) => todo!(),
    }
}

fn free_block<A>(b: &[Statement<A>]) -> HashSet<String> {
    match b {
        [Statement::Let(pattern, body, _), rest @ ..] => free(body)
            .union(free_block(rest))
            .relative_complement(pattern.bound_vars()),
        [Statement::Raw(e), rest @ ..] => free(e).union(free_block(rest)),
        [Statement::TypeDef(..), rest @ ..] => free_block(rest),
        [] => HashSet::new(),
    }
}

pub fn to_lookup(p: &Pattern, base: Expr) -> Vec<Expr> {
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
        Pattern::Tuple(t) => t
            .iter()
            .enumerate()
            .flat_map(|(i, p)| to_lookup(p, Expr::RecordLookup(Box::new(base.clone()), i)))
            .collect(),
        Pattern::Construction(_, ps) => ps
            .iter()
            .enumerate()
            .flat_map(|(i, p)| to_lookup(p, Expr::RecordLookup(Box::new(base.clone()), i + 1)))
            .collect()
    }
}

pub struct ConstructorEnv {
    new: VarSet<u64>,
    interned: HashMap<String, u64>,
}

impl Default for ConstructorEnv {
    fn default() -> Self {
        ConstructorEnv {
            new: VarSet::new(|i| i as u64),
            interned: HashMap::new(),
        }
    }
}

impl ConstructorEnv {
    pub fn get_or_intern(&mut self, cons: &str) -> u64 {
        if let Some(idx) = self.interned.get(cons) {
            *idx
        } else {
            let new_var = self.new.fresh();
            self.interned.insert(cons.to_string(), new_var);
            new_var
        }
    }
}

pub fn lift(
    e: &expr::Expr<Type>,
    current_env: &[String],
    v: &VarSet<String>,
    fun: &FunSet,
    cons: &mut ConstructorEnv,
) -> Result<Program, ()> {
    match e {
        expr::Expr::Constructor(..) => todo!(),
        expr::Expr::Function(pattern, body, _) => {
            let (defs, ptr, env) = lift_function(pattern, body, None, current_env, v, fun, cons)?;

            Ok(Program {
                defs,
                main: Expr::Closure(ptr, Box::new(Expr::Record(env))),
            })
        }
        expr::Expr::Application(e1, e2, _) => {
            if let expr::Expr::Application(e3, e4, _) = e1.as_ref() {
                if let expr::Expr::Operator(o, _) = e3.as_ref() {
                    let mut p1 = lift(e4, current_env, v, fun, cons)?;
                    let Program { defs, main } = lift(e2, current_env, v, fun, cons)?;
                    p1.defs.extend(defs);
                    p1.main = Expr::BinaryPrimitive(*o, Box::new(p1.main), Box::new(main));
                    return Ok(p1);
                }
            }
            let mut p1 = lift(e1, current_env, v, fun, cons)?;
            let Program { defs, main } = lift(e2, current_env, v, fun, cons)?;

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
        expr::Expr::Variable(var, _) => {
            if let Ok(i) = current_env.binary_search(var) {
                Ok(Program {
                    defs: Vec::new(),
                    main: Expr::RecordLookup(Box::new(get_env()), i),
                })
            } else {
                Ok(Program {
                    defs: Vec::new(),
                    main: Expr::Variable(var.clone()),
                })
            }
        }
        expr::Expr::Record(r) => {
            let mut terms: Vec<(String, Program)> = r
                .iter()
                .map(|(var, e)| Ok((var.clone(), lift(e, current_env, v, fun, cons)?)))
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
        expr::Expr::Tuple(es) => {
            let mut defs = Vec::new();
            let mut terms = Vec::new();

            for e in es {
                let Program { defs: d, main } = lift(e, current_env, v, fun, cons)?;
                defs.extend(d);
                terms.push(main);
            }

            Ok(Program {
                defs,
                main: Expr::Record(terms),
            })
        }
        expr::Expr::If(p, c, a) => {
            let mut p1 = lift(p, current_env, v, fun, cons)?;
            let p2 = lift(c, current_env, v, fun, cons)?;
            let p3 = lift(a, current_env, v, fun, cons)?;
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
                    // this is the only form of recursive let statements that we support
                    Statement::Let(Pattern::Variable(var), expr::Expr::Function(pattern, body, _), _) => {
                        let (ds, ptr, env) =
                            lift_function(pattern, body, Some(var.clone()), current_env, v, fun, cons)?;
                        exprs.push(Expr::Assign(
                            var.clone(),
                            Box::new(Expr::Closure(ptr, Box::new(Expr::Record(env)))),
                        ));
                        defs.extend(ds);
                    }
                    Statement::Let(pattern, body, _) => {
                        let temp = v.fresh();
                        let lookup = to_lookup(pattern, Expr::Variable(temp.clone()));
                        let Program { main, defs: d } = lift(body, current_env, v, fun, cons)?;
                        exprs.push(Expr::Assign(temp, Box::new(main)));
                        exprs.extend(lookup);
                        defs.extend(d);
                    }
                    Statement::Raw(e) => {
                        let Program { main, defs: d } = lift(e, current_env, v, fun, cons)?;
                        exprs.push(if i == b.len() - 1 {
                            main
                        } else {
                            Expr::Ignore(Box::new(main))
                        });
                        defs.extend(d);
                    }
                    Statement::TypeDef(..) => ()
                }
            }
            Ok(Program {
                defs,
                main: Expr::All(exprs),
            })
        }
        expr::Expr::Operator(o, _) => {
            let id1 = fun.fresh();
            let id2 = fun.fresh();

            let f1 = FunctionDef {
                arg: String::from("b"),
                body: Expr::BinaryPrimitive(
                    *o,
                    Box::new(Expr::RecordLookup(Box::new(get_env()), 0)),
                    Box::new(Expr::Variable(String::from("b"))),
                ),
                env: vec![String::from("a")],
                locals: HashSet::new(),
            };

            let f2 = FunctionDef {
                arg: String::from("a"),
                body: Expr::Closure(
                    id1,
                    Box::new(Expr::Record(vec![Expr::Variable(String::from("a"))])),
                ),
                env: Vec::new(),
                locals: HashSet::new(),
            };

            Ok(Program {
                defs: vec![f1, f2],
                main: Expr::Closure(id2, Box::new(Expr::Record(Vec::new()))),
            })
        }
        expr::Expr::Match(_, _) => todo!(),
    }
}

fn lift_function(
    p: &Pattern,
    b: &expr::Expr<Type>,
    recuring_on: Option<String>,
    current_env: &[String],
    v: &VarSet<String>,
    fun: &FunSet,
    cons: &mut ConstructorEnv,
) -> Result<(Vec<FunctionDef>, usize, Vec<Expr>), ()> {
    let f_id = fun.fresh();
    let mut env: Vec<_> = free(b)
        .relative_complement(p.bound_vars())
        .into_iter()
        .collect();
    env.sort();

    let mut body = lift(b, &env, v, fun, cons)?;

    let (mut lookup, arg) = if let Pattern::Variable(var) = p {
        (vec![], var.clone())
    } else {
        let arg = v.fresh();

        (to_lookup(p, Expr::Variable(arg.clone())), arg)
    };
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
            if Some(&name) == recuring_on.as_ref() {
                Expr::Closure(f_id, Box::new(Expr::Variable(String::from("working_env"))))
            } else if let Ok(i) = current_env.binary_search(&name) {
                Expr::RecordLookup(Box::new(get_env()), i)
            } else {
                Expr::Variable(name)
            }
        })
        .collect();
    Ok((body.defs, f_id, new_env))
}

pub fn locals(e: &Expr) -> HashSet<String> {
    match e {
        Expr::Variable(_)
        | Expr::Number(_)
        | Expr::Boolean(_)
        | Expr::Integer(_)
        | Expr::RecordLookup(_, _) => HashSet::new(),
        Expr::Application(e1, e2) => locals(e1).union(locals(e2)),
        Expr::All(es) | Expr::Record(es) => es.iter().flat_map(locals).collect(),
        Expr::Ignore(e) | Expr::Closure(_, e) => locals(e),
        Expr::Assign(v, e) => locals(e).update(v.clone()),
        Expr::If(e1, e2, e3) => locals(e1).union(locals(e2)).union(locals(e3)),
        Expr::BinaryPrimitive(_, e1, e2) => locals(e1).union(locals(e2)),
    }
}
