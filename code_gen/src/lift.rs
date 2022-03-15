use expr::{
    expr::{Operator, Pattern, Statement},
    types::{Fresh, Type, TypeDef},
};
use im::{HashMap, HashSet};

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

#[derive(Clone, Debug)]
pub struct State {
    /// a source of unused variables
    free_vars: Fresh,
    /// a source of unused function indeces
    free_funs: Fresh,
    /// a mapping from type names to defined types
    type_defs: HashMap<String, TypeDef>,
}

impl State {
    pub fn new(type_defs: HashMap<String, TypeDef>) -> Self {
        println!("making state with type defs {:?}", type_defs);
        Self {
            free_vars: Fresh::default(),
            free_funs: Fresh::default(),
            type_defs,
        }
    }
    pub fn fresh_var(&self) -> String {
        self.free_vars.fresh().to_string()
    }

    pub fn fresh_fun(&self) -> usize {
        self.free_funs.fresh()
    }

    /// Looks up a variant. Returns the types of its arguments and its variant index
    ///
    /// # Panics
    /// `lookup_variant` will panic if it is given either a type name or a variant name that it is
    /// not aware of. This should not be a problem, as all type and variant names should be
    /// detected during type inference.
    #[allow(clippy::expect_used)]
    pub fn lookup_variant(&self, type_name: &str, variant_name: &str) -> (Vec<Type>, usize) {
        println!("type name {} and type defs {:?}", type_name, self.type_defs);
        let TypeDef { variants, .. } = self
            .type_defs
            .get(type_name)
            .expect("user defined types should be known");

        let mut ordered_variants: Vec<_> = variants.iter().cloned().collect();
        ordered_variants.sort_by_key(|var| var.constructor.clone());

        let idx = ordered_variants
            .binary_search_by_key(&variant_name, |v| &v.constructor)
            .expect("variants should be valid");

        (ordered_variants[idx].fields.clone(), idx)
    }
}

#[allow(clippy::too_many_lines)]
pub fn lift(e: &expr::expr::Expr<Type>, current_env: &[String], st: &State) -> Program {
    match e {
        expr::expr::Expr::Function(pattern, body, _) => {
            let (defs, ptr, env) = lift_function(pattern, body, None, current_env, st);

            Program {
                defs,
                main: Expr::Closure(ptr, Box::new(Expr::Record(env))),
            }
        }
        expr::expr::Expr::Application(e1, e2, _) => {
            if let expr::expr::Expr::Application(e3, e4, _) = e1.as_ref() {
                if let expr::expr::Expr::Operator(o, _) = e3.as_ref() {
                    let mut p1 = lift(e4, current_env, st);
                    let Program { defs, main } = lift(e2, current_env, st);
                    p1.defs.extend(defs);
                    p1.main = Expr::BinaryPrimitive(*o, Box::new(p1.main), Box::new(main));
                    return p1;
                }
            }
            let mut p1 = lift(e1, current_env, st);
            let Program { defs, main } = lift(e2, current_env, st);

            p1.defs.extend(defs);
            p1.main = Expr::Application(Box::new(p1.main), Box::new(main));
            p1
        }
        expr::expr::Expr::Number(n) => Program {
            defs: Vec::new(),
            main: Expr::Number(*n),
        },
        expr::expr::Expr::Boolean(b) => Program {
            defs: Vec::new(),
            main: Expr::Boolean(*b),
        },
        expr::expr::Expr::Variable(var, _) => {
            if let Ok(i) = current_env.binary_search(var) {
                Program {
                    defs: Vec::new(),
                    main: Expr::RecordLookup(Box::new(get_env()), i),
                }
            } else {
                Program {
                    defs: Vec::new(),
                    main: Expr::Variable(var.clone()),
                }
            }
        }
        expr::expr::Expr::Record(record) => {
            let mut terms: Vec<_> = record.iter().collect();
            terms.sort_by_key(|(var, _)| *var);

            let (defs, lifted_terms) =
                lift_all(terms.iter().map(|(_, value)| *value), current_env, st);

            Program {
                defs,
                main: Expr::Record(lifted_terms),
            }
        }
        expr::expr::Expr::Tuple(tuple) => {
            let (defs, terms) = lift_all(tuple.iter(), current_env, st);

            Program {
                defs,
                main: Expr::Record(terms),
            }
        }
        expr::expr::Expr::If(p, c, a) => {
            let mut p1 = lift(p, current_env, st);
            let p2 = lift(c, current_env, st);
            let p3 = lift(a, current_env, st);
            p1.defs.extend(p2.defs);
            p1.defs.extend(p3.defs);
            p1.main = Expr::If(Box::new(p1.main), Box::new(p2.main), Box::new(p3.main));
            p1
        }
        expr::expr::Expr::Block(block) => lift_block(block, current_env, st),
        expr::expr::Expr::Operator(o, _) => {
            let id1 = st.fresh_fun();
            let id2 = st.fresh_fun();

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

            Program {
                defs: vec![f1, f2],
                main: Expr::Closure(id2, Box::new(Expr::Record(Vec::new()))),
            }
        }
        /*
        expr::expr::Expr::Constructor(cons, cons_type) => {
            println!("lifting constructor {:?} with type {:?}", cons, cons_type);
            let (num_args, index) = st.lookup_variant(variant_type(cons_type), cons);

            let mut defs = Vec::new();
            let mut head = Expr::Record({
                let mut elems = vec![Expr::Integer(index as i64)];
                elems.extend((0..num_args).rev().map(|index| Expr::RecordLookup(Box::new(get_env()), index)));
                elems
            });

            for i in 0..num_args {
                defs.push(FunctionDef {
                    arg: String::from("variant_arg"),
                    env: (0..i).map(|_| String::from("variant_arg")).collect(),
                    locals: HashSet::new(),
                    body: head
                });

                let id = st.fresh_fun();

                head = Expr::Closure(id, Box::new(Expr::Record(((num_args - i - 1)..num_args).map(|idx| Expr::RecordLookup(Box::new(get_env()), idx)).collect())));
            }

            Program {
                defs,
                main: head
            }
        }
        */
        expr::expr::Expr::Constructor(cons, typ) => {
            let cons_type = variant_type(typ);
            let (arg_types, index) = st.lookup_variant(cons_type, cons);
            let args: Vec<_> = (0..arg_types.len())
                .map(|_| st.fresh_var())
                .zip(arg_types.into_iter())
                .collect();

            let mut desugared = expr::expr::Expr::Record({
                let mut terms = vec![expr::expr::Expr::Number(index as f64)];
                terms.extend(
                    args.iter()
                        .map(|(name, t)| expr::expr::Expr::Variable(name.clone(), t.clone())),
                );
                (0..terms.len())
                    .map(|i| i.to_string())
                    .zip(terms.into_iter())
                    .collect()
            });

            for (arg, arg_type) in args.into_iter().rev() {
                desugared = expr::expr::Expr::Function(
                    Pattern::Variable(arg),
                    Box::new(desugared),
                    arg_type,
                );
            }
            //let desugared = arg_types.into_iter().rev().rfold(final_type.clone(), |total, t|
            lift(&desugared, current_env, st)
        }
        expr::expr::Expr::Match(..) => todo!(),
    }
}

fn lift_all<'a>(
    exprs: impl Iterator<Item = &'a expr::expr::Expr<Type>>,
    current_env: &[String],
    st: &State,
) -> (Vec<FunctionDef>, Vec<Expr>) {
    let mut defs = Vec::new();
    let mut lifted_exprs = Vec::new();

    for expr in exprs {
        let Program { defs: d, main } = lift(expr, current_env, st);

        defs.extend(d);
        lifted_exprs.push(main);
    }

    (defs, lifted_exprs)
}

fn lift_block(block: &[Statement<Type>], current_env: &[String], st: &State) -> Program {
    let mut defs = Vec::new();
    let mut exprs = Vec::new();
    for (i, stmt) in block.iter().enumerate() {
        match stmt {
            // this is the only form of recursive let statements that we support
            Statement::Let(
                Pattern::Variable(var),
                expr::expr::Expr::Function(pattern, body, _),
                _,
            ) => {
                let (ds, ptr, env) = lift_function(pattern, body, Some(var), current_env, st);
                exprs.push(Expr::Assign(
                    var.clone(),
                    Box::new(Expr::Closure(ptr, Box::new(Expr::Record(env)))),
                ));
                defs.extend(ds);
            }
            Statement::Let(pattern, body, _) => {
                let temp = st.fresh_var();
                let lookup = to_lookup(pattern, Expr::Variable(temp.clone()));
                let Program { main, defs: d } = lift(body, current_env, st);
                exprs.push(Expr::Assign(temp, Box::new(main)));
                exprs.extend(lookup);
                defs.extend(d);
            }
            Statement::Raw(inner) => {
                let Program { main, defs: d } = lift(inner, current_env, st);
                exprs.push(if i == block.len() - 1 {
                    main
                } else {
                    Expr::Ignore(Box::new(main))
                });
                defs.extend(d);
            }
            Statement::TypeDef(..) => (),
        }
    }
    Program {
        defs,
        main: Expr::All(exprs),
    }
}

fn lift_function(
    p: &Pattern,
    b: &expr::expr::Expr<Type>,
    recuring_on: Option<&str>,
    current_env: &[String],
    st: &State,
) -> (Vec<FunctionDef>, usize, Vec<Expr>) {
    let f_id = st.fresh_fun();
    let mut env: Vec<_> = free(b)
        .relative_complement(p.bound_vars())
        .into_iter()
        .collect();
    env.sort();

    let mut body = lift(b, &env, st);

    let (mut lookup, arg) = if let Pattern::Variable(var) = p {
        (vec![], var.clone())
    } else {
        let arg = st.fresh_var();

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
            if Some(&name[..]) == recuring_on {
                Expr::Closure(f_id, Box::new(Expr::Variable(String::from("working_env"))))
            } else if let Ok(i) = current_env.binary_search(&name) {
                Expr::RecordLookup(Box::new(get_env()), i)
            } else {
                Expr::Variable(name)
            }
        })
        .collect();
    (body.defs, f_id, new_env)
}

pub fn locals(e: &Expr) -> HashSet<String> {
    match e {
        Expr::Variable(_)
        | Expr::Number(_)
        | Expr::Boolean(_)
        | Expr::Integer(_)
        | Expr::RecordLookup(_, _) => HashSet::new(),
        Expr::Application(e1, e2) | Expr::BinaryPrimitive(_, e1, e2) => {
            locals(e1).union(locals(e2))
        }
        Expr::All(es) | Expr::Record(es) => es.iter().flat_map(locals).collect(),
        Expr::Ignore(e) | Expr::Closure(_, e) => locals(e),
        Expr::Assign(v, e) => locals(e).update(v.clone()),
        Expr::If(e1, e2, e3) => locals(e1).union(locals(e2)).union(locals(e3)),
    }
}

fn free<A>(e: &expr::expr::Expr<A>) -> HashSet<String> {
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
        expr::expr::Expr::Match(_, _) => todo!(),
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
        Pattern::Variable(var) => vec![Expr::Assign(var.clone(), Box::new(base))],
        Pattern::Record(record) => {
            let mut fields: Vec<_> = record.iter().collect();
            fields.sort_by_key(|(name, _)| *name);
            fields
                .into_iter()
                .enumerate()
                .flat_map(|(i, (_, value))| {
                    to_lookup(value, Expr::RecordLookup(Box::new(base.clone()), i))
                })
                .collect()
        }
        Pattern::Tuple(tuple) => tuple
            .iter()
            .enumerate()
            .flat_map(|(i, elem)| to_lookup(elem, Expr::RecordLookup(Box::new(base.clone()), i)))
            .collect(),
        Pattern::Construction(_, args) => args
            .iter()
            .enumerate()
            .flat_map(|(i, elem)| {
                to_lookup(elem, Expr::RecordLookup(Box::new(base.clone()), i + 1))
            })
            .collect(),
    }
}

/// given the type signature of a variant, return what user define type it belongs to
///
/// # Panics
/// `variant_type` expects the type signature of a variant. If it is given a type signature that is
/// not either an application, a constant, or a function that produces a valid variant signature,
/// it will panic.
fn variant_type(t: &Type) -> &str {
    match t {
        Type::Constant(var, _) => var.as_ref(),
        Type::Application(left, _) => variant_type(left),
        Type::Arrow(_, right) => variant_type(right),
        _ => panic!("illegal variant type {}", t),
    }
}
