use expr::{
    expr::{Expr, Pattern, Statement},
    types::{bool_type, Constraint, Fresh, Kind, Type, TypeDef, TypeError, Var},
};
use im::{HashMap, HashSet};
use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Assumption(String, Var);

/// all the mutable and read only state needed for generating constraints
#[derive(Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct State {
    /// fresh type variables
    fresh_type_vars: Fresh,
    /// type variables introduced by the bindings in a lambda
    monotonic_type_vars: HashSet<Var>,
    /// the set of global constraints
    constraints: Rc<RefCell<HashSet<Constraint>>>,
    /// the mapping from type names to type definitions
    type_defs: Rc<RefCell<HashMap<String, TypeDef>>>,
    /// the mapping from variant names to type names
    variants: Rc<RefCell<HashMap<String, String>>>,
}

impl State {
    pub fn extract(self) -> (Fresh, HashSet<Constraint>, HashMap<String, TypeDef>) {
        (
            self.fresh_type_vars,
            self.constraints.take(),
            self.type_defs.take(),
        )
    }

    pub fn fresh(&self) -> Var {
        Rc::new(self.fresh_type_vars.fresh().to_string())
    }

    pub fn constrain(&self, c: Constraint) {
        self.constraints.borrow_mut().insert(c);
    }

    pub fn equate(&self, t1: Type, t2: Type) {
        self.constrain(Constraint::Equality(t1, t2));
    }

    pub fn instance(&self, super_type: Type, sub_type: Type) {
        self.constrain(Constraint::InstanceOf(
            sub_type,
            self.monotonic_type_vars.clone(),
            super_type,
        ));
    }

    pub fn type_vars(&self) -> &Fresh {
        &self.fresh_type_vars
    }

    pub fn add_monotonic_type_vars(&self, tvs: HashSet<Var>) -> Self {
        State {
            fresh_type_vars: self.fresh_type_vars.clone(),
            constraints: Rc::clone(&self.constraints),
            type_defs: Rc::clone(&self.type_defs),
            variants: Rc::clone(&self.variants),
            monotonic_type_vars: self.monotonic_type_vars.clone().union(tvs),
        }
    }

    pub fn define_type(&self, name: String, type_def: TypeDef) {
        self.variants.borrow_mut().extend(
            type_def
                .variants
                .iter()
                .map(|v| (v.constructor.clone(), name.clone()))
                .collect::<HashMap<_, _>>(),
        );
        self.type_defs.borrow_mut().insert(name, type_def);
    }

    /// Get the type of the given constructor.
    ///
    /// # Example
    /// ```parchment
    /// {
    ///     type box a = Box a;
    /// }
    /// ```
    /// `lookup_variant("Box") = a -> box a`
    pub fn lookup_variant(&self, constructor: &str) -> Option<Type> {
        let type_name = self.variants.borrow().get(constructor)?.clone();
        let type_def = self.type_defs.borrow().get(&type_name)?.clone();
        let result_kind = type_def
            .polymorphic_vars
            .iter()
            .rfold(Kind::Star, |base, _| {
                Kind::Arrow(Box::new(Kind::Star), Box::new(base))
            });
        let result_type = type_def.polymorphic_vars.iter().fold(
            Type::Constant(Rc::new(type_name), result_kind),
            |total, var| {
                Type::Application(
                    Rc::new(total),
                    Rc::new(Type::Variable(Rc::clone(&var.0), var.1.clone())),
                )
            },
        );
        let variant = type_def
            .variants
            .iter()
            .find(|variant| variant.constructor == constructor)?;
        Some(variant.fields.iter().rfold(result_type, |total, field| {
            Type::Arrow(Rc::new(field.clone()), Rc::new(total))
        }))
    }

    /// Return the most specific type that can be infered for the pattern, as well as a mapping
    /// from variable names to type variables.
    ///
    /// # Errors
    /// Currently `type_pattern` will not return an error, but when support for variants is added,
    /// it will return an error if it comes across an unknown variant.
    pub fn type_pattern(
        &self,
        refutable: bool,
        pattern: &Pattern<()>,
    ) -> Result<(HashMap<String, Var>, Pattern<Type>), TypeError> {
        match pattern {
            Pattern::Variable(v, ()) => {
                let b = self.fresh();
                Ok((
                    HashMap::unit(v.clone(), Rc::clone(&b)),
                    Pattern::Variable(v.clone(), Type::Variable(b, Kind::Star)),
                ))
            }
            Pattern::Record(r) => {
                let mut bindings = HashMap::new();
                let mut record = HashMap::new();
                for (var, val) in r {
                    let (b, t) = self.type_pattern(refutable, val)?;
                    bindings.extend(b);
                    record.insert(var.clone(), t);
                }
                Ok((bindings, Pattern::Record(record)))
            }
            Pattern::Tuple(ps) => {
                let mut bindings = HashMap::new();
                let mut terms = Vec::new();
                for p in ps {
                    let (b1, p1) = self.type_pattern(refutable, p)?;
                    bindings.extend(b1);
                    terms.push(p1);
                }
                Ok((bindings, Pattern::Tuple(terms)))
            }
            Pattern::Construction(constructor, ps, ()) => {
                let (type_def, variant) = self
                    .type_defs
                    .borrow()
                    .iter()
                    .find_map(|(_, td)| {
                        Some((
                            td.clone(),
                            td.variants
                                .iter()
                                .find(|v| &v.constructor == constructor)?
                                .clone(),
                        ))
                    })
                    .ok_or_else(|| TypeError::UnknownVariant(constructor.clone()))?;

                if type_def.variants.len() > 1 && !refutable {
                    return Err(TypeError::RefutablePattern(pattern.clone()));
                }
                if variant.fields.len() != ps.len() {
                    return Err(TypeError::FieldMismatch(variant, pattern.clone()));
                }

                let t1 = self
                    .lookup_variant(constructor)
                    .ok_or_else(|| TypeError::UnknownVariant(constructor.clone()))?;

                let beta = self.fresh();

                let (bindings, typed_ps): (Vec<_>, Vec<_>) = ps
                    .iter()
                    .map(|p| self.type_pattern(refutable, p))
                    .collect::<Result<Vec<(HashMap<String, Var>, Pattern<Type>)>, _>>()?
                    .into_iter()
                    .unzip();

                let t2 = typed_ps
                    .iter()
                    .rev()
                    .rfold(Type::Variable(Rc::clone(&beta), Kind::Star), |total, t| {
                        Type::Arrow(Rc::new(t.get_type()), Rc::new(total))
                    });

                self.instance(t1, t2);

                Ok((
                    bindings.into_iter().flatten().collect(),
                    Pattern::Construction(
                        constructor.clone(),
                        typed_ps,
                        Type::Variable(beta, Kind::Star),
                    ),
                ))
            }
        }
    }

    /// Annotates an untyped expression with types, and generates the constraints on those types.
    ///
    /// # Errors
    /// `generate` will return an error of it encounters an unknown variant
    #[allow(clippy::too_many_lines)]
    pub fn generate(&self, e: &Expr<()>) -> Result<(HashSet<Assumption>, Expr<Type>), TypeError> {
        match e {
            Expr::Function(pattern, body, ()) => {
                let (bindings, typed_pattern) = self.type_pattern(false, pattern)?;

                let (mut a1, typed_body) = self
                    .add_monotonic_type_vars(
                        bindings.iter().map(|(_, tv)| tv.as_ref().clone()).collect(),
                    )
                    .generate(body)?;

                for Assumption(v, tv) in &a1 {
                    if let Some(tv2) = bindings.get(v) {
                        self.equate(
                            Type::Variable(Rc::clone(tv), Kind::Star),
                            Type::Variable(Rc::clone(tv2), Kind::Star),
                        );
                    }
                }

                a1.retain(|Assumption(var, _)| !bindings.contains_key(var));

                let pattern_type = typed_pattern.get_type();

                Ok((
                    a1,
                    Expr::Function(typed_pattern, Box::new(typed_body), pattern_type),
                ))
            }
            Expr::Application(left, right, ()) => {
                let (a1, typed_left) = self.generate(left)?;
                let (mut a2, typed_right) = self.generate(right)?;
                a2.extend(a1);

                let beta = self.fresh();

                self.equate(
                    typed_left.get_type(),
                    Type::Arrow(
                        Rc::new(typed_right.get_type()),
                        Rc::new(Type::Variable(Rc::clone(&beta), Kind::Star)),
                    ),
                );
                Ok((
                    a2,
                    Expr::Application(
                        Box::new(typed_left),
                        Box::new(typed_right),
                        Type::Variable(beta, Kind::Star),
                    ),
                ))
            }
            Expr::Number(n) => Ok((HashSet::new(), Expr::Number(*n))),
            Expr::Boolean(b) => Ok((HashSet::new(), Expr::Boolean(*b))),
            Expr::Operator(operator, ()) => Ok((
                HashSet::new(),
                Expr::Operator(*operator, operator.get_type()),
            )),
            Expr::Variable(var, ()) => {
                let beta = self.fresh();
                let a = HashSet::unit(Assumption(var.clone(), Rc::clone(&beta)));
                Ok((
                    a,
                    Expr::Variable(var.clone(), Type::Variable(beta, Kind::Star)),
                ))
            }
            Expr::Record(record) => {
                let mut a = HashSet::new();
                let mut typed_record = HashMap::new();

                for (var, val) in record {
                    let (a1, typed_val) = self.generate(val)?;
                    a.extend(a1);
                    typed_record.insert(var.clone(), typed_val);
                }

                Ok((a, Expr::Record(typed_record)))
            }
            Expr::Tuple(tuple) => {
                let mut a = HashSet::new();
                let mut typed_tuple = Vec::new();

                for val in tuple {
                    let (a1, typed_val) = self.generate(val)?;
                    a.extend(a1);
                    typed_tuple.push(typed_val);
                }

                Ok((a, Expr::Tuple(typed_tuple)))
            }
            Expr::If(pred, cons, altr) => {
                let (mut a1, typed_pred) = self.generate(pred)?;
                let (a2, typed_cons) = self.generate(cons)?;
                let (a3, typed_altr) = self.generate(altr)?;

                a1.extend(a2);
                a1.extend(a3);

                self.equate(typed_cons.get_type(), typed_altr.get_type());

                self.equate(typed_pred.get_type(), bool_type());

                Ok((
                    a1,
                    Expr::If(
                        Box::new(typed_pred),
                        Box::new(typed_cons),
                        Box::new(typed_altr),
                    ),
                ))
            }
            Expr::Block(block) => {
                let (a, typed_block) = self.generate_block(block)?;
                Ok((a, Expr::Block(typed_block)))
            }
            // type box a = Box a;
            // Box :: b, {b < a -> box a}
            Expr::Constructor(constructor, ()) => {
                let cons_type = self
                    .lookup_variant(constructor)
                    .ok_or_else(|| TypeError::UnknownVariant(constructor.clone()))?;
                let beta = Type::Variable(self.fresh(), Kind::Star);
                self.instance(cons_type, beta.clone());
                Ok((HashSet::new(), Expr::Constructor(constructor.clone(), beta)))
            }
            Expr::Match(matchand, arms, ()) => {
                let (mut a1, typed_matchand) = self.generate(matchand)?;
                let mut typed_arms = Vec::new();
                let beta = self.fresh();

                for (pattern, expr) in arms {
                    let (bindings, typed_pattern) = self.type_pattern(true, pattern)?;
                    self.instance(typed_matchand.get_type(), typed_pattern.get_type());

                    let (mut a, typed_expr) = self.generate(expr)?;

                    self.equate(
                        Type::Variable(Rc::clone(&beta), Kind::Star),
                        typed_expr.get_type(),
                    );

                    typed_arms.push((typed_pattern, typed_expr));

                    for Assumption(var, tv1) in &a {
                        if let Some(tv2) = bindings.get(var) {
                            self.instance(
                                Type::Variable(Rc::clone(tv2), Kind::Star),
                                Type::Variable(Rc::clone(tv1), Kind::Star),
                            );
                        }
                    }

                    a.retain(|Assumption(var, _)| !bindings.contains_key(var));
                    a1.extend(a);
                }

                Ok((
                    a1,
                    Expr::Match(
                        Box::new(typed_matchand),
                        typed_arms,
                        Type::Variable(beta, Kind::Star),
                    ),
                ))
            }
        }
    }

    fn generate_block(
        &self,
        statements: &[Statement<()>],
    ) -> Result<(HashSet<Assumption>, Vec<Statement<Type>>), TypeError> {
        match statements {
            [] => Ok((HashSet::new(), vec![])),
            [Statement::Raw(expr)] => {
                let (a, typed_expr) = self.generate(expr)?;
                Ok((a, vec![Statement::Raw(typed_expr)]))
            }
            [Statement::Raw(expr), rest @ ..] => {
                let (mut a1, typed_expr) = self.generate(expr)?;
                let (a2, mut typed_rest) = self.generate_block(rest)?;
                typed_rest.insert(0, Statement::Raw(typed_expr));
                a1.extend(a2);
                Ok((a1, typed_rest))
            }
            [Statement::Let(Pattern::Variable(var, ()), body, ()), rest @ ..] => {
                let beta = self.fresh();
                let (a1, typed_body) = self.generate(body)?;
                let (mut a2, mut typed_rest) = self.generate_block(rest)?;
                self.equate(
                    Type::Variable(Rc::clone(&beta), Kind::Star),
                    typed_body.get_type(),
                );

                for Assumption(var1, tv) in &a2 {
                    if var1 == var {
                        self.instance(
                            Type::Variable(Rc::clone(&beta), Kind::Star),
                            Type::Variable(Rc::clone(tv), Kind::Star),
                        );
                    }
                }

                a2.extend(a1);
                a2.retain(|Assumption(var1, _)| var1 != var);

                typed_rest.insert(
                    0,
                    Statement::Let(
                        Pattern::Variable(
                            var.clone(),
                            Type::Variable(Rc::clone(&beta), Kind::Star),
                        ),
                        typed_body,
                        Type::Variable(beta, Kind::Star),
                    ),
                );

                Ok((a2, typed_rest))
            }
            [Statement::Let(pattern, body, ()), rest @ ..] => {
                let (bindings, typed_pattern) = self.type_pattern(false, pattern)?;
                let (a1, typed_body) = self.generate(body)?;
                let (mut a2, mut typed_rest) = self.generate_block(rest)?;

                self.equate(typed_pattern.get_type(), typed_body.get_type());

                for Assumption(var, tv1) in &a2 {
                    if let Some(tv2) = bindings.get(var) {
                        self.instance(
                            Type::Variable(Rc::clone(tv2), Kind::Star),
                            Type::Variable(Rc::clone(tv1), Kind::Star),
                        );
                    }
                }

                a2.retain(|Assumption(var, _)| !bindings.contains_key(var));
                a2.extend(a1);

                let pattern_type = typed_pattern.get_type();

                typed_rest.insert(0, Statement::Let(typed_pattern, typed_body, pattern_type));

                Ok((a2, typed_rest))
            }
            [Statement::TypeDef(alias, type_vars, variants), rest @ ..] => {
                self.define_type(
                    alias.clone(),
                    TypeDef {
                        polymorphic_vars: type_vars
                            .iter()
                            .map(|var| (Rc::clone(var), Kind::Star))
                            .collect(),
                        variants: variants.clone(),
                    },
                );
                self.generate_block(rest)
            }
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "constraints {{")?;
        for cons in self.constraints.borrow().iter() {
            match cons {
                Constraint::Equality(left, right) => writeln!(f, "  {} = {}", left, right),
                Constraint::InstanceOf(sub_type, m, super_type) => {
                    writeln!(f, "  {} < {} [{:?}]", sub_type, super_type, m)
                }
            }?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

impl Display for Assumption {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} : {}", self.0, self.1)
    }
}