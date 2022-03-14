use super::expr::*;
use super::types::*;
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
    type_defs: HashMap<String, TypeDef>,
    /// the mapping from variant names to type names
    variants: HashMap<String, String>,
}

impl State {
    pub fn extract(self) -> (Fresh, HashSet<Constraint>) {
        (self.fresh_type_vars, self.constraints.take())
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

    pub fn add_monotonic_type_vars(&self, tvs: HashSet<Var>) -> Self {
        State {
            fresh_type_vars: self.fresh_type_vars.clone(),
            constraints: self.constraints.clone(),
            type_defs: self.type_defs.clone(),
            variants: self.variants.clone(),
            monotonic_type_vars: self.monotonic_type_vars.clone().union(tvs),
        }
    }

    pub fn define_type(&self, name: String, type_def: TypeDef) -> Self {
        State {
            fresh_type_vars: self.fresh_type_vars.clone(),
            constraints: self.constraints.clone(),
            monotonic_type_vars: self.monotonic_type_vars.clone().update(Rc::new(name.clone())),
            variants: type_def
                .variants
                .iter()
                .map(|v| (v.constructor.clone(), name.clone()))
                .collect(),
            type_defs: self.type_defs.update(name, type_def),
        }
    }

    pub fn lookup_variant(&self, constructor: &str) -> Option<Type> {
        let type_name = self.variants.get(constructor)?;
        let type_def = self.type_defs.get(type_name)?;
        let result_kind = type_def
            .polymorphic_vars
            .iter()
            .rfold(Kind::Star, |base, _| Kind::Arrow(Box::new(Kind::Star), Box::new(base)));
        let result_type = type_def
            .polymorphic_vars
            .iter()
            .fold(Type::Variable(Rc::new(type_name.clone()), result_kind), |total, var| {
                Type::Application(Rc::new(total), Rc::new(Type::Variable(var.0.clone(), var.1.clone())))
            });
        let variant = type_def
            .variants
            .iter()
            .find(|variant| variant.constructor == constructor)?;
        Some(variant.fields.iter().rfold(result_type, |total, field| {
            Type::Arrow(Rc::new(field.clone()), Rc::new(total))
        }))
    }

    pub fn generate(&self, e: &Expr<()>) -> Result<(HashSet<Assumption>, Expr<Type>), TypeError> {
        match e {
            Expr::Function(pattern, body, ()) => {
                let (bindings, typed_pattern) = pattern.type_pattern(self)?;

                let (mut a1, typed_body) = self
                    .add_monotonic_type_vars(
                        bindings.iter().map(|(_, tv)| tv.as_ref().clone()).collect(),
                    )
                    .generate(body)?;

                for Assumption(v, tv) in &a1 {
                    if let Some(tv2) = bindings.get(v) {
                        self.equate(
                            Type::Variable(tv.clone(), Kind::Star),
                            Type::Variable(tv2.clone(), Kind::Star),
                        );
                    }
                }

                a1.retain(|Assumption(var, _)| !bindings.contains_key(var));

                Ok((
                    a1,
                    Expr::Function(pattern.clone(), Box::new(typed_body), typed_pattern),
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
                        Rc::new(Type::Variable(beta.clone(), Kind::Star)),
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
                let a = HashSet::unit(Assumption(var.clone(), beta.clone()));
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
                    .ok_or(TypeError::UnknownVariant(constructor.clone()))?;
                let beta = Type::Variable(self.fresh(), Kind::Star);
                self.instance(cons_type, beta.clone());
                Ok((HashSet::new(), Expr::Constructor(constructor.clone(), beta)))
            }
            Expr::Match(_, _) => todo!(),
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
            [Statement::Let(Pattern::Variable(var), body, ()), rest @ ..] => {
                let beta = self.fresh();
                let (a1, typed_body) = self.generate(body)?;
                let (mut a2, mut typed_rest) = self.generate_block(rest)?;
                self.equate(
                    Type::Variable(beta.clone(), Kind::Star),
                    typed_body.get_type(),
                );

                for Assumption(var1, tv) in &a2 {
                    if var1 == var {
                        self.instance(
                            Type::Variable(beta.clone(), Kind::Star),
                            Type::Variable(tv.clone(), Kind::Star),
                        );
                    }
                }

                a2.extend(a1);
                a2.retain(|Assumption(var1, _)| var1 != var);

                typed_rest.insert(
                    0,
                    Statement::Let(
                        Pattern::Variable(var.clone()),
                        typed_body,
                        Type::Variable(beta.clone(), Kind::Star),
                    ),
                );

                Ok((a2, typed_rest))
            }
            [Statement::Let(pattern, body, ()), rest @ ..] => {
                let (bindings, typed_pattern) = pattern.type_pattern(self)?;
                let (a1, typed_body) = self.generate(body)?;
                let (mut a2, mut typed_rest) = self.generate_block(rest)?;

                self.equate(typed_pattern.clone(), typed_body.get_type());

                for Assumption(var, tv1) in &a2 {
                    if let Some(tv2) = bindings.get(var) {
                        self.instance(
                            Type::Variable(tv2.clone(), Kind::Star),
                            Type::Variable(tv1.clone(), Kind::Star),
                        );
                    }
                }

                a2.retain(|Assumption(var, _)| !bindings.contains_key(var));
                a2.extend(a1);

                typed_rest.insert(
                    0,
                    Statement::Let(pattern.clone(), typed_body, typed_pattern),
                );

                Ok((a2, typed_rest))
            }
            [Statement::TypeDef(alias, type_vars, variants), rest @ ..] => self
                .define_type(
                    alias.clone(),
                    TypeDef {
                        polymorphic_vars: type_vars
                            .iter()
                            .map(|var| (var.clone(), Kind::Star))
                            .collect(),
                        variants: variants.clone(),
                    },
                )
                .generate_block(rest),
        }
    }
}

impl Display for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "constraints {{")?;
        for cons in self.constraints.borrow().iter() {
            match cons {
                Constraint::Equality(left, right) => writeln!(f, "  {} = {}", left, right),
                Constraint::InstanceOf(sub, m, sup) => writeln!(f, "  {} < {} [{:?}]", sub, sup, m),
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
