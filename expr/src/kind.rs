use super::types::Type;
use im::HashMap;
use std::rc::Rc;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Kind {
    arity: usize,
}

#[derive(Clone, Debug)]
pub enum KindError {
    UnknownTypeVariable(Rc<String>),
    ArityNotZero(Type<Kind>),
    ArityIsZero(Type<Kind>),
}

impl Kind {
    pub fn new(arity: usize) -> Self {
        Kind { arity }
    }

    pub fn annotate(
        t: &Type<()>,
        env: &HashMap<Rc<String>, Kind>,
    ) -> Result<Type<Kind>, KindError> {
        match t {
            Type::Constant(cons, ()) => {
                if let Some(cons_kind) = env.get(cons) {
                    Ok(Type::Constant(Rc::clone(cons), *cons_kind))
                } else {
                    Err(KindError::UnknownTypeVariable(Rc::clone(cons)))
                }
            }
            Type::Variable(var, ()) => {
                if let Some(var_kind) = env.get(var) {
                    Ok(Type::Variable(Rc::clone(var), *var_kind))
                } else {
                    Err(KindError::UnknownTypeVariable(Rc::clone(var)))
                }
            }
            Type::Arrow(left, right) => {
                let kinded_left = Kind::arity_is_zero(Kind::annotate(left, env)?)?;
                let kinded_right = Kind::arity_is_zero(Kind::annotate(right, env)?)?;
                Ok(Type::Arrow(Rc::new(kinded_left), Rc::new(kinded_right)))
            }
            Type::Application(left, right, ()) => {
                let kinded_left = Kind::annotate(left, env)?;
                let kinded_right = Kind::arity_is_zero(Kind::annotate(right, env)?)?;
                let left_arity = kinded_left.get_kind().arity;

                if left_arity == 0 {
                    Err(KindError::ArityIsZero(kinded_left))
                } else {
                    Ok(Type::Application(
                        Rc::new(kinded_left),
                        Rc::new(kinded_right),
                        Kind::new(left_arity - 1),
                    ))
                }
            }
            Type::Tuple(_) => todo!(),
            Type::Record(_) => todo!(),
        }
    }

    pub fn arity_is_zero(kinded: Type<Kind>) -> Result<Type<Kind>, KindError> {
        if kinded.get_kind().arity == 0 {
            Ok(kinded)
        } else {
            Err(KindError::ArityNotZero(kinded))
        }
    }
}
