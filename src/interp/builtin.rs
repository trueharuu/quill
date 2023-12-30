use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{
    common::vox::Vox,
    interp::{
        error::InterpreterError,
        ty::Ty,
        value::{RealFn, Values},
    },
    rc,
};

use super::value::Value;

#[must_use]
pub fn print() -> rc!(ty Value) {
    rc!(Value::Fn(
        "print".into(),
        ["input".into()].into(),
        None,
        Rc::new(|_, _, env| {
            println!("{}", env.borrow().get("input").unwrap().borrow());
            Ok(rc!(Value::Tuple([].vox())))
        }),
    ))
}

#[must_use]
pub fn assert() -> rc!(ty Value) {
    Rc::new(RefCell::new(Value::Fn(
        "assert".into(),
        ["predicate".into()].vox(),
        None,
        Rc::new(|span, _, env| {
            let binding = env.borrow().get("predicate").unwrap();
            let predicate = binding.borrow();
            if let Value::Bool(t) = predicate.clone() {
                if t {
                    Ok(rc!(Value::None))
                } else {
                    Err(InterpreterError::Custom("assertion failed".to_string(), span).vox())
                }
            } else {
                Err(InterpreterError::ExpectedFound(Ty::Bool, predicate.ty(), span).vox())
            }
        }),
    )))
}

pub mod obj {

    use crate::common::etc::Painted;

    use super::{rc, HashMap, InterpreterError, Rc, RealFn, Ty, Value, Values, Vox};
    use crate::fn_of;

    #[must_use]
    pub fn global(u: &Value) -> Values {
        let mut t = HashMap::default();

        t.insert("self".to_string(), rc!(u.clone()));
        t.insert(
            "ty".to_string(),
            rc!(Value::String(u.ty().to_string().into_boxed_str())),
        );

        t
    }

    #[must_use]
    pub fn bool(_b: &bool) -> Values {
        Values::default()
    }

    pub fn function(f: &str, a: &[Box<str>], s: &Option<rc!(ty Value)>, _m: &RealFn) -> Values {
        let mut t = HashMap::default();

        t.insert(
            "name".to_string(),
            rc!(Value::String(f.to_string().into_boxed_str())),
        );
        t.insert("arity".to_string(), rc!(Value::Real(a.len() as f64)));
        if let Some(s) = s {
            t.insert("parent".to_string(), s.clone());
        }

        t
    }

    #[must_use]
    pub fn real(_r: &f64) -> Values {
        Values::default()
    }

    #[must_use]
    pub fn set(s: &[rc!(ty Value)]) -> Values {
        let mut t = HashMap::new();

        t.insert("size".to_string(), rc!(Value::Real(s.len() as f64)));

        t
    }

    #[must_use]
    pub fn tuple(s: &[rc!(ty Value)]) -> Values {
        let mut t = HashMap::new();

        t.insert(
            "nth".to_string(),
            rc!(Value::Fn(
                "nth".into(),
                ["n".into()].into(),
                Some(rc!(Value::Tuple(s.into()))),
                Rc::new(move |span, this, env| {
                    let binding = env.borrow().get("n").unwrap();
                    let predicate = binding.borrow();
                    if let Value::Real(t) = predicate.clone() {
                        if t >= 0.0 && (t.floor() - t).abs() < f64::EPSILON {
                            #[allow(clippy::cast_sign_loss, clippy::cast_possible_truncation)]
                            let u = t as usize;

                            let binding = this.unwrap();
                            let binding_2 = binding.borrow();
                            let m = binding_2.as_tuple().unwrap();
                            m.get(u).map_or_else(
                                || {
                                    Err(Box::new(InterpreterError::Custom(
                                        format!(
                                            "index {} out of range for tuple of length {}",
                                            u.painted().fg(yansi::Color::Blue),
                                            m.len().painted().fg(yansi::Color::Blue)
                                        ),
                                        span,
                                    )))
                                },
                                |value| Ok(value.clone()),
                            )
                        } else {
                            Err(Box::new(InterpreterError::Custom(
                                format!(
                                    "expected an integer above 0, got {}",
                                    t.painted().fg(yansi::Color::Blue)
                                ),
                                span,
                            )))
                        }
                    } else {
                        Err(InterpreterError::ExpectedFound(Ty::Real, predicate.ty(), span).vox())
                    }
                })
            )),
        );
        t
    }

    #[must_use]
    pub fn string(s: &str) -> Values {
        let mut t = HashMap::new();

        t.insert(
            "to_lower".to_string(),
            fn_of!(
                to_lower,
                [],
                [Value::String(s.to_string().into_boxed_str())],
                move |_, this, _| {
                    Ok(rc!(Value::String(
                        this.unwrap()
                            .borrow()
                            .as_string()
                            .unwrap()
                            .to_lowercase()
                            .into_boxed_str()
                    )))
                }
            ),
        );
        t.insert(
            "to_upper".to_string(),
            fn_of!(
                to_upper,
                [],
                [Value::String(s.to_string().into_boxed_str())],
                move |_, this, _| {
                    Ok(rc!(Value::String(
                        this.unwrap()
                            .borrow()
                            .as_string()
                            .unwrap()
                            .to_uppercase()
                            .into_boxed_str()
                    )))
                }
            ),
        );
        
        t
    }
}

#[macro_export]
macro_rules! fn_of {
     ($name:ident, [$($arg:ident$(,)?)*], [$($s:tt)*], $($t:tt)*) => {
         rc!(Value::Fn(stringify!($name).into(), [$(stringify!($arg).into(),)*].into(), Some(rc!($($s)*)), Rc::new($($t)*)))
     };
 }
