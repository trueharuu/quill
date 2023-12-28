use std::{cell::RefCell, rc::Rc};

use crate::{
    interp::{error::InterpreterError, ty::Ty},
    rc,
};

use super::value::Value;

#[must_use]
pub fn print() -> rc!(ty Value) {
    rc!(Value::Fn(
        "print".to_string(),
        vec!["input".to_string()],
        |_, env| {
            println!("{}", env.borrow().get("input").unwrap().borrow());
            Ok(rc!(Value::None))
        },
    ))
}

#[must_use]
pub fn assert() -> rc!(ty Value) {
    Rc::new(RefCell::new(Value::Fn(
        "assert".to_string(),
        vec!["predicate".to_string()],
        |span, env| {
            let binding = env.borrow().get("predicate").unwrap();
            let predicate = binding.borrow();
            if let Value::Bool(t) = predicate.clone() {
                if t {
                    Ok(rc!(Value::None))
                } else {
                    Err(InterpreterError::Custom(
                        "assertion failed".to_string(),
                        span,
                    ))
                }
            } else {
                Err(InterpreterError::ExpectedFound(
                    Ty::Bool,
                    predicate.ty(),
                    span,
                ))
            }
        },
    )))
}
