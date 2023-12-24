use crate::{
    assume,
    common::set::Set,
    parse::expr::Expr,
    spans::span::{Span, Spanned},
};

use super::{env::Env, error::InterpreterError, value::Value};

impl<'t> Expr<'t> {
    pub fn eval(&self, span: Span, env: &mut Env) -> Result<Value, InterpreterError> {
        match self {
            Expr::Literal(v) => Ok(v.clone()),
            Expr::View(_, e) => {
                let value = e.eval(e.span(), env)?;

                println!("{value}");
                Ok(().into())
            }
            Expr::Define(label, .., expr) => assume!(Expr::Label(l) = label.value() => {
                let value = expr.eval(expr.span(), env)?;
                env.values.insert((*l).to_string(), value);
                Ok(().into())
            }),
            Expr::DefinedSet(_, r, _) => {
                let mut set = Set::empty();

                for i in r {
                    let v = i.spanned_eval(env)?;

                    set.insert(v);
                }

                Ok(Value::Set(set))
            }
            Expr::Add(l, .., r) => {
                let lhs = l.spanned_eval(env)?;
                let rhs = r.spanned_eval(env)?;

                lhs.handle_add(&rhs).ok_or(InterpreterError::CannotAdd(
                    (l.value().clone(), lhs.ty()),
                    (r.value().clone(), rhs.ty()),
                    span,
                ))
            }

            Expr::Sub(l, .., r) => {
                let lhs = l.spanned_eval(env)?;
                let rhs = r.spanned_eval(env)?;

                lhs.handle_sub(&rhs).ok_or(InterpreterError::CannotSub(
                    (l.value().clone(), lhs.ty()),
                    (r.value().clone(), rhs.ty()),
                    span,
                ))
            }

            c => Err(InterpreterError::NotImplemented(c.clone(), span)),
        }
    }
}

impl<'t> Spanned<Expr<'t>> {
    pub fn spanned_eval(&self, env: &mut Env) -> Result<Value, InterpreterError> {
        self.eval(self.1, env)
    }
}

pub fn many<'t>(exprs: &'t [Spanned<Expr<'t>>]) -> Result<Env, InterpreterError<'t>> {
    let mut env = Env::new();

    for e in exprs {
        e.eval(e.1, &mut env)?;
    }

    Ok(env)
}
