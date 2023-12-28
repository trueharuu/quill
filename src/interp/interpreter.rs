use std::{cell::RefCell, rc::Rc};



use crate::{
    assume,
    
    parse::expr::Expr,
    rc,
    spans::span::{ApplySpan, Span, Spanned},
};

use super::{builtin, env::Env, error::InterpreterError, ty::Ty, value::Value};
macro_rules! op {
    ($l:ident, $v:ident, $r:ident, $span:ident, $env:ident, $verb:literal, $call:ident) => {{
        let _l = $l.spanned_eval($env.clone())?;
        let _r = $r.spanned_eval($env.clone())?;
        let lhs = _l.borrow();
        let rhs = _r.borrow();

        lhs.$call(&rhs)
            .ok_or(InterpreterError::CannotBinary(
                $verb.to_string(),
                (*$l.clone(), lhs.clone()),
                $v.value().clone(),
                (*$r.clone(), rhs.clone()),
                $span,
            ))
            .map(|x| rc!(x))
    }};
    ($v:ident, $r:ident, $span:ident, $env:ident, $verb:literal, $call:ident) => {{
        let _r = $r.spanned_eval($env)?;
        let rhs = _r.borrow();

        rhs.$call()
            .ok_or(InterpreterError::CannotUnary(
                $verb.to_string(),
                $v.value().clone(),
                (*$r.clone(), rhs.clone()),
                $span,
            ))
            .map(|x| rc!(x))
    }};
}

impl Expr {
    pub fn eval(
        &self,
        span: Span,
        env: Rc<RefCell<Env>>,
    ) -> Result<Rc<RefCell<Value>>, InterpreterError> {
        // println!("{} {self} @ {span}", "[debug]".painted().fg(Color::Yellow),);
        match self {
            Self::Literal(v) => Ok(rc!(v.clone())),
            Self::View(_, e) => {
                let _ = e.eval(e.span(), env)?;
                // a built-in `print` value exists
                // println!("{}", value.borrow());
                Ok(rc!())
            }
            Self::Define(label, .., expr) => assume!(Self::Label(l) = label.value() => {
                let value = expr.eval(expr.span(), env.clone())?;
                let _ = env.borrow_mut().define((*l).to_string(), value);
                Ok(rc!())
            }),
            Self::Tuple(_, r, _) => {
                let mut tuple = Vec::new();

                for i in r {
                    let v = i.spanned_eval(env.clone())?;

                    tuple.push(v);
                }

                Ok(rc!(Value::Tuple(tuple)))
            }
            Self::DefinedSet(_, r, _) => {
                let mut ty = None;
                let mut set = Vec::new();

                for i in r {
                    let v = i.spanned_eval(env.clone())?;

                    if let Some(ref w) = ty {
                        if &v.borrow().ty() != w {
                            return Err(InterpreterError::SetElementIsNotOfType(
                                i.clone(),
                                v.borrow().clone(),
                                w.clone(),
                                span,
                            ));
                        }
                    } else {
                        let _ = ty.insert(v.borrow().ty());
                    }

                    set.push(v);
                }

                Ok(rc!(Value::Set(set)))
            }
            Self::Add(l, v, r) => {
                op!(l, v, r, span, env, "add", handle_add)
            }

            Self::Sub(l, v, r) => {
                op!(l, v, r, span, env, "subtract", handle_sub)
            }

            Self::Mul(l, v, r) => {
                op!(l, v, r, span, env, "multiply", handle_mul)
            }

            Self::Div(l, v, r) => {
                let ll = l.spanned_eval(env.clone())?;
                let lr = r.spanned_eval(env)?;
                let lhs = ll.borrow();
                let rhs = lr.borrow();

                if rhs.is_zero() {
                    return Err(InterpreterError::DivisionByZero(
                        *l.clone(),
                        *r.clone(),
                        span,
                    ));
                }

                lhs.handle_div(&rhs)
                    .ok_or(InterpreterError::CannotBinary(
                        "divide".to_string(),
                        (*l.clone(), lhs.clone()),
                        v.value().clone(),
                        (*r.clone(), rhs.clone()),
                        span,
                    ))
                    .map(|x| rc!(x))
            }

            Self::Eq(l, v, r) => {
                op!(l, v, r, span, env, "compare", handle_eq)
            }

            Self::Ne(l, v, r) => {
                op!(l, v, r, span, env, "compare", handle_ne)
            }

            Self::Gt(l, v, r) => op!(l, v, r, span, env, "compare", handle_gt),
            Self::Ge(l, v, r) => op!(l, v, r, span, env, "compare", handle_ge),
            Self::Lt(l, v, r) => op!(l, v, r, span, env, "compare", handle_lt),
            Self::Le(l, v, r) => op!(l, v, r, span, env, "compare", handle_le),

            Self::Label(l) => {
                let existing = env.borrow().get(l.clone());

                existing.map_or_else(
                    || {
                        env.borrow().last_use.get(l).map_or_else(
                            || {
                                Err(InterpreterError::VariableDoesNotExist(
                                    (*l).to_string(),
                                    span,
                                ))
                            },
                            |v| {
                                Err(InterpreterError::VariableNoLongerExists(
                                    l.clone(),
                                    v.clone(),
                                    span,
                                ))
                            },
                        )
                    },
                    Ok,
                )
            }

            Self::Drop(t, e) => {
                if let Self::Label(l) = e.value() {
                    let existing = env.borrow_mut().remove(l.clone());
                    env.borrow_mut()
                        .last_use
                        .insert(l.clone(), self.clone().apply_span(span));

                    existing.map_or_else(
                        || Err(InterpreterError::VariableDoesNotExist(l.clone(), e.span())),
                        Ok,
                    )
                } else {
                    Err(InterpreterError::UselessDrop(t.clone(), *e.clone(), span))
                }
            }

            Self::Group(_, v, _) => v.spanned_eval(env),

            Self::CrossProduct(l, v, r) => {
                op!(l, v, r, span, env, "cross-product", handle_cross_product)
            }

            Self::Neg(v, r) => op!(v, r, span, env, "negate", handle_neg),
            Self::Not(v, r) => op!(v, r, span, env, "negate", handle_not),

            Self::Or(l, v, r) => op!(l, v, r, span, env, "logical-disjoin", handle_or),
            Self::Nor(l, v, r) => op!(l, v, r, span, env, "logical-non-disjoin", handle_nor),
            Self::And(l, v, r) => op!(l, v, r, span, env, "logical-conjoin", handle_and),
            Self::Nand(l, v, r) => op!(l, v, r, span, env, "logical-non-conjoin", handle_nand),
            Self::Xor(l, v, r) => op!(l, v, r, span, env, "logical-exlusive-disjoin", handle_xor),
            Self::ForAll(v, r) => op!(v, r, span, env, "test", handle_for_all),

            Self::Exists(v, r) => {
                let rr = r.spanned_eval(env)?;
                let rhs = rr.borrow();

                rhs.handle_exists().map_or_else(
                    || {
                        Err(InterpreterError::CannotUnary(
                            "test".to_string(),
                            v.0.clone(),
                            (*r.clone(), rhs.clone()),
                            span,
                        ))
                    },
                    |s| {
                        s.map(|x| rc!(x)).map_err(|x| {
                            InterpreterError::SetElementIsNotOfType(*r.clone(), x, Ty::Bool, span)
                        })
                    },
                )
            }

            Self::NotExists(v, r) => {
                let rr = r.spanned_eval(env)?;
                let rhs = rr.borrow();

                rhs.handle_not_exists().map_or_else(
                    || {
                        Err(InterpreterError::CannotUnary(
                            "test".to_string(),
                            v.0.clone(),
                            (*r.clone(), rhs.clone()),
                            span,
                        ))
                    },
                    |s| {
                        s.map(|x| rc!(x)).map_err(|x| {
                            InterpreterError::SetElementIsNotOfType(*r.clone(), x, Ty::Bool, span)
                        })
                    },
                )
            }

            Self::Map(set, _, label, _, expr, _) => {
                let ll = set.spanned_eval(env.clone())?;
                
                let lhs = ll.borrow();

                if let Value::Set(t) = lhs.clone() {
                    let new_env = rc!(Env::with_parent(Some(env)));
                    let mut new_set = Vec::new();
                    for u in t {
                        let _ = new_env.clone().borrow_mut().define(label.to_string(), u);
                        let out = expr.spanned_eval(new_env.clone())?;
                        new_set.push(out);
                    }
                    Ok(rc!(Value::Set(new_set)))
                } else {
                    Err(InterpreterError::MapOfWrongType(
                        lhs.ty(),
                        *set.clone(),
                        span,
                    ))
                }
            }

            Self::In(l, v, r) => op!(l, v, r, span, env, "compare", handle_in),
            Self::NotIn(l, v, r) => op!(l, v, r, span, env, "compare", handle_not_in),
            Self::Contains(l, v, r) => op!(l, v, r, span, env, "compare", handle_contains),
            Self::NotContains(l, v, r) => op!(l, v, r, span, env, "compare", handle_not_contains),

            Self::Subset(l, v, r) => op!(l, v, r, span, env, "compare", handle_subset),
            Self::SubsetEq(l, v, r) => op!(l, v, r, span, env, "compare", handle_subset_eq),
            Self::SubsetNe(l, v, r) => op!(l, v, r, span, env, "compare", handle_subset_ne),
            Self::Superset(l, v, r) => op!(l, v, r, span, env, "compare", handle_superset),
            Self::SupersetEq(l, v, r) => op!(l, v, r, span, env, "compare", handle_superset_eq),
            Self::SupersetNe(l, v, r) => op!(l, v, r, span, env, "compare", handle_superset_ne),

            Self::Call(expr, l, args, r) => {
                let f = expr.spanned_eval(env.clone())?;

                let lhs = f.borrow();

                if let Value::Fn(a, b, c) = lhs.clone() {
                    let fn_env = rc!(Env::with_parent(Some(env.clone())));

                    for (id, label) in b.iter().enumerate() {
                        if let Some(a) = args.get(id) {
                            let value = a.spanned_eval(env.clone())?;
                            let _ = fn_env.borrow_mut().define(label, value);
                        } else {
                            return Err(InterpreterError::MissingArgument(
                                a,
                                label.clone(),
                                b.len(),
                                args.len(),
                                *expr.clone(),
                                chumsky::span::Span::union(&l.span(), r.span()),
                                span,
                            ));
                        }
                    }

                    c(span, fn_env)
                } else {
                    Err(InterpreterError::CallOnWrongType(
                        lhs.ty(),
                        *expr.clone(),
                        span,
                    ))
                }
            }

            c => Err(InterpreterError::NotImplemented(
                c.clone().apply_span(span),
                span,
            )),
        }
    }
}

impl Spanned<Expr> {
    pub fn spanned_eval(&self, env: rc!(ty Env)) -> Result<Rc<RefCell<Value>>, InterpreterError> {
        self.eval(self.1, env)
    }
}

pub fn many(exprs: &[Spanned<Expr>]) -> Result<Env, InterpreterError> {
    let env = rc!(Env::new());

    let _ = env.borrow_mut().define("print", builtin::print());
    let _ = env.borrow_mut().define("assert", builtin::assert());

    for e in exprs {
        e.eval(e.1, env.clone())?;
    }

    let x = Ok(env.borrow().clone());
    x
}
