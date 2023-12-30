use std::{cell::RefCell, rc::Rc};

use crate::{
    assume,
    common::vox::Vox,
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
            .ok_or(
                InterpreterError::CannotBinary(
                    $verb.to_string(),
                    ($l.clone(), lhs.clone()),
                    $v.value().clone(),
                    ($r.clone(), rhs.clone()),
                    $span,
                )
                .vox(),
            )
            .map(|x| rc!(x))
    }};
    ($v:ident, $r:ident, $span:ident, $env:ident, $verb:literal, $call:ident) => {{
        let _r = $r.spanned_eval($env)?;
        let rhs = _r.borrow();

        rhs.$call()
            .ok_or(
                InterpreterError::CannotUnary(
                    $verb.to_string(),
                    $v.value().clone(),
                    ($r.clone(), rhs.clone()),
                    $span,
                )
                .vox(),
            )
            .map(|x| rc!(x))
    }};
}

pub type Output = Result<rc!(ty Value), Box<InterpreterError>>;

impl Expr {
    pub fn eval(&self, span: Span, env: rc!(ty Env)) -> Output {
        // println!("{} {self} @ {span}", "[debug]".painted().fg(Color::Yellow),);
        match self {
            Self::Literal(v) => Ok(rc!(v.clone())),
            Self::View(_, e) => self.view(e, span, &env),
            Self::Define(label, .., expr) => self.define(label, expr, span, &env),
            Self::Reassign(label, .., expr) => self.reassign(label, expr, span, &env),
            Self::Tuple(_, r, _) => self.tuple(r, span, &env),
            Self::DefinedSet(_, r, _) => self.defined_set(r, span, &env),
            Self::Identical(a, .., b) => {
                let la = a.spanned_eval(env.clone())?;
                let lb = b.spanned_eval(env)?;

                Ok(rc!(Value::Bool(la.as_ptr() == lb.as_ptr())))
            }

            Self::Block(_, e, _) => {
                let sub_env = rc!(Env::with_parent(Some(env)));
                many(e, &sub_env)?;
                Ok(rc!(Value::None))
            }

            Self::Add(..)
            | Self::Sub(..)
            | Self::Mul(..)
            | Self::Div(..)
            | Self::Eq(..)
            | Self::Ne(..)
            | Self::Gt(..)
            | Self::Ge(..)
            | Self::Lt(..)
            | Self::Le(..)
            | Self::Or(..)
            | Self::Nor(..)
            | Self::And(..)
            | Self::Nand(..)
            | Self::Xor(..)
            | Self::In(..)
            | Self::NotIn(..)
            | Self::Contains(..)
            | Self::NotContains(..)
            | Self::Subset(..)
            | Self::SubsetEq(..)
            | Self::SubsetNe(..)
            | Self::Superset(..)
            | Self::SupersetEq(..)
            | Self::SupersetNe(..)
            | Self::CrossProduct(..)
            | Self::Rem(..)
            | Self::Union(..)
            | Self::Intersection(..)
            | Self::DotProduct(..) => self.binary(span, env),

            Self::Neg(..)
            | Self::Not(..)
            | Self::ForAll(..)
            | Self::Exists(..)
            | Self::NotExists(..) => self.unary(span, env),

            Self::Label(l) => self.label(l, span, &env),

            Self::Drop(t, e) => {
                if let Self::Label(l) = e.value() {
                    let existing = env.borrow_mut().remove(l.clone());
                    env.borrow_mut()
                        .last_use
                        .insert(l.clone(), self.clone().apply_span(span));

                    existing.map_or_else(
                        || Err(InterpreterError::VariableDoesNotExist(l.clone(), e.span()).vox()),
                        Ok,
                    )
                } else {
                    Err(InterpreterError::UselessDrop(t.clone(), e.clone(), span).vox())
                }
            }

            Self::Group(_, v, _) => v.spanned_eval(env),

            Self::If(_, _, l_cond, _, l_block, _, l_e_block) => {
                let lc = l_cond.spanned_eval(env.clone())?;
                let cond = lc.borrow();

                if let Value::Bool(t) = cond.clone() {
                    if t {
                        l_block.spanned_eval(env)
                    } else if let Some(e_block) = l_e_block {
                        e_block.spanned_eval(env)
                    } else {
                        Ok(rc!(Value::None))
                    }
                } else {
                    Err(Box::new(InterpreterError::ExpectedFound(
                        Ty::Bool,
                        cond.ty(),
                        l_cond.span(),
                    )))
                }
            }

            Self::Map(set, _, label, _, expr, _) => {
                let ll = set.spanned_eval(env.clone())?;

                let lhs = ll.borrow();

                if let Value::Set(t) = lhs.clone() {
                    let new_env = rc!(Env::with_parent(Some(env)));
                    let mut new_set = Vec::new();
                    for u in t.iter() {
                        let _ = new_env
                            .clone()
                            .borrow_mut()
                            .define(label.to_string(), u.clone());
                        let out = expr.spanned_eval(new_env.clone())?;
                        new_set.push(out);
                    }
                    Ok(rc!(Value::Set(new_set.into())))
                } else {
                    Err(InterpreterError::MapOfWrongType(lhs.ty(), set.clone(), span).vox())
                }
            }

            Self::Call(expr, l, args, r) => {
                let f = expr.spanned_eval(env.clone())?;
                // dbg!(expr);

                let lhs = f.borrow();

                if let Value::Fn(a, b, d, c) = lhs.clone() {
                    let fn_env = rc!(Env::with_parent(Some(env.clone())));

                    for (id, label) in b.iter().enumerate() {
                        if let Some(a) = args.get(id) {
                            let value = a.spanned_eval(env.clone())?;
                            let _ = fn_env.borrow_mut().define(label, value);
                        } else {
                            return Err(InterpreterError::MissingArgument(
                                a,
                                label.clone(),
                                b,
                                args.len(),
                                expr.clone(),
                                chumsky::span::Span::union(&l.span(), r.span()),
                                span,
                            )
                            .vox());
                        }
                    }

                    c(span, d, fn_env)
                } else {
                    Err(InterpreterError::CallOnWrongType(lhs.ty(), expr.clone(), span).vox())
                }
            }
            Self::Access(expr, _, label) => {
                let ll = expr.spanned_eval(env)?;
                let lhs = ll.borrow();

                let rhs = assume!(Self::Label(l) = label.value().clone() => { l });

                lhs.transform_object().borrow().get(&rhs).cloned().ok_or(
                    InterpreterError::KeyDoesNotExist(
                        expr.clone(),
                        lhs.clone(),
                        rhs.clone().apply_span(label.span()),
                        span,
                    )
                    .vox(),
                )
            }

            Self::Fn(_, label, _, argv, _, e) => {
                let ll = assume!(Self::Label(l) = label.value() => { l });
                let vv = argv
                    .iter()
                    .map(|x| {
                        assume!(Self::Label(p) = x.value() => { p })
                            .clone()
                            .into_boxed_str()
                    })
                    .collect::<Vec<_>>();

                let m = e.value().clone();

                let _ = env.borrow_mut().define(
                    ll,
                    rc!(Value::Fn(
                        ll.clone().into_boxed_str(),
                        vv.into(),
                        None,
                        Rc::new(move |span, _, env| {
                            let u = m.eval(span, env);
                            if let Err(ii) = u {
                                if let InterpreterError::SafeReturn(vb, _) = *ii {
                                    Ok(vb)
                                } else {
                                    Err(ii)
                                }
                            } else {
                                u
                            }
                        })
                    )),
                );
                Ok(rc!(Value::None))
            }

            Self::Return(_, e) => {
                if env.borrow().parent.is_none() {
                    return Err(Box::new(InterpreterError::ReturnOutsideOfFunction(span)));
                }
                let ll = e.spanned_eval(env)?;
                Err(Box::new(InterpreterError::SafeReturn(ll, span)))
            }

            Self::Throw(_, e) => {
                let ll = e.spanned_eval(env)?;
                let l = ll.borrow();
                Err(Box::new(InterpreterError::Custom(l.to_string(), span)))
            } // c => {
              //     Err(InterpreterError::NotImplemented(c.clone().apply_span(span).vox(), span).vox())
              // }
        }
    }

    pub fn view(&self, e: &Spanned<Self>, _: Span, env: &rc!(ty Env)) -> Output {
        let _ = e.eval(e.span(), env.clone())?;
        Ok(rc!())
    }

    pub fn tuple(&self, r: &[Spanned<Self>], _: Span, env: &rc!(ty Env)) -> Output {
        let mut tuple = Vec::new();

        for i in r {
            let v = i.spanned_eval(env.clone())?;

            tuple.push(v);
        }

        Ok(rc!(Value::Tuple(tuple.into_boxed_slice())))
    }

    pub fn define(
        &self,
        label: &Spanned<Self>,
        expr: &Spanned<Self>,
        _: Span,
        env: &rc!(ty Env),
    ) -> Output {
        assume!(Self::Label(l) = label.value() => {
            let value = expr.eval(expr.span(), env.clone())?;
            let _ = env.borrow_mut().define((*l).to_string(), value);
            Ok(rc!())
        })
    }

    pub fn reassign(
        &self,
        label: &Spanned<Self>,
        expr: &Spanned<Self>,
        _: Span,
        env: &rc!(ty Env),
    ) -> Output {
        assume!(Self::Label(l) = label.value() => {
            let value = expr.eval(expr.span(), env.clone())?;
            env.borrow_mut().reassign(l, value).map_or_else(|| Err(Box::new(InterpreterError::VariableDoesNotExist(l.to_string(), label.span()))), |()| Ok(rc!(Value::None)))
        })
    }

    pub fn defined_set(&self, r: &Vec<Spanned<Self>>, span: Span, env: &rc!(ty Env)) -> Output {
        let mut ty = None;
        let mut set = Vec::new();

        for i in r {
            let v = i.spanned_eval(env.clone())?;

            if let Some(ref w) = ty {
                if &v.borrow().ty() != w {
                    return Err(InterpreterError::SetElementIsNotOfType(
                        i.clone().vox(),
                        v.borrow().clone(),
                        w.clone(),
                        span,
                    )
                    .vox());
                }
            } else {
                let _ = ty.insert(v.borrow().ty());
            }

            set.push(v);
        }

        Ok(rc!(Value::Set(set.into())))
    }

    pub fn binary(&self, span: Span, env: rc!(ty Env)) -> Output {
        match self {
            Self::Add(l, v, r) => op!(l, v, r, span, env, "add", handle_add),
            Self::Sub(l, v, r) => op!(l, v, r, span, env, "subtract", handle_sub),
            Self::Mul(l, v, r) => op!(l, v, r, span, env, "multiply", handle_mul),

            Self::Div(l, v, r) => {
                let ll = l.spanned_eval(env.clone())?;
                let lr = r.spanned_eval(env)?;
                let lhs = ll.borrow();
                let rhs = lr.borrow();

                if rhs.is_zero() {
                    return Err(InterpreterError::DivisionByZero(l.clone(), r.clone(), span).vox());
                }

                lhs.handle_div(&rhs)
                    .ok_or(
                        InterpreterError::CannotBinary(
                            "divide".to_string(),
                            (l.clone(), lhs.clone()),
                            v.value().clone(),
                            (r.clone(), rhs.clone()),
                            span,
                        )
                        .vox(),
                    )
                    .map(|x| rc!(x))
            }

            Self::Rem(l, v, r) => op!(l, v, r, span, env, "get remainder", handle_rem),

            Self::Eq(l, v, r) => op!(l, v, r, span, env, "compare", handle_eq),
            Self::Ne(l, v, r) => op!(l, v, r, span, env, "compare", handle_ne),

            Self::Gt(l, v, r) => op!(l, v, r, span, env, "compare", handle_gt),
            Self::Ge(l, v, r) => op!(l, v, r, span, env, "compare", handle_ge),
            Self::Lt(l, v, r) => op!(l, v, r, span, env, "compare", handle_lt),
            Self::Le(l, v, r) => op!(l, v, r, span, env, "compare", handle_le),

            Self::Or(l, v, r) => op!(l, v, r, span, env, "logical-disjoin", handle_or),
            Self::Nor(l, v, r) => op!(l, v, r, span, env, "logical-non-disjoin", handle_nor),
            Self::And(l, v, r) => op!(l, v, r, span, env, "logical-conjoin", handle_and),
            Self::Nand(l, v, r) => op!(l, v, r, span, env, "logical-non-conjoin", handle_nand),
            Self::Xor(l, v, r) => op!(l, v, r, span, env, "logical-exlusive-disjoin", handle_xor),

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

            Self::CrossProduct(l, v, r) => {
                op!(l, v, r, span, env, "cross-product", handle_cross_product)
            }

            Self::Union(l, v, r) => op!(l, v, r, span, env, "disjoin", handle_union),
            Self::Intersection(l, v, r) => op!(l, v, r, span, env, "conjoin", handle_intersection),
            Self::DotProduct(l, v, r) => op!(l, v, r, span, env, "dot-product", handle_dot_product),

            _ => unreachable!(),
        }
    }

    pub fn unary(&self, span: Span, env: rc!(ty Env)) -> Output {
        match self {
            Self::Neg(v, r) => op!(v, r, span, env, "negate", handle_neg),
            Self::Not(v, r) => op!(v, r, span, env, "negate", handle_not),

            Self::ForAll(v, r) => op!(v, r, span, env, "test", handle_for_all),

            Self::Exists(v, r) => {
                let rr = r.spanned_eval(env)?;
                let rhs = rr.borrow();

                rhs.handle_exists().map_or_else(
                    || {
                        Err(InterpreterError::CannotUnary(
                            "test".to_string(),
                            v.0.clone(),
                            (r.clone(), rhs.clone()),
                            span,
                        )
                        .vox())
                    },
                    |s| {
                        s.map(|x| rc!(x)).map_err(|x| {
                            InterpreterError::SetElementIsNotOfType(r.clone(), x, Ty::Bool, span)
                                .vox()
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
                            (r.clone(), rhs.clone()),
                            span,
                        )
                        .vox())
                    },
                    |s| {
                        s.map(|x| rc!(x)).map_err(|x| {
                            InterpreterError::SetElementIsNotOfType(r.clone(), x, Ty::Bool, span)
                                .vox()
                        })
                    },
                )
            }

            _ => unreachable!(),
        }
    }

    pub fn label(&self, l: &String, span: Span, env: &rc!(ty Env)) -> Output {
        let existing = env.borrow().get(l.clone());

        existing.map_or_else(
            || {
                env.borrow().last_use.get(l).map_or_else(
                    || Err(InterpreterError::VariableDoesNotExist((*l).to_string(), span).vox()),
                    |v| {
                        Err(InterpreterError::VariableNoLongerExists(
                            l.clone(),
                            v.clone().vox(),
                            span,
                        )
                        .vox())
                    },
                )
            },
            Ok,
        )
    }
}

impl Spanned<Expr> {
    pub fn spanned_eval(
        &self,
        env: rc!(ty Env),
    ) -> Result<Rc<RefCell<Value>>, Box<InterpreterError>> {
        self.eval(self.1, env)
    }
}

pub fn many(exprs: &[Spanned<Expr>], env: &rc!(ty Env)) -> Result<Env, Box<InterpreterError>> {
    let _ = env.borrow_mut().define("print", builtin::print());
    let _ = env.borrow_mut().define("assert", builtin::assert());

    for e in exprs {
        e.eval(e.1, env.clone())?;
    }

    let x = Ok(env.borrow().clone());
    x
}
