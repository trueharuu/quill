use std::{
    cell::RefCell,
    cmp::Ordering,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{common::convert::Convert, rc, spans::span::Span};

use super::{env::Env, error::InterpreterError, ty::Ty};

pub type RealFn = fn(Span, rc!(ty Env)) -> Result<rc!(ty Value), InterpreterError>;

#[derive(Clone, PartialEq)]
pub enum Value {
    Natural(usize),
    Integer(isize),
    Real(f64),
    Bool(bool),
    Set(Vec<Rc<RefCell<Self>>>),
    Tuple(Vec<Rc<RefCell<Self>>>),
    Fn(String, Vec<String>, RealFn),
    None,
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        Self::None
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool(v) => format!("{v}"),
                Self::Natural(v) => format!("{v}"),
                Self::Integer(v) => format!("{v}"),

                Self::Real(v) => format!("{v}"),
                Self::Set(v) => format!(
                    "{{ {} }}",
                    v.iter()
                        .map(|x| format!("{:?}", x.borrow()))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::Tuple(v) => format!(
                    "({})",
                    v.iter()
                        .map(|x| format!("{:?}", x.borrow()))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::Fn(label, args, _) => format!("fn {label}({})", args.len()),

                Self::None => String::of("!"),
            }
        )
    }
}

impl Value {
    #[must_use]
    pub fn ty(&self) -> Ty {
        match self {
            Self::Bool(..) => Ty::Bool,
            Self::Natural(..) => Ty::Natural,
            Self::Integer(..) => Ty::Integer,
            Self::None => Ty::None,
            Self::Tuple(t) => Ty::Tuple(t.iter().map(|x| x.borrow().ty()).collect::<Vec<_>>()),
            Self::Real(..) => Ty::Real,
            Self::Set(t) => Ty::Set(Box::new(
                t.first().map_or(Ty::Unresolved, |x| x.borrow().ty()),
            )),
            Self::Fn(label, _, _) => Ty::Fn(label.clone()),
        }
    }

    #[must_use]
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        self.ty().ty_eq(&rhs.ty())
    }

    #[must_use]
    pub const fn as_numeric(&self) -> Option<f64> {
        match self {
            #[allow(clippy::cast_precision_loss)]
            Self::Natural(l) => Some(*l as f64),
            #[allow(clippy::cast_precision_loss)]
            Self::Integer(l) => Some(*l as f64),
            Self::Real(l) => Some(*l),

            _ => None,
        }
    }

    #[must_use]
    pub const fn as_bool(&self) -> Option<bool> {
        match self {
            Self::Bool(l) => Some(*l),
            _ => None,
        }
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        self.as_numeric().is_some()
    }

    #[must_use]
    pub fn fix_numeric(self) -> Option<Self> {
        match self {
            Self::Integer(v) => {
                if v >= 0 {
                    Some(Self::Natural(v.unsigned_abs()))
                } else {
                    Some(self)
                }
            }

            Self::Real(v) => {
                if (v.floor() - v).abs() < f64::EPSILON {
                    #[allow(clippy::cast_possible_truncation)]
                    Self::Integer(v as isize).fix_numeric()
                } else {
                    // special handling for really bad floating-point values
                    if v.is_infinite() || v.is_nan() {
                        return None;
                    }
                    Some(self)
                }
            }
            _ => Some(self),
        }
    }

    #[must_use]
    pub fn handle_add(&self, rhs: &Self) -> Option<Self> {
        Self::Real(self.as_numeric()? + rhs.as_numeric()?).fix_numeric()
    }

    #[must_use]
    pub fn handle_sub(&self, rhs: &Self) -> Option<Self> {
        Self::Real(self.as_numeric()? - rhs.as_numeric()?).fix_numeric()
    }

    #[must_use]
    pub fn handle_mul(&self, rhs: &Self) -> Option<Self> {
        Self::Real(self.as_numeric()? * rhs.as_numeric()?).fix_numeric()
    }

    #[must_use]
    pub fn handle_div(&self, rhs: &Self) -> Option<Self> {
        Self::Real(self.as_numeric()? * rhs.as_numeric()?).fix_numeric()
    }

    #[must_use]
    pub fn handle_cmp(&self, rhs: &Self) -> Option<Option<Ordering>> {
        if let Some((l, r)) = self.as_numeric().zip(rhs.as_numeric()) {
            Some(l.partial_cmp(&r))
        } else if self == rhs {
            Some(Some(Ordering::Equal))
        } else if self.ty() == rhs.ty() {
            Some(None)
        } else {
            None
        }
    }

    #[must_use]
    pub fn handle_eq(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.handle_cmp(rhs)? == Some(Ordering::Equal)))
    }

    #[must_use]
    pub fn handle_ne(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool((self.handle_cmp(rhs)?).is_none()))
    }

    #[must_use]
    pub fn handle_gt(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.handle_cmp(rhs)? == Some(Ordering::Greater)))
    }

    #[must_use]
    pub fn handle_ge(&self, rhs: &Self) -> Option<Self> {
        self.handle_eq(rhs).or_else(|| self.handle_gt(rhs))
    }

    #[must_use]
    pub fn handle_lt(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.handle_cmp(rhs)? == Some(Ordering::Less)))
    }

    #[must_use]
    pub fn handle_le(&self, rhs: &Self) -> Option<Self> {
        self.handle_eq(rhs).or_else(|| self.handle_lt(rhs))
    }

    #[must_use]
    pub fn handle_not(&self) -> Option<Self> {
        if let Self::Bool(b) = self {
            Some(Self::Bool(!b))
        } else {
            None
        }
    }

    #[must_use]
    pub fn is_zero(&self) -> bool {
        self.as_numeric().is_some_and(|x| x == 0.0)
    }

    #[must_use]
    pub fn handle_cross_product(&self, rhs: &Self) -> Option<Self> {
        if self.is_numeric() && rhs.is_numeric() {
            self.handle_mul(rhs)
        } else if let (Self::Set(t), Self::Set(u)) = (self, rhs) {
            let mut set = Vec::new();

            for i in t {
                for j in u {
                    set.push(Rc::new(RefCell::new(Self::Tuple(vec![
                        i.clone(),
                        j.clone(),
                    ]))));
                }
            }

            Some(Self::Set(set))
        } else {
            None
        }
    }

    #[must_use]
    pub fn handle_neg(&self) -> Option<Self> {
        Self::Real(-self.as_numeric()?).fix_numeric()
    }

    #[must_use]
    pub fn handle_or(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.as_bool()? || rhs.as_bool()?))
    }

    #[must_use]
    pub fn handle_and(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.as_bool()? && rhs.as_bool()?))
    }

    #[must_use]
    pub fn handle_xor(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(self.as_bool()? ^ rhs.as_bool()?))
    }

    #[must_use]
    pub fn handle_nor(&self, rhs: &Self) -> Option<Self> {
        self.handle_or(rhs)?.handle_not()
    }

    #[must_use]
    pub fn handle_nand(&self, rhs: &Self) -> Option<Self> {
        self.handle_and(rhs)?.handle_not()
    }

    #[must_use]
    pub const fn as_set(&self) -> Option<&Vec<Rc<RefCell<Self>>>> {
        if let Self::Set(t) = self {
            Some(t)
        } else {
            None
        }
    }

    #[must_use]
    pub fn handle_for_all(&self) -> Option<Self> {
        for t in self.as_set()? {
            let ths = t.borrow();

            if let Self::Bool(t) = *ths {
                if !t {
                    return Some(Self::Bool(false));
                }
            } else {
                return None;
            }
        }

        Some(Self::Bool(true))
    }

    #[must_use]
    pub fn handle_exists(&self) -> Option<Result<Self, Self>> {
        for t in self.as_set()? {
            let ths = t.borrow();

            if let Self::Bool(t) = *ths {
                if t {
                    return Some(Ok(Self::Bool(true)));
                }
            } else {
                return Some(Err(ths.clone()));
            }
        }

        Some(Ok(Self::Bool(false)))
    }

    #[must_use]
    pub fn handle_not_exists(&self) -> Option<Result<Self, Self>> {
        self.handle_exists()?.map(|x| x.handle_not()).transpose()
    }

    #[must_use]
    pub fn handle_in(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(rhs.as_set()?.contains(&rc!(self.clone()))))
    }

    #[must_use]
    pub fn handle_not_in(&self, rhs: &Self) -> Option<Self> {
        self.handle_in(rhs)?.handle_not()
    }

    #[must_use]
    pub fn handle_contains(&self, rhs: &Self) -> Option<Self> {
        rhs.handle_in(self)
    }

    #[must_use]
    pub fn handle_not_contains(&self, rhs: &Self) -> Option<Self> {
        rhs.handle_not_in(self)
    }

    #[must_use]
    pub fn handle_subset_eq(&self, rhs: &Self) -> Option<Self> {
        let r = rhs.as_set()?;
        Some(Self::Bool(self.as_set()?.iter().all(|x| r.contains(x))))
    }

    #[must_use]
    pub fn handle_subset(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(
            self.handle_subset_eq(rhs)?.as_bool()? && self.handle_ne(rhs)?.as_bool()?,
        ))
    }

    #[must_use]
    pub fn handle_subset_ne(&self, rhs: &Self) -> Option<Self> {
        self.handle_subset_eq(rhs)?.handle_not()
    }

    #[must_use]
    pub fn handle_superset_eq(&self, rhs: &Self) -> Option<Self> {
        rhs.handle_subset_eq(self)
    }

    #[must_use]
    pub fn handle_superset(&self, rhs: &Self) -> Option<Self> {
        rhs.handle_subset(self)
    }

    #[must_use]
    pub fn handle_superset_ne(&self, rhs: &Self) -> Option<Self> {
        rhs.handle_subset_ne(self)
    }
}
