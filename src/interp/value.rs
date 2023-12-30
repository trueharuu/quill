use std::{
    cell::RefCell,
    cmp::Ordering,
    collections::HashMap,
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{common::convert::Convert, list, rc, spans::span::Span};

use super::{builtin, env::Env, error::InterpreterError, ty::Ty};

pub type RealFn = Rc<
    dyn Fn(
        Span,
        Option<rc!(ty Value)>,
        rc!(ty Env),
    ) -> Result<rc!(ty Value), Box<InterpreterError>>,
>;
pub type Values = HashMap<String, rc!(ty Value)>;

#[derive(Clone)]
pub enum Value {
    Real(f64),
    Bool(bool),
    Set(list!(rc!(ty Self))),
    Tuple(list!(rc!(ty Self))),
    Fn(Box<str>, list!(Box<str>), Option<rc!(ty Self)>, RealFn),
    Object(rc!(ty Values)),
    String(Box<str>),
    None,
}

impl From<()> for Value {
    fn from((): ()) -> Self {
        Self::None
    }
}

impl Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool(v) => format!("{v}"),

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
                Self::Fn(label, args, _, _) => format!("fn {label}({})", args.len()),
                Self::Object(values) => format!(
                    "{{ {} }}",
                    values
                        .borrow()
                        .iter()
                        .map(|(label, value)| { format!("{label}: {}", value.borrow()) })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::String(t) => t.to_string(),

                Self::None => String::of("!"),
            }
        )
    }
}

impl Debug for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool(v) => format!("{v}"),

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
                Self::Fn(label, args, _, _) => format!("fn {label}({})", args.len()),
                Self::Object(values) => format!(
                    "{{ {} }}",
                    values
                        .borrow()
                        .iter()
                        .map(|(label, value)| { format!("{label}: {}", value.borrow()) })
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::String(t) => format!("{t:?}"),

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
            Self::None => Ty::None,
            Self::Tuple(t) => Ty::Tuple(t.iter().map(|x| x.borrow().ty()).collect::<Vec<_>>()),
            Self::Real(..) => Ty::Real,
            Self::Set(t) => Ty::Set(Box::new(
                t.first().map_or(Ty::Unresolved, |x| x.borrow().ty()),
            )),
            Self::Fn(label, _, _, _) => Ty::Fn(label.clone()),
            Self::Object(..) => Ty::Object,
            Self::String(..) => Ty::String,
        }
    }

    #[must_use]
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        self.ty().ty_eq(&rhs.ty())
    }

    #[must_use]
    pub const fn is_numeric(&self) -> bool {
        self.as_real().is_some()
    }

    #[must_use]
    pub fn handle_add(&self, rhs: &Self) -> Option<Self> {
        if let (Self::String(a), Self::String(b)) = (self, rhs) {
            return Some(Self::String((a.to_string() + b).into_boxed_str()));
        }
        Some(Self::Real(self.as_real()? + rhs.as_real()?))
    }

    #[must_use]
    pub fn handle_sub(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Real(self.as_real()? - rhs.as_real()?))
    }

    #[must_use]
    pub fn handle_mul(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Real(self.as_real()? * rhs.as_real()?))
    }

    #[must_use]
    pub fn handle_div(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Real(self.as_real()? * rhs.as_real()?))
    }

    #[must_use]
    pub fn handle_rem(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Real(self.as_real()? % rhs.as_real()?))
    }

    #[must_use]
    pub fn handle_cmp(&self, rhs: &Self) -> Option<Option<Ordering>> {
        if self.ty() == rhs.ty() {
            Some(self.part_cmp(rhs))
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
        self.as_real().is_some_and(|x| *x == 0.0)
    }

    #[must_use]
    pub fn handle_cross_product(&self, rhs: &Self) -> Option<Self> {
        if self.is_numeric() && rhs.is_numeric() {
            self.handle_mul(rhs)
        } else if let (Self::Set(t), Self::Set(u)) = (self, rhs) {
            let mut set = Vec::new();

            for i in t.iter() {
                for j in u.iter() {
                    set.push(Rc::new(RefCell::new(Self::Tuple(
                        vec![i.clone(), j.clone()].into_boxed_slice(),
                    ))));
                }
            }

            Some(Self::Set(set.into()))
        } else {
            None
        }
    }

    #[must_use]
    pub fn handle_dot_product(&self, rhs: &Self) -> Option<Self> {
        if self.is_numeric() && rhs.is_numeric() {
            self.handle_mul(rhs)
        } else {
            None
        }
    }

    #[must_use]
    pub fn handle_neg(&self) -> Option<Self> {
        Some(Self::Real(-*self.as_real()?))
    }

    #[must_use]
    pub fn handle_or(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(*self.as_bool()? || *rhs.as_bool()?))
    }

    #[must_use]
    pub fn handle_and(&self, rhs: &Self) -> Option<Self> {
        Some(Self::Bool(*self.as_bool()? && *rhs.as_bool()?))
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
    pub fn handle_for_all(&self) -> Option<Self> {
        for t in self.as_set()?.iter() {
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
        for t in self.as_set()?.iter() {
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
            *self.handle_subset_eq(rhs)?.as_bool()? && *self.handle_ne(rhs)?.as_bool()?,
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

    #[must_use]
    pub fn handle_union(&self, rhs: &Self) -> Option<Self> {
        let mut v = vec![];

        for i in self.as_set()?.iter() {
            v.push(i.clone());
        }

        for i in rhs.as_set()?.iter() {
            if !v.contains(i) {
                v.push(i.clone());
            }
        }

        Some(Self::Set(v.into_boxed_slice()))
    }

    #[must_use]
    pub fn handle_intersection(&self, rhs: &Self) -> Option<Self> {
        let mut v = vec![];
        let h = rhs.as_set()?;
        for i in self.as_set()?.iter() {
            if h.contains(i) {
                v.push(i.clone());
            }
        }

        Some(Self::Set(v.into_boxed_slice()))
    }

    #[must_use]
    pub fn transform_object(&self) -> rc!(ty Values) {
        let mut out = builtin::obj::global(self);

        out.extend(match self {
            Self::Bool(b) => builtin::obj::bool(b),
            Self::Fn(f, a, s, m) => builtin::obj::function(f, a, s, m),
            Self::None => Values::default(),
            Self::Object(b) => b.borrow().clone(),
            Self::Real(r) => builtin::obj::real(r),
            Self::Set(s) => builtin::obj::set(s),
            Self::Tuple(t) => builtin::obj::tuple(t),
            Self::String(t) => builtin::obj::string(t),
        });

        rc!(out)
    }

    #[must_use]
    pub fn part_cmp(&self, rhs: &Self) -> Option<Ordering> {
        match (self, rhs) {
            (Self::Bool(a), Self::Bool(b)) if a == b => Some(Ordering::Equal),
            #[allow(clippy::float_cmp)]
            (Self::Real(a), Self::Real(b)) if a == b => Some(Ordering::Equal),
            (Self::String(a), Self::String(b)) if a == b => Some(Ordering::Equal),
            (Self::Set(a), Self::Set(b)) if a == b => Some(Ordering::Equal),
            (Self::Tuple(a), Self::Tuple(b)) if a == b => Some(Ordering::Equal),
            (Self::None, Self::None) => Some(Ordering::Equal),

            _ => None,
        }
    }

    #[must_use]
    pub const fn as_real(&self) -> Option<&f64> {
        if let Self::Real(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_bool(&self) -> Option<&bool> {
        if let Self::Bool(b) = self {
            Some(b)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_set(&self) -> Option<&list!(rc!(ty Self))> {
        if let Self::Set(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_tuple(&self) -> Option<&list!(rc!(ty Self))> {
        if let Self::Tuple(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_object(&self) -> Option<&rc!(ty Values)> {
        if let Self::Object(v) = self {
            Some(v)
        } else {
            None
        }
    }

    #[must_use]
    pub const fn as_string(&self) -> Option<&str> {
        if let Self::String(v) = self {
            Some(v)
        } else {
            None
        }
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        self.part_cmp(other).is_some_and(Ordering::is_eq)
    }
}
