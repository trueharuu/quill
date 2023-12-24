use std::fmt::{Debug, Display};

use crate::common::{convert::Convert, set::Set, vox::Vox};

use super::ty::Ty;

#[derive(Clone, PartialEq)]
pub enum Value {
    Natural(usize),
    Integer(isize),
    Real(f64),
    Bool(bool),
    Set(Set<Self>),
    Range((RangeType, RangeType), Box<Self>, Box<Self>),
    None,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RangeType {
    Open,
    Closed,
}

impl RangeType {
    #[must_use]
    pub const fn left(&self) -> char {
        match self {
            Self::Open => '(',
            Self::Closed => '[',
        }
    }

    #[must_use]
    pub const fn right(&self) -> char {
        match self {
            Self::Open => ')',
            Self::Closed => ']',
        }
    }
}

impl From<()> for Value {
    fn from(_: ()) -> Self {
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
                    v.0.iter()
                        .map(|x| format!("{x:?}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::Range((tl, tr), l, r) => format!("{}{l:?}, {r:?}{}", tl.left(), tr.right()),
                Self::None => String::of("!"),
            }
        )
    }
}

impl Value {
    pub fn ty(&self) -> Ty {
        match self {
            Self::Bool(..) => Ty::Bool,
            Self::Natural(..) => Ty::Natural,
            Self::Integer(..) => Ty::Integer,
            Self::None => Ty::None,
            Self::Range(.., l, r) => {
                if l.ty().is_unresolved() {
                    if r.ty().is_unresolved() {
                        Ty::Range(Ty::Unresolved.vox())
                    } else {
                        Ty::Range(r.ty().vox())
                    }
                } else {
                    Ty::Range(l.ty().vox())
                }
            }
            Self::Real(..) => Ty::Real,
            Self::Set(.., t) => Ty::Set(t.0.first().map_or(Ty::Unresolved, Self::ty).vox()),
        }
    }

    #[must_use]
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        self.ty().ty_eq(&rhs.ty())
    }

    #[must_use]
    pub fn fix_numeric(self) -> Self {
        match self {
            Self::Integer(v) => {
                if v >= 0 {
                    Self::Natural(v.unsigned_abs())
                } else {
                    self
                }
            }

            Self::Real(v) => {
                if (v.floor() - v).abs() < f64::EPSILON {
                    #[allow(clippy::cast_possible_truncation)]
                    Self::Integer(v as isize).fix_numeric()
                } else {
                    self
                }
            }
            _ => self,
        }
    }

    #[must_use]
    pub fn handle_add(&self, rhs: &Self) -> Option<Self> {
        Some(
            match (self, rhs) {
                (Self::Natural(l), Self::Natural(r)) => Self::Natural(l + r),
                // widens
                (Self::Natural(l), Self::Integer(r)) => {
                    Self::Integer((*l).try_to::<isize>().ok()? + r)
                }
                (Self::Natural(l), Self::Real(r)) => Self::Real(*l as f64 + r),
                (Self::Integer(l), Self::Integer(r)) => Self::Integer(l + r),
                (Self::Real(l), Self::Real(r)) => Self::Real(l + r),
                (Self::Integer(l), Self::Real(r)) => Self::Real(*l as f64 + r),
                (Self::Real(..) | Self::Integer(..), Self::Natural(..))
                | (Self::Real(..), Self::Integer(..)) => rhs.handle_add(self)?,

                _ => return None,
            }
            .fix_numeric(),
        )
    }

    #[must_use]
    pub fn handle_sub(&self, rhs: &Self) -> Option<Self> {
        Some(
            match (self, rhs) {
                (Self::Natural(l), Self::Natural(r)) => {
                    Self::Integer((*l).try_to::<isize>().ok()? - (*r).try_to::<isize>().ok()?)
                }
                // widens
                (Self::Natural(l), Self::Integer(r)) => {
                    Self::Integer((*l).try_to::<isize>().ok()? - r)
                }
                (Self::Natural(l), Self::Real(r)) => Self::Real(*l as f64 - r),
                (Self::Integer(l), Self::Integer(r)) => Self::Integer(l - r),
                (Self::Real(l), Self::Real(r)) => Self::Real(l - r),
                (Self::Integer(l), Self::Real(r)) => Self::Real(*l as f64 - r),
                (Self::Real(..) | Self::Integer(..), Self::Natural(..))
                | (Self::Real(..), Self::Integer(..)) => rhs.handle_sub(self)?,

                _ => return None,
            }
            .fix_numeric(),
        )
    }
}
