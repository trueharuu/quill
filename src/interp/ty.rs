use std::fmt::Display;

#[derive(Clone, PartialEq, Eq)]
pub enum Ty {
    Natural,
    Integer,
    Real,
    Bool,
    Set(Box<Ty>),
    Range(Box<Ty>),
    None,
    Unresolved,
}
impl Ty {
    #[must_use]
    pub fn is_unresolved(&self) -> bool {
        self == &Self::Unresolved
    }

    #[must_use]
    pub fn ty_eq(&self, rhs: &Self) -> bool {
        (self.is_unresolved() || rhs.is_unresolved()) || (self == rhs)
    }
}

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool => "Bool".to_string(),
                Self::Integer => "Integer".to_string(),
                Self::Natural => "Natural".to_string(),
                Self::Real => "Real".to_string(),
                Self::None => "<none>".to_string(),
                Self::Range(l) => format!("Range({l})"),
                Self::Set(l) => format!("Set({l})"),
                Self::Unresolved => "<unresolved>".to_string(),
            }
        )
    }
}
