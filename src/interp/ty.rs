use std::fmt::Display;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ty {
    Natural,
    Integer,
    Real,
    Bool,
    Set(Box<Ty>),
    Tuple(Vec<Ty>),
    Fn(Box<str>),
    String,
    Object,
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
                Self::None => "None".to_string(),
                Self::Tuple(t) => format!(
                    "Tuple({})",
                    t.iter()
                        .map(|x| format!("{x}"))
                        .collect::<Vec<_>>()
                        .join(", ")
                ),
                Self::Set(t) => format!("Set({t})"),
                Self::Unresolved => "<unresolved>".to_string(),
                Self::Fn(t) => format!("Fn({t})"),
                Self::Object => "Object".to_string(),
                Self::String => "String".to_string(),
            }
        )
    }
}
