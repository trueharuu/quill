use std::fmt::Display;

use crate::{common::named::Named, interp::value::Value, lex::token::Token, spans::span::Spanned};

#[derive(Clone, Debug, PartialEq)]
pub enum Expr<'t> {
    Add(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a + b
    Sub(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a - b
    Mul(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a * b
    CrossProduct(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a × b
    DotProduct(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a · b
    Modulus(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a % b
    Div(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a / b
    Eq(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a = b
    Ne(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a != b
    Gt(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a > b
    Ge(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a ≥ b
    Lt(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a < b
    Le(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a ≤ b
    Identical(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ≡ b
    Define(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a := b
    Into(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a → b
    Group(Spanned<Token<'t>>, Box<Spanned<Self>>, Spanned<Token<'t>>), // (a)
    IntervalOO(
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
    ), // (a, b)
    IntervalOC(
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
    ), // (a, b]
    IntervalCO(
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
    ), // [a, b)
    IntervalCC(
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
    ), // [a, b]
    Union(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ∪ b
    Intersection(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ∩ b
    In(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a ∈ b
    NotIn(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ∉ b
    ForAll(Spanned<Token<'t>>, Box<Spanned<Self>>),                  // ∀a
    Exists(Spanned<Token<'t>>, Box<Spanned<Self>>),                  // ∃a
    NotExists(Spanned<Token<'t>>, Box<Spanned<Self>>),               // ∄a
    Subset(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊂ b
    SubsetEq(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊆ b
    SubsetNe(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊄ b
    Superset(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊃ b
    SupersetEq(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊇ b
    SupersetNe(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊅ b
    And(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ∧ b
    Or(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>),  // a ∨ b
    Xor(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊻ b
    Nand(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊼ b
    Nor(Box<Spanned<Self>>, Spanned<Token<'t>>, Box<Spanned<Self>>), // a ⊽ b
    Not(Spanned<Token<'t>>, Box<Spanned<Self>>),                     // ¬a
    Neg(Spanned<Token<'t>>, Box<Spanned<Self>>),                     // -a
    DefinedSet(Spanned<Token<'t>>, Vec<Spanned<Self>>, Spanned<Token<'t>>), // {a, b}
    PredicateSet(
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
        Box<Spanned<Self>>,
        Spanned<Token<'t>>,
    ), // {a : b}
    Literal(Value),
    Label(&'t str),
    View(Spanned<Token<'t>>, Box<Spanned<Self>>),
}

impl<'a> Expr<'a> {
    fn parens(&self) -> String {
        match self {
            Self::Add(a, o, b)
            | Self::And(a, o, b)
            | Self::CrossProduct(a, o, b)
            | Self::Div(a, o, b)
            | Self::DotProduct(a, o, b)
            | Self::Eq(a, o, b)
            | Self::Ge(a, o, b)
            | Self::Gt(a, o, b)
            | Self::Identical(a, o, b)
            | Self::In(a, o, b)
            | Self::Intersection(a, o, b)
            | Self::Into(a, o, b)
            | Self::Le(a, o, b)
            | Self::Lt(a, o, b)
            | Self::Modulus(a, o, b)
            | Self::Mul(a, o, b)
            | Self::Nand(a, o, b)
            | Self::Ne(a, o, b)
            | Self::Nor(a, o, b)
            | Self::NotIn(a, o, b)
            | Self::SubsetNe(a, o, b)
            | Self::SupersetNe(a, o, b)
            | Self::Or(a, o, b)
            | Self::Sub(a, o, b)
            | Self::Subset(a, o, b)
            | Self::SubsetEq(a, o, b)
            | Self::Superset(a, o, b)
            | Self::SupersetEq(a, o, b)
            | Self::Union(a, o, b)
            | Self::Xor(a, o, b) => format!("({a}) {o} ({b})"),
            Self::Exists(a, b)
            | Self::ForAll(a, b)
            | Self::Neg(a, b)
            | Self::Not(a, b)
            | Self::NotExists(a, b) => format!("{a}({b})"),
            Self::Group(a, o, b) => format!("{a}{o}{b}"),
            Self::DefinedSet(l, e, r) => format!(
                "{l} {} {r}",
                e.iter()
                    .map(|x| format!("{x}"))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Self::Literal(v) => format!("{v:?}"),
            Self::Define(a, o, c) => format!("{a} {o} {c}"),
            Self::View(a, o) => format!("{a} {o}"),
            Self::IntervalOO(left, a, comma, b, right)
            | Self::IntervalOC(left, a, comma, b, right)
            | Self::IntervalCO(left, a, comma, b, right)
            | Self::IntervalCC(left, a, comma, b, right) => format!("{left}{a}{comma} {b}{right}"),
            Self::PredicateSet(l, set, pipe, filter, r) => format!("{l}{set} {pipe} {filter}{r}"),
            Self::Label(v) => format!("{v}"),
        }
    }
}
impl<'a> Display for Expr<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.parens())
    }
}

impl<'a> Named for Expr<'a> {
    fn name(&self) -> String {
        match self {
            Self::Add(..) => String::from("Add"),
            Self::Sub(..) => String::from("Sub"),
            Self::Mul(..) => String::from("Mul"),
            Self::CrossProduct(..) => String::from("CrossProduct"),
            Self::DotProduct(..) => String::from("DotProduct"),
            Self::Modulus(..) => String::from("Modulus"),
            Self::Div(..) => String::from("Div"),
            Self::Eq(..) => String::from("Eq"),
            Self::Ne(..) => String::from("Ne"),
            Self::Gt(..) => String::from("Gt"),
            Self::Ge(..) => String::from("Ge"),
            Self::Lt(..) => String::from("Lt"),
            Self::Le(..) => String::from("Le"),
            Self::Identical(..) => String::from("Identical"),
            Self::Define(..) => String::from("Define"),
            Self::Into(..) => String::from("Into"),
            Self::Group(..) => String::from("Group"),
            Self::IntervalOO(..) => String::from("IntervalOO"),
            Self::IntervalOC(..) => String::from("IntervalOC"),
            Self::IntervalCO(..) => String::from("IntervalCO"),
            Self::IntervalCC(..) => String::from("IntervalCC"),
            Self::Union(..) => String::from("Union"),
            Self::Intersection(..) => String::from("Intersection"),
            Self::In(..) => String::from("In"),
            Self::NotIn(..) => String::from("NotIn"),
            Self::ForAll(..) => String::from("ForAll"),
            Self::Exists(..) => String::from("Exists"),
            Self::NotExists(..) => String::from("NotExists"),
            Self::Subset(..) => String::from("Subset"),
            Self::SubsetEq(..) => String::from("SubsetEq"),
            Self::SubsetNe(..) => String::from("SubsetNe"),
            Self::Superset(..) => String::from("Superset"),
            Self::SupersetEq(..) => String::from("SupersetEq"),
            Self::SupersetNe(..) => String::from("SupersetNe"),
            Self::And(..) => String::from("And"),
            Self::Or(..) => String::from("Or"),
            Self::Xor(..) => String::from("Xor"),
            Self::Nand(..) => String::from("Nand"),
            Self::Nor(..) => String::from("Nor"),
            Self::Not(..) => String::from("Not"),
            Self::Neg(..) => String::from("Neg"),
            Self::DefinedSet(..) => String::from("DefinedSet"),
            Self::PredicateSet(..) => String::from("PredicateSet"),
            Self::Literal(..) => String::from("Literal"),
            Self::Label(..) => String::from("Label"),
            Self::View(..) => String::from("View"),
        }
    }
}
