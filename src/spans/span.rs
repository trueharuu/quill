use std::{
    fmt::{Debug, Display},
    ops::Deref,
};

use chumsky::{input::MapExtra, span::SimpleSpan};

pub type Span = SimpleSpan<usize>;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Deref for Spanned<T> {
    fn deref(&self) -> &Self::Target {
        &self.0
    }

    type Target = T;
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if f.alternate() {
            write!(f, "{:#?} @ {:?}", self.0, self.1)
        } else {
            write!(f, "{:?}", self.0)
        }
    }
}

impl<T> Spanned<T> {
    pub const fn value(&self) -> &T {
        &self.0
    }

    pub const fn span(&self) -> Span {
        self.1
    }
}

pub trait ApplySpan {
    fn apply_span(self, span: Span) -> Spanned<Self>
    where
        Self: Sized,
    {
        Spanned(self, span)
    }
}

impl<T> ApplySpan for T {}

pub fn span<'a, 'b, T, I, E>(t: T, s: &MapExtra<'a, 'b, I, E>) -> Spanned<T>
where
    I: chumsky::input::Input<'a, Span = chumsky::span::SimpleSpan>,
    E: chumsky::extra::ParserExtra<'a, I>,
{
    Spanned(t, s.span())
}

