use ariadne::{Color, Fmt};

use crate::{common::named::Named, parse::expr::Expr, spans::span::Span};

use super::ty::Ty;

pub enum InterpreterError<'t> {
    NotImplemented(Expr<'t>, Span),
    CannotAdd((Expr<'t>, Ty), (Expr<'t>, Ty), Span),
    CannotSub((Expr<'t>, Ty), (Expr<'t>, Ty), Span),
}

impl<'t> InterpreterError<'t> {
    #[must_use]
    pub const fn span(&self) -> &Span {
        match self {
            Self::NotImplemented(.., span) => span,
            Self::CannotAdd(.., span) => span,
            Self::CannotSub(.., span) => span,
        }
    }

    #[must_use]
    pub fn diagnostic(&self) -> String {
        match self {
            Self::NotImplemented(value, ..) => {
                format!("feature {} not implemented", value.name().fg(Color::Red))
            }
            Self::CannotAdd((_, l), (_, r), ..) => {
                format!(
                    "cannot add values of type {} and {} together",
                    format!("{l}").fg(Color::Red),
                    format!("{r}").fg(Color::Blue)
                )
            }
            Self::CannotSub((_, l), (_, r), ..) => {
                format!(
                    "cannot subtract values of type {} and {} together",
                    format!("{l}").fg(Color::Red),
                    format!("{r}").fg(Color::Blue)
                )
            }
        }
    }

    #[must_use]
    pub fn note(&self) -> Option<String> {
        match self {
            Self::CannotAdd((f, l), (e, r), ..) | Self::CannotSub((f, l), (e, r), ..) => {
                if let Ty::Set(l) = l {
                    if l.ty_eq(r) {
                        return Some(format!(
                            "{} {} {} {} {} {}",
                            "wrap the right-hand value in set delimiters,".fg(Color::Black),
                            e,
                            "->".fg(Color::Black),
                            "{".fg(Color::Green),
                            e,
                            "}".fg(Color::Green)
                        ));
                    }
                } else if let Ty::Set(r) = r {
                    if r.ty_eq(l) {
                        return Some(format!(
                            "{} {} {} {} {} {}",
                            "wrap the left-hand value in set delimiters,".fg(Color::Black),
                            f,
                            "->".fg(Color::Black),
                            "{".fg(Color::Green),
                            f,
                            "}".fg(Color::Green)
                        ));
                    }
                }

                None
            }

            _ => None,
        }
    }
}
