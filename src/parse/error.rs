use ariadne::{Color, Fmt};
use chumsky::error::Error;

use crate::{lex::token::Token, spans::span::{Span, Spanned}};

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError {
    ExpectedFound(Span, Vec<Option<Spanned<Token>>>, Option<Spanned<Token>>),
}

impl ParserError {
    #[must_use]
    pub const fn span(&self) -> Span {
        match self {
            Self::ExpectedFound(s, _, _) => *s,
        }
    }

    #[must_use]
    pub fn diagnostic(&self) -> String {
        match self {
            Self::ExpectedFound(.., e, f) => format!(
                "expected {}, found {}",
                (if e.is_empty() {
                    "something else".fg(Color::Red).to_string()
                } else {
                    e.iter()
                        .map(|x| {
                            x.as_ref().map_or_else(|| String::from("end of input"), |x| format!("{x:?}"))
                                .fg(Color::Red)
                                .to_string()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }),
                f.as_ref().map_or_else(|| String::from("end of input"), |x| format!("{x:?}"))
                    .fg(Color::Green)
            ),
        }
    }
}

impl<'a> Error<'a, &'a [Spanned<Token>]> for ParserError {
    fn expected_found<
        E: IntoIterator<
            Item = Option<
                chumsky::util::MaybeRef<
                    'a,
                    <&'a [Spanned<Token>] as chumsky::prelude::Input<'a>>::Token,
                >,
            >,
        >,
    >(
        expected: E,
        found: Option<
            chumsky::util::MaybeRef<'a, <&'a [Spanned<Token>] as chumsky::prelude::Input<'a>>::Token>,
        >,
        span: <&'a [Spanned<Token>] as chumsky::prelude::Input<'a>>::Span,
    ) -> Self {
        Self::ExpectedFound(
            span,
            expected
                .into_iter()
                .map(|e| e.as_deref().cloned())
                .collect(),
            found.as_deref().cloned(),
        )
    }
}
