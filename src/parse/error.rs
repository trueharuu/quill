use ariadne::{Color, Fmt};
use chumsky::error::Error;

use crate::{lex::token::Token, spans::span::Span};

#[derive(Debug, PartialEq, Clone)]
pub enum ParserError<'t> {
    ExpectedFound(Span, Vec<Option<Token<'t>>>, Option<Token<'t>>),
}

impl<'t> ParserError<'t> {
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
                            x.map_or_else(|| String::from("end of input"), |x| format!("{x:?}"))
                                .fg(Color::Red)
                                .to_string()
                        })
                        .collect::<Vec<_>>()
                        .join(", ")
                }),
                f.map_or_else(|| String::from("end of input"), |x| format!("{x:?}"))
                    .fg(Color::Green)
            ),
        }
    }
}

impl<'a, 't> Error<'a, &'a [Token<'t>]> for ParserError<'t> {
    fn expected_found<
        E: IntoIterator<
            Item = Option<
                chumsky::util::MaybeRef<
                    'a,
                    <&'a [Token<'t>] as chumsky::prelude::Input<'a>>::Token,
                >,
            >,
        >,
    >(
        expected: E,
        found: Option<
            chumsky::util::MaybeRef<'a, <&'a [Token<'t>] as chumsky::prelude::Input<'a>>::Token>,
        >,
        span: <&'a [Token<'t>] as chumsky::prelude::Input<'a>>::Span,
    ) -> Self {
        Self::ExpectedFound(
            span,
            expected
                .into_iter()
                .map(|e| e.as_deref().copied())
                .collect(),
            found.as_deref().copied(),
        )
    }
}
