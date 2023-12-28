use std::{fmt::Debug, hash::Hash};

use ariadne::{Color, Label, ReportBuilder};

use crate::{
    common::{etc::Painted, named::Named},
    lex::token::Token,
    parse::expr::Expr,
    spans::span::{Span, Spanned},
};

use super::{ty::Ty, value::Value};
#[derive(Debug)]
pub enum InterpreterError {
    // expr, span
    NotImplemented(Spanned<Expr>, Span),
    // verb, (expr, value), token, (expr, value), span
    CannotBinary(
        String,
        (Spanned<Expr>, Value),
        Token,
        (Spanned<Expr>, Value),
        Span,
    ),
    // verb, token, (expr, value), span
    CannotUnary(String, Token, (Spanned<Expr>, Value), Span),
    // expr, element, span
    SetElementIsNotOfType(Spanned<Expr>, Value, Ty, Span),
    // expr, expr, span
    DivisionByZero(Spanned<Expr>, Spanned<Expr>, Span),
    // label, span
    VariableDoesNotExist(String, Span),
    // label, last_use, span
    VariableNoLongerExists(String, Spanned<Expr>, Span),
    // token, expr, span
    UselessDrop(Spanned<Token>, Spanned<Expr>, Span),
    // type, expr, span
    MapOfWrongType(Ty, Spanned<Expr>, Span),
    // type, expr, span
    CallOnWrongType(Ty, Spanned<Expr>, Span),
    // name, label, arity, given_arity, expr, call_site, span
    MissingArgument(String, String, usize, usize, Spanned<Expr>, Span, Span),
    // expected_ty, found_ty, span
    ExpectedFound(Ty, Ty, Span),
    Custom(String, Span),
}

impl InterpreterError {
    #[must_use]
    pub const fn span(&self) -> &Span {
        match self {
            Self::CannotBinary(.., span)
            | Self::CannotUnary(.., span)
            | Self::DivisionByZero(.., span)
            | Self::NotImplemented(.., span)
            | Self::VariableDoesNotExist(.., span)
            | Self::VariableNoLongerExists(.., span)
            | Self::UselessDrop(.., span)
            | Self::SetElementIsNotOfType(.., span)
            | Self::MapOfWrongType(.., span)
            | Self::CallOnWrongType(.., span)
            | Self::Custom(.., span)
            | Self::MissingArgument(.., span)
            | Self::ExpectedFound(.., span) => span,
        }
    }

    pub fn diagnostic<Id: Debug + Hash + Eq + Clone>(
        &self,
        id: Id,
        report: &mut ReportBuilder<'_, (Id, std::ops::Range<usize>)>,
    ) {
        match self {
            Self::NotImplemented(path, span) => {
                report.set_message(format!(
                    "feature {} not yet implemented",
                    path.name().painted().fg(Color::Red)
                ));
                report.add_label(
                    Label::new((id, span.into_range()))
                        .with_color(Color::Red)
                        .with_message("not yet implemented"),
                );
            }
            Self::VariableNoLongerExists(label, last_use, span) => {
                report.set_message(format!(
                    "variable {} no longer exists",
                    label.painted().fg(Color::Red)
                ));
                report.add_label(
                    Label::new((id.clone(), span.into_range()))
                        .with_color(Color::Red)
                        .with_message("no longer exists"),
                );
                report.add_label(
                    Label::new((id, last_use.span().into_range()))
                        .with_color(Color::Blue)
                        .with_message("variable consumed here"),
                );
                report.set_note(format!(
                    "the {} operator marks the value unusable",
                    Token::Asterisk.painted().fg(Color::Red)
                ));
            }
            Self::VariableDoesNotExist(label, span) => {
                report.set_message(format!(
                    "variable {} does not exist",
                    label.painted().fg(Color::Red)
                ));
                report.add_label(
                    Label::new((id, span.into_range()))
                        .with_color(Color::Red)
                        .with_message("does not exist".painted().fg(Color::Red)),
                );
                report.set_help(format!(
                    "define it with the syntax {}",
                    format!("`{label} {} ...`", Token::Defined)
                        .painted()
                        .fg(Color::Red)
                ));
            }
            Self::UselessDrop(t, v, _) => {
                report.set_message("attempt to call drop with no location");
                report.add_label(
                    Label::new((id.clone(), v.span().into_range()))
                        .with_message(
                            "expression does not reference anything"
                                .painted()
                                .fg(Color::Blue),
                        )
                        .with_color(Color::Blue),
                );
                report.add_label(
                    Label::new((id, t.span().into_range()))
                        .with_color(Color::Red)
                        .with_message("error occurs here".painted().fg(Color::Red)),
                );
                report.set_note(format!(
                    "the {} operator is only available on references to variables",
                    "*".painted().fg(Color::Blue)
                ));
                report.set_help(format!("remove the {}", "*".painted().fg(Color::Blue)));
            }
            Self::CannotBinary(verb, (l, lhs), token, (r, rhs), _) => {
                report.set_message(format!(
                    "cannot {verb} with {}",
                    format!("[{token}]").painted().fg(Color::Blue)
                ));
                report.add_label(
                    Label::new((id.clone(), l.span().into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "because this is of type {}",
                            lhs.ty().painted().fg(Color::Red)
                        )),
                );
                report.add_label(
                    Label::new((id, r.span().into_range()))
                        .with_color(Color::Blue)
                        .with_message(format!(
                            "and this is of type {}",
                            rhs.ty().painted().fg(Color::Blue)
                        )),
                );
            }

            Self::CannotUnary(verb, token, (l, lhs), _) => {
                report.set_message(format!(
                    "cannot {verb} with {}",
                    format!("[{token}]").painted().fg(Color::Blue)
                ));
                report.add_label(
                    Label::new((id, l.span().into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "because this is of type {}",
                            lhs.ty().painted().fg(Color::Red)
                        )),
                );

                if lhs.ty() == Ty::Bool && *token == Token::Minus {
                    report.set_help(format!(
                        "use {} to negate the proposition instead",
                        format!("[{}]", Token::Not).painted().fg(Color::Blue)
                    ));
                }
            }
            Self::DivisionByZero(_, b, s) => {
                report.set_message("division by zero");
                report.add_label(
                    Label::new((id.clone(), s.into_range()))
                        .with_color(Color::Red)
                        .with_message("error occurs here".painted().fg(Color::Red)),
                );
                report.add_label(
                    Label::new((id, b.span().into_range()))
                        .with_color(Color::Blue)
                        .with_message("value here is zero".painted().fg(Color::Blue)),
                );
            }
            Self::SetElementIsNotOfType(expr, element, ty, _) => {
                report.set_message(format!(
                    "an element of this set is not of type {}",
                    ty.painted().fg(Color::Blue)
                ));
                report.add_label(
                    Label::new((id, expr.span().into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "contains the element {} which is of type {}",
                            element.painted().fg(Color::Blue),
                            element.ty().painted().fg(Color::Red),
                        )),
                );
            }

            Self::MapOfWrongType(ty, expr, _) => {
                report.set_message("cannot map this value");
                report.add_label(
                    Label::new((id, expr.span().into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "because it has type {}",
                            ty.painted().fg(Color::Red)
                        )),
                );
            }
            Self::CallOnWrongType(ty, expr, _) => {
                report.set_message("expression is not callable");
                report.add_label(
                    Label::new((id, expr.span().into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "because it has type {}",
                            ty.painted().fg(Color::Red)
                        )),
                );
            }

            Self::Custom(err, span) => {
                report.set_message("custom error");
                report.add_label(
                    Label::new((id, span.into_range()))
                        .with_message(err)
                        .with_color(Color::Yellow),
                );
            }

            Self::MissingArgument(name, label, arity, given_arity, expr, call_site, _) => {
                report.set_message(format!(
                    "missing argument for fn {}",
                    format!("[{name}]").painted().fg(Color::Blue)
                ));
                report.add_label(
                    Label::new((id.clone(), call_site.into_range()))
                        .with_color(Color::Red)
                        .with_message(format!(
                            "the argument {} was not given a value",
                            format!("[{label}]").painted().fg(Color::Red)
                        ))
                        .with_order(0),
                );
                report.add_label(
                    Label::new((id, expr.span().into_range()))
                        .with_color(Color::Blue)
                        .with_message(format!(
                            "{} has arity {} but was given {} arguments",
                            format!("[{name}]").painted().fg(Color::Blue),
                            arity.painted().fg(Color::Green),
                            given_arity.painted().fg(Color::Yellow),
                        ))
                        .with_order(1),
                );
            }

            Self::ExpectedFound(expected, found, span) => {
                report.set_message("expected a different type");
                report.add_label(
                    Label::new((id, span.into_range()))
                        .with_color(Color::Yellow)
                        .with_message(format!(
                            "expected a value of type {} but recieved {}",
                            expected.painted().fg(Color::Blue),
                            found.painted().fg(Color::Green)
                        )),
                );
            }
            #[allow(unreachable_patterns, clippy::match_wildcard_for_single_variants)]
            _ => todo!(),
        }
    }
}
