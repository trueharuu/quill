use chumsky::{prelude::*, primitive::select};

use crate::{
    common::vox::Vox,
    interp::value::Value,
    lex::token::Token,
    spans::span::{ApplySpan, Spanned},
};

macro_rules! just {
    (|$p:ident, $s:ident| $t:pat => $u:expr) => {
        select(move |$p: Spanned<Token>, $s| match $p.value() {
            $t => Some($u),
            _ => None,
        })
    };

    ($($t:tt)*) => {select(move |t: Spanned<Token>, _| {match t.value() {
        $($t)* => Some($($t)*.apply_span(t.span())),
        _ => None,
    }})
}
}

macro_rules! join {
    ($l:expr, $r:expr) => {
        $l.span().union($r.span())
    };
}

use super::{error::ParserError, expr::Expr};
#[must_use]
pub fn parser<'t, 's: 't>(
) -> impl Parser<'t, &'t [Spanned<Token>], Vec<Spanned<Expr>>, chumsky::extra::Err<ParserError>> + Clone
{
    let label: _ =
        just!(|t, _s| Token::Label(l) => Expr::Label(l.clone()).apply_span(t.span()).vox()).boxed();
    let semi: _ = just!(Token::Semicolon).boxed();
    let expr: _ = recursive(|e| {
        let kgroup = group((just!(Token::LeftParen), e.clone(), just!(Token::RightParen)))
            .map(|(l, e, r)| Expr::Group(l.clone(), e, r.clone()).apply_span(join!(l, r)));

        let set = group((
            just!(Token::LeftBrace),
            e.clone()
                .map(|x| *x)
                .separated_by(just!(Token::Comma))
                .allow_trailing()
                .collect(),
            just!(Token::RightBrace),
        ))
        .map(|(l, es, r)| Expr::DefinedSet(l.clone(), es, r.clone()).apply_span(join!(l, r)));

        let tuple = group((
            just!(Token::LeftParen),
            e.clone()
                .map(|x| *x)
                .separated_by(just!(Token::Comma))
                .allow_trailing()
                .collect(),
            just!(Token::RightParen),
        ))
        .map(|(l, es, r)| Expr::Tuple(l.clone(), es, r.clone()).apply_span(join!(l, r)));

        macro_rules! op {
            (1, $rely:ident, [$($token:path => $expr:path$(,)?)*]) => {
                choice((
                    $(just!($token).map(|x| (x, $expr as fn(_, _) -> Expr)),)*
                )).repeated().foldr($rely.clone(), |(t, f), b| f(t.clone(), b.clone()).apply_span(join!(t, b)).vox())
            };
            (2, $rely:ident, [$($token:path => $expr:path$(,)?)*]) => {
                $rely.clone()
                    .foldl(
                        choice(($(
                            just!($token).map(|x| (x, $expr as fn(_, _, _) -> Expr)),
                        )*)).then($rely)
                        .repeated(),
                        |a, ((o, f), b)| f(a.clone(), o, b.clone()).apply_span(join!(a, b)).vox()
                    )
                    .boxed()
            };
        }

        let primary = select(move |t: Spanned<Token>, _| match t.value() {
            Token::Label(l) => Some(Expr::Label(l.clone()).apply_span(t.span())),
            Token::Natural(l) => Some(Expr::Literal(Value::Natural(*l)).apply_span(t.span())),
            Token::Real(l) => Some(Expr::Literal(Value::Real(*l)).apply_span(t.span())),
            Token::True => Some(Expr::Literal(Value::Bool(true)).apply_span(t.span())),
            Token::False => Some(Expr::Literal(Value::Bool(false)).apply_span(t.span())),
            Token::EmptySet => Some(Expr::Literal(Value::Set(Vec::new())).apply_span(t.span())),

            _ => None,
        })
        .or(kgroup.clone())
        .or(tuple.clone())
        .or(set.clone())
        .map(Box::new)
        .boxed();

        let call = primary
            .clone()
            .foldl(
                group((
                    just!(Token::LeftParen),
                    e
                        .clone()
                        .map(|x| *x)
                        .separated_by(just!(Token::Comma))
                        .allow_trailing()
                        .collect(),
                    just!(Token::RightParen),
                ))
                .repeated(),
                |a, (l, es, r)| {
                    Expr::Call(a.clone(), l, es, r.clone())
                        .apply_span(join!(a, r))
                        .vox()
                },
            )
            .boxed();

        let unary = op!(1, call, [
            Token::Not => Expr::Not,
            Token::Minus => Expr::Neg,
            Token::ForAll => Expr::ForAll,
            Token::Exists => Expr::Exists,
            Token::NotExists => Expr::NotExists,
            Token::Asterisk => Expr::Drop,
        ]);

        let product = op!(2, unary, [
            Token::Asterisk => Expr::Mul,
            Token::Slash => Expr::Div,
            Token::Interpunct => Expr::DotProduct,
            Token::Cross => Expr::CrossProduct,
            Token::Percent => Expr::Modulus,
        ]);

        let sum = op!(2, product, [
            Token::Plus => Expr::Add,
            Token::Minus => Expr::Sub,
        ]);

        let cmp = op!(2, sum, [
            Token::Eq => Expr::Eq,
            Token::Ne => Expr::Ne,
            Token::Gt => Expr::Gt,
            Token::Ge => Expr::Ge,
            Token::Lt => Expr::Lt,
            Token::Le => Expr::Le,

            Token::In => Expr::In,
            Token::NotIn => Expr::NotIn,
            Token::Contains => Expr::Contains,
            Token::NotContains => Expr::NotContains,
            Token::Subset => Expr::Subset,
            Token::Subset => Expr::SubsetEq,
            Token::Subset => Expr::SubsetNe,
            Token::Superset => Expr::Superset,
            Token::Superset => Expr::SupersetEq,
            Token::Superset => Expr::SupersetNe,

            Token::Identical => Expr::Identical,
        ]);

        let and_equiv = op!(2, cmp, [
            Token::Intersection => Expr::Intersection,
            Token::And => Expr::And,
            Token::Nand => Expr::Nand,
        ]);

        let xor_equiv = op!(2, and_equiv, [
            Token::Xor => Expr::Xor,
        ]);

        let or_equiv = op!(2, xor_equiv, [
            Token::Union => Expr::Union,
            Token::Or => Expr::Or,
        ]);

        let map = group((
            or_equiv.clone(),
            just!(Token::Arrow),
            select(move |t: Spanned<Token>, _| match t.value() {
                Token::Label(..) => Some(t),
                _ => None,
            }),
            just!(Token::LeftBracket),
            or_equiv.clone(),
            just!(Token::RightBracket),
        ))
        .map(|(a, b, c, d, e, f)| {
            Expr::Map(a.clone(), b, c, d, e, f.clone())
                .apply_span(join!(a, f))
                .vox()
        })
        .or(or_equiv);

        map
    });
    let def = group((
        label.clone(),
        just!(Token::Defined),
        expr.clone(),
        semi.clone().or_not().ignored(),
    ))
    .map(|(x, t, y, ())| Expr::Define(x.clone(), t, y.clone()).apply_span(join!(x, y)))
    .boxed();
    let view = group((just!(Token::DoubleColon), expr, semi.or_not().ignored()))
        .map(|(x, y, ())| Expr::View(x.clone(), y.clone()).apply_span(x.span().union(y.span())));
    let stmt = choice((def, view));
    stmt.repeated().collect()
}
