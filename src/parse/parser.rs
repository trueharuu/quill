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
    let stmt: _ = recursive(|s| {
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
                Token::Real(l) => Some(Expr::Literal(Value::Real(*l)).apply_span(t.span())),
                Token::String(l) => {
                    Some(Expr::Literal(Value::String(l.clone().into())).apply_span(t.span()))
                }

                Token::True => Some(Expr::Literal(Value::Bool(true)).apply_span(t.span())),
                Token::False => Some(Expr::Literal(Value::Bool(false)).apply_span(t.span())),
                Token::EmptySet => Some(Expr::Literal(Value::Set([].vox())).apply_span(t.span())),

                _ => None,
            })
            .or(kgroup.clone())
            .or(tuple.clone())
            .or(set.clone())
            .map(Box::new)
            .boxed();

            let access = primary.clone().foldl(
                group((just!(Token::Colon), label.clone())).repeated(),
                |a, (l, e)| {
                    Expr::Access(a.clone(), l, e.clone())
                        .apply_span(join!(a, e))
                        .vox()
                },
            );
            let call = access
                .clone()
                .foldl(
                    group((
                        just!(Token::LeftParen),
                        e.clone()
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
                Token::Percent => Expr::Rem,
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
        .boxed()
        .map(Box::new);

        let reassign = group((
            label.clone(),
            just!(Token::Arrow),
            expr.clone(),
            semi.clone().or_not(),
        ))
        .map(|(x, t, y, _)| {
            Expr::Reassign(x.clone(), t, y.clone())
                .apply_span(join!(x, y))
                .vox()
        });

        let block =
            group((just!(Token::LeftBrace), s, just!(Token::RightBrace))).map(|(lb, es, rb)| {
                Expr::Block(lb.clone(), es, rb.clone())
                    .apply_span(join!(lb, rb))
                    .vox()
            });

        let kw_if = recursive(|k| {
            group((
                just!(Token::If),
                just!(Token::LeftParen),
                expr.clone(),
                just!(Token::RightParen),
                block.clone(),
                just!(Token::Else).or_not(),
                k.or(block.clone()).or_not(),
            ))
            .map(|(k_if, lp, c, rp, b, et, e)| {
                Expr::If(k_if.clone(), lp, c, rp, b.clone(), et, e.clone())
                    .apply_span(join!(k_if, e.unwrap_or(b)))
                    .vox()
            })
        });

        let kw_fn = group((
            just!(Token::Fn),
            label.clone(),
            just!(Token::LeftParen),
            label
                .map(|x| *x)
                .separated_by(just!(Token::Comma))
                .allow_trailing()
                .collect(),
            just!(Token::RightParen),
            block.clone(),
        ))
        .map(|(k_fn, label, left_paren, argv, right_paren, block)| {
            Expr::Fn(
                k_fn.clone(),
                label,
                left_paren,
                argv,
                right_paren,
                block.clone(),
            )
            .apply_span(join!(k_fn, block))
            .vox()
        });

        let kw_return = group((
            just!(Token::Return),
            expr.clone(),
            just!(Token::Semicolon).or_not(),
        ))
        .map(|(k_ret, expr, _)| {
            Expr::Return(k_ret.clone(), expr.clone())
                .apply_span(join!(k_ret, expr))
                .vox()
        });

        let kw_throw = group((
            just!(Token::Throw),
            expr.clone(),
            just!(Token::Semicolon).or_not(),
        ))
        .map(|(k_ret, expr, _)| {
            Expr::Throw(k_ret.clone(), expr.clone())
                .apply_span(join!(k_ret, expr))
                .vox()
        });

        choice((
            def,
            reassign,
            kw_fn,
            kw_if,
            kw_throw,
            kw_return,
            block,
            expr.then_ignore(just!(Token::Semicolon)),
        ))
        .map(|x| *x)
        .repeated()
        .collect()
    });

    stmt
}
