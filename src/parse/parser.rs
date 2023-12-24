use chumsky::{prelude::*, primitive::select};

use crate::{
    common::{set::Set, vox::Vox},
    interp::value::Value,
    lex::token::Token,
    spans::span::{span, ApplySpan, Spanned},
};

use super::{error::ParserError, expr::Expr};
#[must_use]
pub fn parser<'t, 's: 't>(
) -> impl Parser<'t, &'t [Token<'s>], Vec<Spanned<Expr<'s>>>, chumsky::extra::Err<ParserError<'s>>> + Clone
{
    let label = select(move |t, s| match t {
        Token::Label(l) => Some(Expr::Label(l).apply_span(s.span()).vox()),
        _ => None,
    });

    // let natural = select(move |t, s| match t {
    //     Token::Natural(n) => Some(n.apply_span(s.span())),
    //     _ => None,
    // });

    let expr = recursive(|e| {
        let kgroup = group((
            just(Token::LeftParen).map_with(span),
            e.clone(),
            just(Token::RightParen).map_with(span),
        ))
        .map_with(|(left_paren, expr, right_paren), s| {
            Expr::Group(left_paren, expr, right_paren).apply_span(s.span())
        });

        let set = group((
            just(Token::LeftBrace).map_with(span),
            e.clone()
                .map(|x| *x)
                .separated_by(just(Token::Comma))
                .allow_trailing()
                .collect(),
            just(Token::RightBrace).map_with(span),
        ))
        .map_with(|(l, es, r), s| Expr::DefinedSet(l, es, r).apply_span(s.span()))
        .boxed();

        let predicate_set = group((
            just(Token::LeftBrace).map_with(span),
            label,
            just(Token::Colon).map_with(span),
            e.clone(),
            just(Token::RightBrace).map_with(span),
        ))
        .map_with(|(l, x, c, p, r), s| Expr::PredicateSet(l, x, c, p, r).apply_span(s.span()));

        macro_rules! op {
            (1, $rely:ident, [$($token:path => $expr:path$(,)?)*]) => {
                choice(($(just($token).map_with(span).map(|x| (x, $expr as fn(_, _) -> Expr<'s>)),)*))
                    .repeated()
                    .foldr_with($rely.clone(), |(t, f), b, e| f(t, b).apply_span(e.span()).vox())
                    .boxed()
            };
            (2, $rely:ident, [$($token:path => $expr:path$(,)?)*]) => {
                $rely.clone()
                    .foldl_with(
                        choice(($(
                            just($token).map_with(span).map(|x| (x, $expr as fn(_, _, _) -> Expr<'s>)),
                        )*)).then($rely)
                        .repeated(),
                        |a, ((o, f), b), e| f(a, o, b).apply_span(e.span()).vox()
                    )
                    .boxed()
            };
            (I, $rely:ident, [$($l:path | $c:path | $r:path => $e:expr$(,)?)*]) => {
                choice(($(group((just($l).map_with(span), $rely.clone(), just($c).map_with(span), $rely.clone(), just($r).map_with(span))).map(|x| (x, $e as fn(_,_,_,_,_)->Expr<'s>)).map_with(|((a,b,c,d,e),f),s| f(a,b,c,d,e).apply_span(s.span())).boxed(),)*))
            }
        }

        let interval = op!(
            I,
            e,
            [
                Token::LeftParen | Token::Comma | Token::RightParen => Expr::IntervalOO,
                Token::LeftParen | Token::Comma | Token::RightBracket => Expr::IntervalOC,
                Token::LeftBracket | Token::Comma | Token::RightParen => Expr::IntervalCO,
                Token::LeftBracket | Token::Comma | Token::RightBracket => Expr::IntervalCC,
            ]
        );

        let primary = select(move |t, _| match t {
            Token::Label(l) => Some(Expr::Label(l)),
            Token::Natural(t) => Some(Expr::Literal(Value::Natural(t))),
            Token::Real(t) => Some(Expr::Literal(Value::Real(t))),
            Token::True => Some(Expr::Literal(Value::Bool(true))),
            Token::False => Some(Expr::Literal(Value::Bool(false))),
            Token::EmptySet => Some(Expr::Literal(Value::Set(Set::empty()))),

            _ => None,
        })
        .map_with(span)
        .or(interval.clone())
        .or(kgroup.clone())
        .or(set.clone())
        .or(predicate_set.clone())
        .map(Box::new)
        .boxed();

        // let product_op = choice((
        //     op!(2s, Token::Asterisk => Expr::Mul),
        //     op!(2s, Token::Slash => Expr::Div),
        // ));

        // let product = primary
        //     .clone()
        //     .foldl_with(product_op.then(primary).repeated(), |a, ((o, f), b), e| {
        //         f(a, o, b).apply_span(e.span()).vox()
        //     });

        let unary = op!(1, primary, [
            Token::Not => Expr::Not,
            Token::Minus => Expr::Neg,
            Token::ForAll => Expr::ForAll,
            Token::Exists => Expr::Exists,
            Token::NotExists => Expr::NotExists,
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

        or_equiv
    });
    let define = group((label, just(Token::Defined).map_with(span), expr.clone()))
        .map_with(|(label, defined_token, expr), s| {
            Expr::Define(label, defined_token, expr).apply_span(s.span())
        })
        .boxed();

    let view = group((just(Token::DoubleColon).map_with(span), expr))
        .map_with(|(view_token, expr), s| Expr::View(view_token, expr).apply_span(s.span()));

    let stmt = choice((define, view)).boxed();

    stmt.repeated().at_least(1).collect()
}
