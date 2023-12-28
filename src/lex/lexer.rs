use std::fmt::Display;

use chumsky::prelude::*;

use crate::spans::span::{Span, Spanned};

use super::token::Token;

pub fn lexer<'s>() -> impl Parser<'s, &'s str, Vec<Spanned<Token>>, extra::Err<Rich<'s, char, Span>>>
{
    let eof = end().to(Token::Eof);
    let natural = text::int(10)
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Natural)
        .boxed();
    let real = text::int(10)
        .then(just('.').then(text::digits(10)))
        .to_slice()
        .from_str()
        .unwrapped()
        .map(Token::Real)
        .boxed();
    let bool = choice((just("true").to(Token::True), just("false").to(Token::False))).boxed();
    let label = text::ident().map(|x: &str| Token::Label(x.to_string()));

    let sym_syntax = choice((
        just("+").or(just("\\plus")).to(Token::Plus),
        just("->").to(Token::Arrow),
        just("-").or(just("\\minus")).to(Token::Minus),
        just("*").or(just("\\asterisk")).to(Token::Asterisk),
        just("/").or(just("\\slash")).to(Token::Slash),
        just("%").or(just("\\percent")).to(Token::Percent),
        // just("\\").to(Token::Backslash),
        just("==").to(Token::Identical),
        just("=").or(just("\\eq")).to(Token::Eq),
        just("!=").to(Token::Ne),
        just("≠").or(just("\\ne")).to(Token::Ne),
        just(">=").to(Token::Ge),
        just(">").to(Token::Gt),
        just("≥").or(just("\\ge")).to(Token::Ge),
        just("<=").to(Token::Le),
        just("<").to(Token::Lt),
        just("≤").or(just("\\le")).to(Token::Le),
        just("≡").or(just("\\identical")).to(Token::Identical),
        just(":=").or(just("\\defined")).to(Token::Defined),
        just("::").to(Token::DoubleColon),
        just(";").to(Token::Semicolon),
    ));
    let sym_other = choice((
        just("'").to(Token::Apostrophe),
        just(":").to(Token::Colon),
        just("→").or(just("\\arrow")).to(Token::Arrow),
    ));
    let sym_grouping = choice((
        just("[").to(Token::LeftBracket),
        just("]").to(Token::RightBracket),
        just("(").to(Token::LeftParen),
        just(")").to(Token::RightParen),
        just("{").to(Token::LeftBrace),
        just("}").to(Token::RightBrace),
        just(",").to(Token::Comma),
    ));
    let sym_set = choice((
        just("∪").or(just("\\union")).to(Token::Union),
        just("∩").or(just("\\intersection")).to(Token::Intersection),
        just("∅").or(just("\\emptyset")).to(Token::EmptySet),
        just("∈").or(just("\\in")).to(Token::In),
        just("∉").or(just("\\notin")).to(Token::NotIn),
        just("∋").or(just("\\contains")).to(Token::Contains),
        just("∌ ").or(just("\\notcontains")).to(Token::NotContains),
        just("∀").or(just("\\forall")).to(Token::ForAll),
        just("∃").or(just("\\exists")).to(Token::Exists),
        just("∄").or(just("\\notexists")).to(Token::NotExists),
        just("⊂").or(just("\\subset")).to(Token::Subset),
        just("⊆").or(just("\\subseteq")).to(Token::SubsetEq),
        just("⊄").or(just("\\subsetne")).to(Token::SubsetNe),
        just("⊃").or(just("\\superset")).to(Token::Superset),
        just("⊇").or(just("\\superseteq")).to(Token::SupersetEq),
        just("⊅").or(just("\\supersetne")).to(Token::SupersetNe),
    ));
    let sym_logic = choice((
        just("∧").or(just("\\and")).or(just("&&")).to(Token::And),
        just("∨").or(just("\\or")).or(just("||")).to(Token::Or),
        just("&").to(Token::Ampersand),
        just("|").to(Token::Pipe),
        just("⊻").or(just("\\xor")).or(just("^^")).to(Token::Xor),
        just("^").to(Token::Caret),
        just("⊼").or(just("\\nand")).or(just("~&")).to(Token::Nand),
        just("⊽").or(just("\\nor")).or(just("~|")).to(Token::Nor),
        just("¬").or(just("~")).or(just("\\not")).to(Token::Not),
    ));
    let sym_math = choice((
        just("×").or(just("\\cross")).to(Token::Cross),
        just("·").or(just("\\dot")).to(Token::Interpunct),
        just("∞").or(just("\\infinity")).to(Token::Infinity),
    ));

    let sym = choice((
        sym_syntax,
        sym_other,
        sym_grouping,
        sym_set,
        sym_logic,
        sym_math,
    ));

    let token = real.or(natural).or(bool).or(sym).or(label);

    let comment = just("//")
        .then(any().and_is(just('\n').not()).repeated())
        .padded()
        .boxed();

    token
        .map_with(|t, e| Spanned(t, e.span()))
        .padded_by(comment.repeated())
        .padded()
        .recover_with(skip_then_retry_until(any().ignored(), eof.ignored()))
        .repeated()
        .collect()
}

pub fn refmt(s: impl Display) -> String {
    let binding = s.to_string();
    let tokens = lexer().parse(&binding).unwrap();
    tokens
        .iter()
        .map(|x| x.0.to_string())
        .collect::<Vec<_>>()
        .join(" ")
}
