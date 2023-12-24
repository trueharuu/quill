#![warn(
    clippy::perf,
    clippy::correctness,
    clippy::suspicious,
    clippy::nursery,
    clippy::complexity,
    clippy::pedantic
)]
#![allow(
    clippy::too_many_lines,
    clippy::module_name_repetitions,
    clippy::similar_names
)]

use ariadne::{sources, Config, Label, Report};
use chumsky::Parser;
use lex::lexer::lexer;

use crate::{parse::parser::parser, spans::span::Spanned};
pub mod common;
// pub mod error;
pub mod interp;
pub mod lex;
pub mod parse;
pub mod spans;

fn main() {
    let args = std::env::args().collect::<Vec<_>>();
    let options = &args[1];
    assert!(options.starts_with('-'));
    let input = &args[2];

    if options.contains('l') || options.contains('L') {
        let lexed = lexer().parse(input).into_output_errors();

        lexed
            .1
            .clone()
            .into_iter()
            .map(|x| x.map_token(|c| c.to_string()))
            .for_each(|x| {
                Report::build(
                    ariadne::ReportKind::Custom("lexing error", ariadne::Color::Cyan),
                    "test.q",
                    x.span().start,
                )
                .with_message(x.to_string())
                .with_label(
                    Label::new(("test.q", x.span().into_range()))
                        .with_message(x.reason().to_string())
                        .with_color(ariadne::Color::Red),
                )
                .finish()
                .eprint(sources([("test.q", input)]))
                .unwrap();
            });

        if let Some(tokens) = lexed.0 {
            if options.contains('L') {
                println!(
                    "{}",
                    tokens
                        .iter()
                        .map(|Spanned(x, _)| format!("{x:?}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                );
                println!(
                    "{}",
                    tokens
                        .iter()
                        .map(|Spanned(x, _)| format!("{x}"))
                        .collect::<Vec<_>>()
                        .join(" ")
                );
            }

            if options.contains('p') || options.contains('P') {
                let collect = tokens.iter().map(|x| x.0).collect::<Vec<_>>();
                let parsed = parser().parse(&collect).into_output_errors();

                parsed.1.clone().into_iter().for_each(|x| {
                    Report::build(
                        ariadne::ReportKind::Custom("parsing error", ariadne::Color::Magenta),
                        "test.q",
                        tokens.get(x.span().start).map_or(0, |x| x.1.start),
                    )
                    .with_message(x.diagnostic())
                    .with_label(
                        Label::new((
                            "test.q",
                            tokens.get(x.span().start).map_or(0, |x| x.1.start)
                                ..tokens.get(x.span().end - 1).map_or(0, |x| x.1.end),
                        ))
                        .with_message(x.diagnostic())
                        .with_color(ariadne::Color::Magenta),
                    )
                    .with_config(Config::default().with_compact(true))
                    .finish()
                    .eprint(sources([("test.q", input)]))
                    .unwrap();
                });

                if let Some(p) = parsed.0 {
                    if options.contains('P') {
                        println!("{p:#?}");
                        println!(
                            "{}",
                            p.iter()
                                .map(|x| format!("{x}"))
                                .collect::<Vec<_>>()
                                .join("\n")
                        );
                    }

                    if options.contains('i') {
                        if let Err(x) = interp::interpreter::many(&p) {
                            let mut r = Report::build(
                                ariadne::ReportKind::Custom(
                                    "interpreter error",
                                    ariadne::Color::Red,
                                ),
                                "test.q",
                                tokens.get(x.span().start).map_or(0, |x| x.1.start),
                            )
                            .with_message(x.diagnostic())
                            .with_label(
                                Label::new((
                                    "test.q",
                                    tokens.get(x.span().start).map_or(0, |x| x.1.start)
                                        ..tokens.get(x.span().end - 1).map_or(0, |x| x.1.end),
                                ))
                                .with_message(x.diagnostic())
                                .with_color(ariadne::Color::Magenta),
                            );

                            if let Some(s) = x.note() {
                                r.set_note(s);
                            }

                            // .with_config(Config::default().with_compact(true))
                            r.finish().eprint(sources([("test.q", input)])).unwrap();
                        };
                    }
                }
            }
        }
    }
}
