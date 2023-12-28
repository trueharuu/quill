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
    clippy::similar_names,
    clippy::missing_errors_doc,
    clippy::missing_panics_doc,
    clippy::missing_safety_doc,
    clippy::let_with_type_underscore
)]

use std::{mem::ManuallyDrop, time::Instant};

use ariadne::{sources, Label, Report};
use chumsky::Parser;
use lex::lexer::lexer;
use yansi::Color;

use crate::{
    common::etc::Painted, lex::escapes::escape, parse::parser::parser, spans::span::Spanned,
};
pub mod common;
// pub mod error;
pub mod interp;
pub mod lex;
pub mod parse;
pub mod spans;

fn main() {
    let args = ManuallyDrop::new(std::env::args().collect::<Vec<_>>());
    let options = &args[1];
    assert!(options.starts_with('-'));
    // dbg!(std::fs::canonicalize(args[2].strip_prefix("-f=").unwrap()));
    let mut input = args[2]
        .clone()
        .strip_prefix("-f=")
        .map_or_else(|| args[2].clone(), |x| std::fs::read_to_string(x).unwrap());
    let id = "test.q";

    // println!("\n{}", "Input:".painted().fg(yansi::Color::Green).bold());

    // remove escapes from source input.
    if options.contains('e') | options.contains('E') {
        let start = Instant::now();
        let output = escape(&input);

        println!("{output}");
        if options.contains('b') {
            println!(
                "{} {:?}",
                "[bench::fmt]".painted().fg(Color::Magenta),
                start.elapsed()
            );
        }
        if options.contains('E') {
            if let Some(t) = args[2].strip_prefix("-f=") {
                std::fs::write(t, output.clone()).unwrap();
            }
            input = output;
        }
    }

    if options.contains('l') || options.contains('L') {
        let start = Instant::now();
        let lexed = lexer().parse(&input).into_output_errors();
        if options.contains('b') {
            println!(
                "{} {:?}",
                "[bench::lexer]".painted().fg(Color::Magenta),
                start.elapsed()
            );
        }

        lexed
            .1
            .clone()
            .into_iter()
            .map(|x| x.map_token(|c| c.to_string()))
            .for_each(|x| {
                Report::build(
                    ariadne::ReportKind::Custom("lexing error", ariadne::Color::Cyan),
                    id,
                    x.span().start,
                )
                .with_message(x.to_string())
                .with_label(
                    Label::new((id, x.span().into_range()))
                        .with_message(x.reason().to_string())
                        .with_color(ariadne::Color::Cyan),
                )
                .finish()
                .eprint(sources([(id, &input)]))
                .unwrap();
            });

        if let Some(tokens) = lexed.0 {
            if options.contains('L') {
                println!(
                    "{}",
                    tokens
                        .iter()
                        .map(|x| format!("{x:#?}"))
                        .collect::<Vec<_>>()
                        .join("\n")
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
                let collect = tokens.clone().into_iter().collect::<Vec<_>>();
                let start = Instant::now();
                let parsed = parser().parse(&collect).into_output_errors();
                if options.contains('b') {
                    println!(
                        "{} {:?}",
                        "[bench::parser]".painted().fg(Color::Magenta),
                        start.elapsed()
                    );
                }

                parsed.1.clone().into_iter().for_each(|x| {
                    Report::build(
                        ariadne::ReportKind::Custom("parsing error", ariadne::Color::Magenta),
                        id,
                        tokens.get(x.span().start).map_or(0, |x| x.1.start),
                    )
                    .with_message(x.diagnostic())
                    .with_label(
                        Label::new((
                            id,
                            tokens.get(x.span().start).map_or(0, |x| x.1.start)
                                ..tokens.get(x.span().end - 1).map_or(0, |x| x.1.end),
                        ))
                        .with_message(x.diagnostic())
                        .with_color(ariadne::Color::Magenta),
                    )
                    // .with_config(Config::default().with_compact(true))
                    .finish()
                    .eprint(sources([(id, &input)]))
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
                        let start = Instant::now();
                        let out = interp::interpreter::many(&p);
                        if options.contains('b') {
                            println!(
                                "{} {:?}",
                                "[bench::interpreter]".painted().fg(Color::Magenta),
                                start.elapsed()
                            );
                        }
                        if let Err(x) = out {
                            // dbg!(&x);
                            let mut r = Report::build(
                                ariadne::ReportKind::Custom(
                                    "interpreter error",
                                    ariadne::Color::Red,
                                ),
                                id,
                                tokens.get(x.span().start).map_or(0, |x| x.1.start),
                            );

                            x.diagnostic(id, &mut r);

                            // .with_config(Config::default().with_compact(true))
                            r.finish().eprint(sources([(id, &input)])).unwrap();
                        };
                    }
                }
            }
        }
    }
}
