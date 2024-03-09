#![warn(clippy::pedantic)]

use lambc_parse::{Call, Expr, ExprStatement, Ident, Module, Parser, Statement};
use repl::Command;
use std::{error::Error, path::Path};

mod cli;
mod repl;

fn main() -> Result<(), Box<dyn Error>> {
    human_panic::setup_panic!();

    let cli::LambOptions {
        debug_level: _,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    match path.as_ref().map(|p| (p, std::fs::read_to_string(p))) {
        Some((path, src)) => run_input(path.canonicalize().unwrap(), &src?),
        None => run_repl()?,
    }

    Ok(())
}

fn run_input<P: AsRef<Path>>(path: P, input: &str) {
    let mut parser = Parser::new(input.as_bytes(), path.as_ref());
    match parser.parse_module() {
        Ok(s) => {
            if let Err(err) = lambc_vm::run_script(path.as_ref(), &s) {
                println!("{}: {err}", path.as_ref().display());
            }
        }
        Err(err) => eprintln!("{}: {err}", path.as_ref().display()),
    }
}

fn run_repl() -> Result<(), repl::Error> {
    let mut lamb = repl::Repl::new()?;
    match lamb.with_history() {
        Ok(_) => (),
        Err(err) => println!("[Lamb]: Error while loading history ({err})"),
    }

    print!("{}", repl::Repl::REPL_START);

    let mut vm = lambc_vm::Vm::new();
    let path = std::fs::canonicalize(".").unwrap().join("repl");
    loop {
        match lamb.read_line()? {
            Command::Quit => return Ok(()),
            Command::Run => break,
            Command::String(s) => match extract_script(&s) {
                Ok(script) => {
                    if let Err(err) = vm.load_script(&script, &path) {
                        println!("{err}");
                        continue;
                    }

                    if let Err(err) = vm.run() {
                        println!("{err}");
                    }
                }
                Err(err) => {
                    eprintln!("{err}");
                    continue;
                }
            },
        }
    }

    Ok(())
}

fn extract_script(input: &str) -> Result<Module, lambc_parse::Error> {
    let mut parser = Parser::new(input.as_bytes(), "repl.lb");
    match parser.parse_module() {
        Ok(script) => Ok(script),
        Err(_) => {
            let mut parser = Parser::new(input.as_bytes(), "repl.lb");
            let expr = parser.parse_expr()?;
            let span = expr.span();
            let stat = wrap_expr(expr);
            Ok(Module {
                span,
                exports: vec![],
                imports: vec![],
                path: "repl.lb".into(),
                statements: vec![stat],
            })
        }
    }
}

fn wrap_expr(expr: Expr) -> Statement {
    if let Expr::Call(call) = &expr {
        if let Expr::Ident(Ident { raw, .. }) = &call.callee {
            if raw == "println" || raw == "print" {
                return Statement::Expr(ExprStatement {
                    span: expr.span(),
                    expr,
                });
            }
        }
    }

    let span = expr.span();
    Statement::Expr(ExprStatement {
        expr: Expr::Call(Box::new(Call {
            callee: Expr::Ident(Ident {
                raw: "println".into(),
                span,
            }),
            args: vec![expr],
            span,
        })),
        span,
    })
}
