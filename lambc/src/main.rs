#![warn(clippy::pedantic)]

use lambc_parse::{Atom, Block, Expr, FuncCall, Ident, Script, Statement, SyntaxResult};
use repl::Command;
use std::{error::Error, path::Path};

mod cli;
mod optimization;
mod repl;
mod report;

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
    match lambc_parse::script(input) {
        Ok(s) => {
            if let Err(err) = lambc_vm::run_script(path, &s) {
                println!("{err}");
            }
        }
        Err(errs) => report::errors(input, None, &errs, "[Lamb] Syntax Errors:"),
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
                    vm.load_script(&script, &path);
                    if let Err(err) = vm.run() {
                        println!("{err}");
                    }
                }
                Err(errs) => {
                    report::errors(&s, None, &errs, "[Lamb] Syntax Errors:");
                    continue;
                }
            },
        }
    }

    Ok(())
}

fn extract_script(input: &str) -> SyntaxResult<Script> {
    match lambc_parse::script(input) {
        Ok(script) => Ok(script),
        Err(_) => {
            let expr = lambc_parse::expr(input)?;
            let stat = wrap_expr(expr);
            Ok(Script {
                exports: None,
                imports: vec![],
                block: Block {
                    stats: vec![stat],
                    value: None,
                },
            })
        }
    }
}

fn wrap_expr(expr: Expr) -> Statement {
    if let Expr::FuncCall(FuncCall { callee, args: _ }) = &expr {
        if let Expr::Atom(Atom::Ident(Ident(name))) = &**callee {
            if name == "println" || name == "print" {
                return Statement::Expr(expr);
            }
        }
    }

    Statement::Expr(Expr::FuncCall(FuncCall {
        callee: Box::new(Expr::Atom(Atom::Ident(Ident("println".into())))),
        args: vec![expr],
    }))
}
