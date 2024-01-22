#![warn(clippy::pedantic)]

use lamb_ast::{Atom, Block, Expr, FuncCall, Ident, Script, Statement};
use lamb_parse::SyntaxResult;
use repl::Command;
use std::error::Error;

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

    match path.as_ref().map(std::fs::read_to_string) {
        Some(src) => run_input(&src?),
        None => run_repl()?,
    }

    Ok(())
}

fn run_input(input: &str) {
    match lamb_parse::script(input) {
        Ok(s) => {
            if let Err(err) = lamb_vm::run_script(&s) {
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

    let mut vm = lamb_vm::Vm::new();
    loop {
        match lamb.read_line()? {
            Command::Quit => return Ok(()),
            Command::Run => break,
            Command::String(s) => match extract_script(&s) {
                Ok(script) => {
                    vm.load_script(&script);
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
    match lamb_parse::script(input) {
        Ok(script) => Ok(script),
        Err(_) => {
            let expr = lamb_parse::expr(input)?;
            let stat = wrap_expr(expr);
            Ok(Script {
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
