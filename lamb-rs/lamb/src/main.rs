#![warn(clippy::pedantic)]

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

    Ok(match path.as_ref().map(std::fs::read_to_string) {
        Some(src) => run_input(&src?),
        None => run_repl()?,
    })
}

fn run_repl() -> Result<(), repl::Error> {
    let mut lamb = repl::Repl::new()?;
    match lamb.with_history() {
        Ok(_) => (),
        Err(err) => println!("[Lamb]: Error while loading history ({err})"),
    }

    print!("{}", repl::Repl::REPL_START);

    let mut lines = String::with_capacity(32);
    loop {
        match lamb.read_line()? {
            Command::Quit => return Ok(()),
            Command::Run => break,
            Command::String(s) => lines.push_str(&s),
        }
    }

    Ok(run_input(&lines))
}

fn run_input(input: &str) {
    match lamb_parse::script(&input) {
        Ok(s) => lamb_vm::run_script(&s),
        Err(errs) => report::errors(&input, None, &errs, "[Lamb] Syntax Errors:"),
    }
}
