#![warn(clippy::pedantic)]

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

    let src = match path.as_ref().map(std::fs::read_to_string) {
        Some(src) => src?,
        None => match repl::input()? {
            Some(src) => src,
            None => return Ok(()),
        },
    };

    let script = match lamb_parse::script(src.as_str()) {
        Ok(s) => s,
        Err(errs) => {
            report::errors(&src, path.as_deref(), &errs, "[Lamb] Syntax Errors:");
            return Ok(());
        }
    };

    lamb_vm::run_script(&script);

    Ok(())
}
