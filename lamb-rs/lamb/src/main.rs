#![warn(clippy::pedantic)]

use crate::cli::DebugLevel;

mod cli;
mod optimization;
mod repl;
mod report;

fn main() -> std::io::Result<()> {
    human_panic::setup_panic!();

    let cli::LambOptions {
        debug_level,
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

    lamb_ffi::run_script(
        &script,
        debug_level == DebugLevel::Full,
        debug_level != DebugLevel::None,
    );

    Ok(())
}
