#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use crate::{cli::DebugLevel, report::errors};

mod cli;
mod optimization;
mod repl;
mod report;

fn main() -> Result<(), Box<dyn std::error::Error>> {
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

    let Ok(script) = lamb_parse::parse_script(src.as_str(), path.as_deref(), errors, errors) else {
        return Ok(());  
    };

    lamb_ffi::run_script(
        &script,
        debug_level == DebugLevel::Full,
        debug_level != DebugLevel::None,
    );

    Ok(())
}
