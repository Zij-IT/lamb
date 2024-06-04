#![warn(clippy::pedantic)]

use std::error::Error;

mod cli;

fn main() -> Result<(), Box<dyn Error>> {
    human_panic::setup_panic!();

    let cli::LambOptions {
        debug_level: _,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    Ok(lambc_vm::run_script(path)?)
}
