#![warn(clippy::pedantic)]

use miette::Result;

mod cli;

fn main() -> Result<()> {
    human_panic::setup_panic!();
    let cli::LambOptions {
        debug_level: _,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    Ok(lambc_vm::run_script(path)?)
}
