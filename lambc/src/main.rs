#![warn(clippy::pedantic)]

use miette::Result;

mod cli;

fn main() -> Result<()> {
    human_panic::setup_panic!();

    let cli::LambOptions { path } = cli::LambOptions::parse();
    Ok(lambc_vm::run_script(path)?)
}
