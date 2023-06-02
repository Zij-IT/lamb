#![warn(clippy::pedantic)]
#![allow(
    clippy::cast_sign_loss,
    clippy::cast_possible_truncation,
    clippy::cast_possible_wrap
)]

use ast::Script;
use clap::Parser;
use cli::LambOptions;
use ffi::run_script;
use optimization::Optimize;

mod ast;
mod cli;
mod ffi;
mod optimization;

fn optimize(script: &mut Script) {
    while script.block.optimize() {
        println!("Script optimized:\n{script:#?}");
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let options = LambOptions::parse();
    let mut script = ffi::parse_script(&options.path)?;
    match options.optimization_level {
        cli::OptLevel::Basic | cli::OptLevel::Some | cli::OptLevel::All => optimize(&mut script),
        cli::OptLevel::None => (),
    }

    let (print_fns, print_main) = match options.debug_level {
        cli::DebugLevel::None => (false, false),
        cli::DebugLevel::Basic => (true, true),
        cli::DebugLevel::Full => {
            println!("{script:#?}");
            (true, true)
        }
    };

    run_script(&script, print_fns, print_main);
    Ok(())
}
