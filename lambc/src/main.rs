#![warn(clippy::pedantic)]

use lambc_vm::{Compiler, LambGc};
use repl::Command;
use std::error::Error;

mod cli;
mod repl;

fn main() -> Result<(), Box<dyn Error>> {
    human_panic::setup_panic!();

    let cli::LambOptions {
        debug_level: _,
        gc_debug_level: _,
        optimization_level: _,
        path,
    } = cli::LambOptions::parse();

    match path {
        Some(path) => lambc_vm::run_script(path)?,
        None => run_repl()?,
    }

    Ok(())
}

fn run_repl() -> Result<(), repl::Error> {
    let mut lamb = repl::Repl::new()?;
    match lamb.with_history() {
        Ok(_) => (),
        Err(err) => eprintln!("[Lamb]: Error while loading history ({err})"),
    }

    print!("{}", repl::Repl::REPL_START);

    let mut gc = LambGc::new();
    let mut vm = lambc_vm::Vm::new(&mut gc);
    loop {
        match lamb.read_line()? {
            Command::Quit => return Ok(()),
            Command::Run => break,
            Command::String(s) => {
                let mut compiler = Compiler::new(vm.gc_mut());
                compiler.enable_repl_exprs();
                let Ok(exe) = compiler.build_from_source(s) else {
                    eprintln!("Error: wait for diagnostics...");
                    // And now I lie :D
                    return Ok(());
                };

                if let Err(err) = vm.load_exe(exe) {
                    println!("{err}");
                    continue;
                }

                if let Err(err) = vm.run() {
                    println!("{err}");
                }
            }
        }
    }

    Ok(())
}
