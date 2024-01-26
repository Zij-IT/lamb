use std::path::Path;

use lambc_parse::Script;

mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

pub use vm::Vm;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Runtime Error: {0}")]
    Runtime(#[from] vm::Error),
}

pub fn run_script<P: AsRef<Path>>(path: P, script: &Script) -> Result<(), Error> {
    let mut vm = Vm::new();
    vm.load_script(script, path);
    Ok(vm.run()?)
}
