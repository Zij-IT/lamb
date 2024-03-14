use std::path::Path;

use lambc_parse::Module;

mod bytecode;
mod chunk;
mod gc;
mod value;
mod vm;

pub use vm::Error;
pub use vm::Vm;

pub fn run_script<P: AsRef<Path>>(path: P, script: &Module) -> Result<(), Error> {
    let mut vm = Vm::new();
    vm.load_script(script, path)?;
    vm.run()
}
