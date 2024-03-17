mod bytecode;
mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

use std::path::Path;

pub use crate::{
    compiler::Compiler,
    gc::LambGc,
    vm::{Error, Vm},
};

pub fn run_script<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let mut gc = LambGc::new();
    let mut compiler = Compiler::new(&mut gc);
    let Ok(exe) = compiler.build(path.as_ref().to_path_buf()) else {
        _ = compiler.print_diagnostics();
        return Ok(());
    };

    let mut vm = Vm::new(&mut gc);
    vm.load_exe(exe)?;
    vm.run()
}
