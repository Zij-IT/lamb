mod bytecode;
mod chunk;
mod exe;
mod gc;
mod value;
mod vm;

use std::path::Path;

use lambc_compiler::Compiler;

pub use self::bytecode::Backend;
pub use crate::{
    gc::LambGc,
    vm::{Error, Vm},
};

pub fn run_script<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let mut gc = LambGc::new();
    let mut compiler = Compiler::new(Backend::new(&mut gc));

    let Ok(exe) = compiler.build(path.as_ref().to_path_buf()) else {
        _ = compiler.print_diagnostics();
        return Ok(());
    };

    drop(compiler);
    let mut vm = Vm::new(&mut gc);
    vm.load_exe(exe)?;
    vm.run()
}
