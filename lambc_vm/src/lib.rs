mod bytecode;
mod chunk;
mod exe;
mod gc;
mod value;
mod vm;

use std::{io::Write, path::Path};

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
        let mut buffer = String::new();
        let handler = miette::GraphicalReportHandler::new();
        for diagnostic in compiler.diagnostics() {
            _ = handler.render_report(&mut buffer, diagnostic.as_ref());
        }

        return Ok(std::io::stderr().write_all(buffer.as_bytes())?);
    };

    let mut vm = Vm::new(&mut gc);
    vm.load_exe(exe)?;
    vm.run()
}
