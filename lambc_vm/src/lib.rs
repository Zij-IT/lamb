//! The Lamb Bytecode Virtual Machine
//!
//! This module defines the bytecode machine which runs the `Lamb` language.
//! The virtual machine is based off of the work [Crafting Interpreters](https://craftinginterpreters.com/)
//! by Robert Nystrom, with modifications made to conform to the rules of the
//! Rust language and the design of Lamb.
//!
//! This crate defines a [virtual machine](`Vm`), a [garbage collector](`LambGc`)
//! as well as a ByteCode [`Backend`](`Backend`).
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

/// A simple helper-function which runs the script located at `path`,
/// outputing any errors during compilation.
///
/// Note: this function will return `Ok(())` if there were compilation errors,
/// and the errors will be output to `stderr`.
//
// todo: write the stderr to a `Write` and use `Err(..)` for when there was
// a compilation error.
pub fn run_script<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let mut gc = LambGc::new();
    let mut compiler = Compiler::new(Backend::new(&mut gc));

    let Ok(exe) = compiler.build(path.as_ref().to_path_buf()) else {
        let mut buffer = String::from("Compilation Errors:\n");
        let handler = miette::GraphicalReportHandler::new();
        for diagnostic in compiler.get_diagnostics() {
            _ = handler.render_report(&mut buffer, diagnostic.as_ref());
        }

        return Ok(std::io::stderr().write_all(buffer.as_bytes())?);
    };

    let mut vm = Vm::new(&mut gc);
    vm.load_exe(exe)?;
    vm.run()
}
