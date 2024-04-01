mod bytecode;
mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

use std::path::{Path, PathBuf};

use bytecode::Lowerer;
use compiler::{
    Backend, CompiledImport, CompiledModule, Exe, ParsedModule, State,
};

pub use crate::{
    compiler::Compiler,
    gc::LambGc,
    vm::{Error, Vm},
};

pub fn vm_backend(gc: &mut LambGc) -> impl Backend<Output = Exe> + '_ {
    |state: &mut State, main: PathBuf, parsed: Vec<ParsedModule>| {
        let name = gc.intern(" __MODULE__ ");
        let compiled = parsed
            .into_iter()
            .map(|m: ParsedModule| {
                let main_path = &m.path;
                let path = gc.intern(m.path.to_string_lossy());
                let code = Lowerer::new(gc, state, name, path).lower(&m.ast);

                let parent =
                    main_path.parent().expect("Can't run a directory :D");

                let imports = m
                    .ast
                    .imports
                    .into_iter()
                    .map(|i| {
                        let path =
                            i.file.text.as_ref().map_or("", |t| &t.inner);
                        let path = parent.join(path);
                        let path = path.canonicalize().unwrap_or(path);
                        let path = gc.intern(path.to_string_lossy());
                        CompiledImport { raw: i, path }
                    })
                    .collect();

                CompiledModule {
                    // TODO: This should be caught in analysis
                    export: m.ast.exports.into_iter().next(),
                    imports,
                    code,
                    path,
                }
            })
            .map(|cm| (cm.path, cm))
            .collect();

        let main = gc.intern(main.to_string_lossy());
        Exe { main, modules: compiled }
    }
}

pub fn run_script<P: AsRef<Path>>(path: P) -> Result<(), Error> {
    let mut gc = LambGc::new();
    let mut compiler = Compiler::new(vm_backend(&mut gc));

    let Ok(exe) = compiler.build(path.as_ref().to_path_buf()) else {
        _ = compiler.print_diagnostics();
        return Ok(());
    };

    drop(compiler);
    let mut vm = Vm::new(&mut gc);
    vm.load_exe(exe)?;
    vm.run()
}
