use interner::Interner;
use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod interner;
mod value;
mod vm;

use compiler::Compiler;
use vm::Vm;

pub fn run_script(script: &Script) {
    let mut interner = Interner::new();
    let name = interner.intern("__LAMB__SCRIPT__");
    let mut compiler = Compiler::new(name);

    compiler.compile(&mut interner, script);
    let closure = compiler.finish();
    let vm = Vm::new(interner);
    vm.exec(closure);
}
