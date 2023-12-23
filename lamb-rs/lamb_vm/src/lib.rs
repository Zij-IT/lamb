use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

use compiler::Compiler;
use vm::Vm;

pub fn run_script(script: &Script) {
    let mut gc = gc::LambGc::new();
    let name = gc.intern("__LAMB__SCRIPT__");
    let mut compiler = Compiler::new(name);

    compiler.compile(&mut gc, script);
    let closure = compiler.finish(&mut gc);
    let vm = Vm::new(gc);
    vm.exec(closure);
}
