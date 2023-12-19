use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;

use compiler::Compiler;

pub fn run_script(script: &Script) {
    let name = gc::GcRef::new("__LAMB__SCRIPT__");
    let mut compiler = Compiler::for_script(name);
    compiler.compile_script(script);
    let _ = compiler.finish();
}
