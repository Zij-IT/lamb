use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;

use compiler::Compiler;

fn compile_script(script: &Script) {
    let name = gc::GcRef::new();
    let mut compiler = Compiler::for_script(name);
    compiler.compile_script(script);
}
