use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;

use compiler::Compiler;
use value::LambString;

pub fn run_script(script: &Script) {
    let mut gc = gc::LambGc::new();

    let name = LambString::new("__LAMB__SCRIPT__");
    let mut compiler = Compiler::new(gc.alloc(name));

    compiler.compile(&mut gc, script);
    let _ = compiler.finish(&mut gc);
}
