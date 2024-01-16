use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

use vm::Vm;

pub fn run_script(script: &Script) {
    let mut vm = Vm::new();
    vm.load_script(script);
    vm.run();
}
