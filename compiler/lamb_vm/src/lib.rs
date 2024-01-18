use lamb_ast::Script;

mod chunk;
mod compiler;
mod gc;
mod value;
mod vm;

pub use vm::Vm;

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Runtime Error: {0}")]
    Runtime(#[from] vm::Error),
}

pub fn run_script(script: &Script) -> Result<(), Error> {
    let mut vm = Vm::new();
    vm.load_script(script);
    Ok(vm.run()?)
}
