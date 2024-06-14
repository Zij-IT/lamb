mod module_parser;
mod name_res;
mod state;

use std::path::PathBuf;

use lambc_parse::{Ident, Module};
use module_parser::ModuleParser;

pub use self::state::{PathRef, State};

pub type Result<T> = core::result::Result<T, Error>;

pub enum Error {
    /// The path provided was unable to be built. Information
    /// is available in the diagnostics.
    Invalid,
}

pub trait Backend {
    type Output;

    fn build(
        &mut self,
        state: &mut State,
        main: PathRef,
        parsed: Vec<Module<Ident, PathRef>>,
    ) -> Result<Self::Output>;
}

pub struct Compiler<B: Backend> {
    backend: B,
    state: State,
}

impl<B: Backend> Compiler<B> {
    pub fn new(backend: B) -> Self {
        Self { backend, state: State::new() }
    }

    pub fn build(&mut self, main: PathBuf) -> Result<B::Output> {
        let parsed =
            ModuleParser::new(&mut self.state).parse(vec![main.clone()]);

        let main = self.state.add_path(main);
        self.pipeline(main, parsed)
    }

    pub fn diagnostics(&self) -> &[miette::Report] {
        &self.state.diagnostics
    }

    fn pipeline(
        &mut self,
        main: PathRef,
        parsed: Vec<Module<Ident, PathRef>>,
    ) -> Result<B::Output> {
        if self.state.has_errors() {
            return Err(Error::Invalid);
        }

        let mut resolver = name_res::Resolver::new(&mut self.state);
        _ = resolver.resolve_modules(parsed.clone());

        if self.state.has_errors() {
            return Err(Error::Invalid);
        }

        self.backend.build(&mut self.state, main, parsed)
    }
}
