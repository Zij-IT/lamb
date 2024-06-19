mod module_parser;
mod name_res;
mod state;
mod type_check;

use std::path::PathBuf;

use lambc_parse::{Ident, Module};
use miette::NamedSource;
use module_parser::ModuleParser;
use type_check::TypeChecker;

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

    pub fn get_diagnostics(&mut self) -> Vec<miette::Report> {
        let diags = std::mem::take(&mut self.state.diagnostics);
        diags
            .inner
            .into_iter()
            .map(|(pref, diags)| {
                let path = pref.and_then(|p| {
                    let path = self.state.resolve_path(p);
                    std::fs::read_to_string(path).ok().map(|src| (path, src))
                });

                let report = miette::Report::new(diags);
                match path {
                    Some((p, src)) => {
                        let p = p.file_name().unwrap().to_string_lossy();
                        report.with_source_code(NamedSource::new(p, src))
                    }
                    _ => report,
                }
            })
            .collect()
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
        let resolved = resolver.resolve_modules(parsed.clone());
        if self.state.has_errors() {
            return Err(Error::Invalid);
        }

        let checker = &mut TypeChecker::new(&mut self.state);
        let _checked = checker.check_modules(resolved);

        if self.state.has_errors() {
            return Err(Error::Invalid);
        }

        self.backend.build(&mut self.state, main, parsed)
    }
}
