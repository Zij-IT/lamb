mod module_parser;
mod state;

use std::{io::Write, path::PathBuf};

use module_parser::ModuleParser;

pub use self::{module_parser::ParsedModule, state::State};

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
        main: PathBuf,
        parsed: Vec<ParsedModule>,
    ) -> Result<Self::Output>;
}

impl<T, R> Backend for T
where
    T: FnMut(&mut State, PathBuf, Vec<ParsedModule>) -> R,
{
    type Output = R;

    fn build(
        &mut self,
        state: &mut State,
        main: PathBuf,
        parsed: Vec<ParsedModule>,
    ) -> Result<Self::Output> {
        let val = self(state, main, parsed);
        if state.has_errors() {
            Err(Error::Invalid)
        } else {
            Ok(val)
        }
    }
}

pub struct Compiler<B: Backend> {
    backend: B,
    state: State,
}

impl<B: Backend> Compiler<B> {
    pub fn new(backend: B) -> Self {
        Self { backend, state: State::new() }
    }

    pub fn build(&mut self, path: PathBuf) -> Result<B::Output> {
        let main = path.canonicalize().unwrap_or(path);
        let parsed =
            ModuleParser::new(&mut self.state).parse(vec![main.clone()]);

        self.pipeline(main, parsed)
    }

    pub fn print_diagnostics(&self) -> std::fmt::Result {
        let mut buffer = String::new();

        let handler = miette::GraphicalReportHandler::new();
        for diagnostic in &self.state.diagnostics {
            handler.render_report(&mut buffer, diagnostic.as_ref())?;
        }

        // There's not really a lot to do here if for whatever reason
        _ = std::io::stderr().write_all(buffer.as_bytes());
        Ok(())
    }

    fn pipeline(
        &mut self,
        main: PathBuf,
        parsed: Vec<ParsedModule>,
    ) -> Result<B::Output> {
        self.backend.build(&mut self.state, main, parsed)
    }
}
