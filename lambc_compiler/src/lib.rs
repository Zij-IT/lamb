//! `lamb_compiler` defines and provides the tools necessary for compilation
//! of Lamb source files.
//!
//!
//! Among other things, this crate exports a [`Compiler`], the [`State`] of compilation
//! as well as a [`Backend`] trait to allow for Lamb source to be compiled to
//! multiple backends without being dependent on a specific implementation.
//!
//! # Design
//! ---
//!
//! The compiler is currently designed to take a single input file, and
//! output whatever type is specified by the chosen backend. The compiler
//! abandons the compilation if any errors are detected, though it should
//! never panic and should strive to produce error messages in line with
//! Rust's own.
//!
//! The start file and any ones imported are run through the compiler pipeline
//! which at this point consists of name resolution and type-checking.
//!
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

/// An error type representing whether compilation was successful or not.
/// If the compiler has output this error, there is information available
/// in the diagnostics of the compiler
pub enum Error {
    /// For reasons found in the diagnostics, the desired program was invalid
    Invalid,
}

/// A data structure that further compiles the output of the compiler into
/// another format.
///
/// This is likely premature abstraction, as the only implementor of this trait
/// is found within the `VM`, this provides a nice barrier between the compiler
/// itself and the final output. Similar to how `cargo` can compile code to either
/// `wasm`, assembly through `cranelift`, or optimized assembly through `LLVM`.
pub trait Backend {
    type Output;

    /// The final step of compilation, in which the compiler uses the backend
    /// to create the final output.
    ///
    /// Note: This function will only be called if there were no errors until
    /// this point. Currently that means that name-resolution and type-checking
    /// must have been successful for this to be called.
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
    /// Creates a new `Compiler` which uses the backend `B` to generate
    /// complete the compilation.
    pub fn new(backend: B) -> Self {
        Self { backend, state: State::new() }
    }

    /// Takes the file from which compilation should begin, and runs the input
    /// through the compilation pipeline.
    pub fn build(&mut self, main: PathBuf) -> Result<B::Output> {
        let parsed =
            ModuleParser::new(&mut self.state).parse(vec![main.clone()]);

        let main = self.state.add_path(main);
        self.pipeline(main, parsed)
    }

    /// Returns the diagnostics generated during the compilation process.
    ///
    /// Note: calling this function twice with no compilation in between will
    /// result in a list of diagnostics from the first compilation, followed
    /// by an empty list.
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

        let mut checker = TypeChecker::new(&mut self.state);
        let _checked = checker.check_modules(resolved);
        if self.state.has_errors() {
            return Err(Error::Invalid);
        }

        self.backend.build(&mut self.state, main, parsed)
    }
}
