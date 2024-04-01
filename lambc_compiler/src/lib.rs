mod module_parser;
mod state;

use std::{
    io::Write,
    marker::PhantomData,
    path::{Path, PathBuf},
};

use lambc_parse::{Expr, Module, Parser, Statement};
use module_parser::ModuleParser;

pub use self::{module_parser::ParsedModule, state::State};

mod sealed {
    pub struct Repl;
    pub struct File;
    pub trait CompilerKind {}

    impl CompilerKind for Repl {}
    impl CompilerKind for File {}
}

use sealed::*;

const REPL: &'static str = "\0REPL\0";

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

pub struct Compiler<B: Backend, K: CompilerKind = File> {
    backend: B,
    state: State,
    _phantom: PhantomData<K>,
}

impl<B: Backend, K: CompilerKind> Compiler<B, K> {
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

impl<B: Backend> Compiler<B, File> {
    pub fn new(backend: B) -> Self {
        Self { backend, state: State::new(), _phantom: PhantomData }
    }

    pub fn build(&mut self, path: PathBuf) -> Result<B::Output> {
        let main = path.canonicalize().unwrap_or(path);
        let parsed =
            ModuleParser::new(&mut self.state).parse(vec![main.clone()]);

        self.pipeline(main, parsed)
    }
}

impl<B: Backend> Compiler<B, Repl> {
    pub fn new_for_repl(backend: B) -> Self {
        Self { backend, state: State::new(), _phantom: PhantomData }
    }

    pub fn build(&mut self, source: String) -> Result<B::Output> {
        let parsed = self.parse_modules_from_source(source.as_bytes())?;
        self.pipeline(REPL.into(), parsed)
    }

    fn parse_modules_from_source(
        &mut self,
        source: &[u8],
    ) -> Result<Vec<ParsedModule>> {
        let mut parser = Parser::new(source, REPL);
        let module = match parser.parse_module() {
            Ok(module) => ParsedModule { ast: module, path: REPL.into() },
            Err(err) => self.attempt_repl_expr(source, err)?,
        };

        let curr_path = std::fs::canonicalize(".").expect("No.");
        let import_paths = module
            .ast
            .imports
            .iter()
            .map(|i| {
                let import_path =
                    Path::new(i.file.text.as_ref().map_or("", |t| &t.inner));

                if import_path.is_relative() {
                    curr_path.join(import_path)
                } else {
                    import_path.to_path_buf()
                }
            })
            .collect();

        let mut parsed =
            ModuleParser::new(&mut self.state).parse(import_paths);
        parsed.insert(0, module);
        Ok(parsed)
    }

    fn attempt_repl_expr(
        &mut self,
        source: &[u8],
        original_err: lambc_parse::Error,
    ) -> Result<ParsedModule> {
        let mut expr = Parser::new(source, REPL);
        let Ok(expr) = expr.parse_expr_end() else {
            self.state.add_error(
                original_err,
                // Normally I would expect the input to be utf-8, however for
                // the repl we just add replacement characters.
                Some(String::from_utf8_lossy(source).into_owned()),
            );

            return Err(Error::Invalid);
        };

        Ok(Self::wrap_expr(expr))
    }

    // Wraps an expression in a "println" call if the function is not already
    // wrapped in a println or print.
    fn wrap_expr(expr: Expr) -> ParsedModule {
        let span = expr.span();
        ParsedModule {
            ast: Module {
                exports: vec![],
                imports: vec![],
                statements: vec![Statement::Expr(
                    lambc_parse::ExprStatement {
                        expr: Expr::Call(Box::new(lambc_parse::Call {
                            callee: Expr::Ident(lambc_parse::Ident {
                                raw: "println".into(),
                                span,
                            }),
                            args: vec![expr],
                            span,
                        })),
                        span,
                    },
                )],
                path: REPL.into(),
                span,
            },
            path: REPL.into(),
        }
    }
}
