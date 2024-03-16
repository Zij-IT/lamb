mod exe;
mod module_parser;
mod state;

use std::path::{Path, PathBuf};

use lambc_parse::{Expr, Import, Module, Parser, Statement};
use module_parser::{ModuleParser, ParsedModule};

pub use self::{
    exe::{CompiledImport, CompiledModule, Exe},
    state::State,
};
use crate::{bytecode::Lowerer, gc::LambGc};

const REPL: &'static str = "\0REPL\0";

pub type Result<T> = core::result::Result<T, Error>;

pub enum Error {
    /// The path provided was unable to be built. Information
    /// is available in the diagnostics.
    Invalid,
}

pub struct Compiler<'gc> {
    is_for_repl: bool,
    state: State<'gc>,
}

impl<'gc> Compiler<'gc> {
    pub fn new(gc: &'gc mut LambGc) -> Self {
        Self { is_for_repl: false, state: State::new(gc) }
    }

    pub fn build_from_source(&mut self, source: String) -> Result<Exe> {
        let parsed = self.parse_modules_from_source(source.as_bytes())?;
        let compiled = self.compile_modules(parsed)?;
        Ok(self.build_exe(REPL.into(), compiled))
    }

    pub fn build_from_path(&mut self, path: PathBuf) -> Result<Exe> {
        let main = path.canonicalize().unwrap_or(path);
        let parsed =
            ModuleParser::new(&mut self.state).parse(vec![main.clone()]);
        let compiled = self.compile_modules(parsed)?;
        Ok(self.build_exe(main, compiled))
    }

    fn compile_modules(
        &mut self,
        parsed: Vec<ParsedModule>,
    ) -> Result<Vec<CompiledModule>> {
        let name = self.state.gc.intern(" __MODULE__ ");
        let compiled = parsed
            .into_iter()
            .map(|m| {
                let main_path = &m.path;
                let path = self.state.gc.intern(m.path.to_string_lossy());
                let code =
                    Lowerer::new(name, path).lower(self.state.gc, &m.ast);
                let imports = self.compile_imports(main_path, m.ast.imports);
                CompiledModule {
                    // TODO: This should be caught in analysis
                    export: m.ast.exports.into_iter().next(),
                    imports,
                    code,
                    path,
                }
            })
            .collect();

        if self.state.has_errors() {
            Err(Error::Invalid)
        } else {
            Ok(compiled)
        }
    }

    fn compile_imports(
        &mut self,
        main: &Path,
        imports: Vec<Import>,
    ) -> Vec<CompiledImport> {
        let parent = main.parent().expect("Can't run a directory :D");
        imports
            .into_iter()
            .map(|i| {
                let path = i.file.text.as_ref().map_or("", |t| &t.inner);
                let path = parent.join(path);
                let path = path.canonicalize().unwrap_or(path);
                let path = self.state.gc.intern(path.to_string_lossy());
                CompiledImport { raw: i, path }
            })
            .collect()
    }

    fn build_exe(
        &mut self,
        main: PathBuf,
        compiled: Vec<CompiledModule>,
    ) -> Exe {
        let main = self.state.gc.intern(main.to_string_lossy());
        Exe {
            main,
            modules: compiled.into_iter().map(|cm| (cm.path, cm)).collect(),
        }
    }

    fn parse_modules_from_source(
        &mut self,
        source: &[u8],
    ) -> Result<Vec<ParsedModule>> {
        let mut parser = Parser::new(source, REPL);
        let module = match parser.parse_module() {
            Ok(module) => ParsedModule { ast: module, path: REPL.into() },
            Err(err) if self.is_for_repl => {
                self.attempt_repl_expr(source, err)?
            }
            Err(err) => {
                self.state.add_error(err);
                return Err(Error::Invalid);
            }
        };

        let curr_path = std::fs::canonicalize(".").expect("No.");
        let import_paths = module
            .ast
            .imports
            .iter()
            .map(|i| {
                curr_path.join(i.file.text.as_ref().map_or("", |t| &t.inner))
            })
            .collect();

        let mut parsed =
            ModuleParser::new(&mut self.state).parse(import_paths);
        parsed.insert(0, module);
        Ok(parsed)
    }

    pub fn enable_repl_exprs(&mut self) {
        self.is_for_repl = true;
    }

    fn attempt_repl_expr(
        &mut self,
        source: &[u8],
        original_err: lambc_parse::Error,
    ) -> Result<ParsedModule> {
        let mut expr = Parser::new(source, REPL);
        let Ok(expr) = expr.parse_expr_end() else {
            self.state.add_error(original_err);
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
