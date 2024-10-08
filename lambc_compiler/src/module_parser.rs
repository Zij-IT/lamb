use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use lambc_parse::{Ident, Import, Module, Parser};
use miette::Diagnostic;
use thiserror::Error as ThError;

use super::State;
use crate::state::PathRef;

#[derive(Debug, Diagnostic, ThError)]
#[diagnostic()]
enum Error {
    #[diagnostic(code("file::unreadable"))]
    #[error("Unable to read the file at '{}': '{}", .path.display(), .inner)]
    FailedToRead { path: PathBuf, inner: std::io::Error },
    #[diagnostic(code("file::not-a-file"))]
    #[error("This path is not a file: {}", .path.display())]
    ImportNotAFile {
        path: PathBuf,
        #[label]
        span: lambc_parse::Span,
    },
    #[diagnostic(code("file::empty-import"))]
    #[error("This import path is empty")]
    EmptyImport {
        #[label]
        span: lambc_parse::Span,
    },
}

/// A Parser responsible for parsing all files connected to the input file via
/// imports.
#[derive(Debug)]
pub struct ModuleParser<'b> {
    state: &'b mut State,
}

impl<'b> ModuleParser<'b> {
    /// Constructs a new `ModuleParser` which writes errors to `state`.
    pub fn new(state: &'b mut State) -> Self {
        Self { state }
    }

    /// Parses files starting with the `initial` list. The list returned
    /// is guarunteed to be traversed in a BFS manner. Given the files
    ///
    /// ```text
    /// -- File A:
    /// from "file_b.lb" import { a, b };
    /// from "file_c.lb" import { a, b };
    ///
    /// -- File B:
    /// from "file_d.lb" import { a, b };
    ///
    /// -- File C:
    /// from "file_e.lb" import { a, b };
    /// ```
    ///
    /// the output order will be as follows: a, b, c, d, e
    pub fn parse(
        &mut self,
        initial: Vec<PathBuf>,
    ) -> Vec<Module<Ident, PathRef>> {
        let mut scheduled = HashSet::<PathBuf>::from_iter(initial.clone());
        let mut pending = initial;
        let mut modules = vec![];

        while let Some(path) = pending.pop() {
            if let Some(module) = self.parse_module(path.as_path()) {
                let mut imports = Vec::new();
                for import in module.imports {
                    let import_path =
                        match self.import_path_from(&import, &path) {
                            Ok(p) => p,
                            Err(e) => {
                                let path = self.state.add_path(path.clone());
                                self.state.add_error(e, Some(path));
                                continue;
                            }
                        };

                    let import = Import {
                        file: self.state.add_path(&import_path),
                        items: import.items,
                        name: import.name,
                        star: import.star,
                        span: import.span,
                        path_span: import.path_span,
                    };

                    imports.push(import);

                    if scheduled.contains(&import_path) {
                        continue;
                    }

                    scheduled.insert(import_path.clone());
                    pending.push(import_path);
                }

                let parsed = Module {
                    imports,
                    exports: module.exports,
                    items: module.items,
                    path: self.state.add_path(module.path),
                    span: module.span,
                };

                modules.push(parsed);
            }
        }

        modules
    }

    fn parse_module(&mut self, path: &Path) -> Option<Module<Ident, PathBuf>> {
        let input = match std::fs::read_to_string(path) {
            Ok(b) => b,
            Err(err) => {
                self.state.add_error(
                    Error::FailedToRead { path: path.into(), inner: err },
                    None,
                );
                return None;
            }
        };
        let mut parser = Parser::new(input.as_bytes(), path);
        match parser.parse_module() {
            Ok(module) => Some(module),
            Err(err) => {
                let pref = self.state.add_path(path);
                self.state.add_error(err, Some(pref));
                None
            }
        }
    }

    fn import_path_from(
        &self,
        import: &lambc_parse::Import<Ident, PathBuf>,
        path: &Path,
    ) -> Result<PathBuf, Error> {
        let import_path = import.file.as_path();
        if import_path == Path::new("") {
            return Err(Error::EmptyImport { span: import.span });
        }

        let origin = path.parent().unwrap_or(path);
        let import_path = origin.join(import_path);
        let import_path = import_path.canonicalize().unwrap_or(import_path);

        if !import_path.is_file() {
            return Err(Error::ImportNotAFile {
                path: import_path,
                span: import.span,
            });
        }

        Ok(import_path)
    }
}
