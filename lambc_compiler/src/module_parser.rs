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
    #[error("Unable to read the file at '{}': '{}", .path.display(), .inner)]
    FailedToRead { path: PathBuf, inner: std::io::Error },
    #[error("This path is not a file: {}", .path.display())]
    ImportNotAFile {
        path: PathBuf,
        #[label]
        span: lambc_parse::Span,
    },
    #[error("This import path is empty")]
    EmptyImport {
        #[label]
        span: lambc_parse::Span,
    },
}

#[derive(Debug)]
pub struct ModuleParser<'b> {
    state: &'b mut State,
}

impl<'b> ModuleParser<'b> {
    pub fn new(state: &'b mut State) -> Self {
        Self { state }
    }

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
                                self.state.add_error(
                                    e,
                                    std::fs::read_to_string(path.clone()).ok(),
                                );
                                continue;
                            }
                        };

                    if scheduled.contains(&import_path) {
                        continue;
                    }

                    let import = Import {
                        file: self.state.add_path(&import_path),
                        items: import.items,
                        name: import.name,
                        star: import.star,
                        span: import.span,
                    };

                    scheduled.insert(import_path.clone());
                    pending.push(import_path);
                    imports.push(import);
                }

                let parsed = Module {
                    imports,
                    exports: module.exports,
                    statements: module.statements,
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
                self.state.add_error(err, input.into());
                None
            }
        }
    }

    fn import_path_from(
        &self,
        import: &lambc_parse::Import<PathBuf>,
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
