use std::{
    collections::HashSet,
    path::{Path, PathBuf},
};

use lambc_parse::Parser;

use super::State;

#[derive(Debug)]
enum Error {
    FailedToRead { path: PathBuf, inner: std::io::Error },
    ImportNotAFile { path: PathBuf, span: lambc_parse::Span },
    EmptyImport { span: lambc_parse::Span },
    SyntaxError { inner: lambc_parse::Error },
}

#[derive(Debug)]
pub struct ParsedModule {
    pub ast: lambc_parse::Module,
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct ModuleParser<'b, 'state> {
    state: &'b mut State<'state>,
}

impl<'b, 'state> ModuleParser<'b, 'state> {
    pub fn new(state: &'b mut State<'state>) -> Self {
        Self { state }
    }

    pub fn parse(&mut self, initial: Vec<PathBuf>) -> Vec<ParsedModule> {
        let mut scheduled = HashSet::<PathBuf>::from_iter(initial.clone());
        let mut pending = initial;
        let mut modules = vec![];

        while let Some(path) = pending.pop() {
            if let Some(module) = self.parse_module(path.as_path()) {
                for import in Self::extract_imports(&module) {
                    let path = match import {
                        Ok(path) => path,
                        Err(err) => {
                            self.state.add_error(err);
                            continue;
                        }
                    };

                    if scheduled.contains(&path) {
                        continue;
                    }

                    scheduled.insert(path.clone());
                    pending.push(path);
                }

                modules.push(module);
            }
        }

        modules
    }

    fn parse_module(&mut self, path: &Path) -> Option<ParsedModule> {
        let input = match std::fs::read(path) {
            Ok(b) => b,
            Err(err) => {
                self.state.add_error(Error::FailedToRead {
                    path: path.into(),
                    inner: err,
                });
                return None;
            }
        };
        let mut parser = Parser::new(&input, path);
        match parser.parse_module() {
            Ok(module) => {
                Some(ParsedModule { ast: module, path: path.into() })
            }
            Err(err) => {
                self.state.add_error(Error::SyntaxError { inner: err });
                None
            }
        }
    }

    fn extract_imports(
        module: &ParsedModule,
    ) -> impl Iterator<Item = Result<PathBuf, Error>> + '_ {
        let parent = module.path.parent().unwrap_or(Path::new(""));
        module.ast.imports.iter().map(|i| {
            let file: Option<PathBuf> = i.file.text.as_ref().map(|i| {
                let path = parent.join(i.inner.as_str());
                path.canonicalize().unwrap_or(path)
            });

            match file {
                Some(f) if f.is_file() => Ok(f),
                // Error because the path is not a file
                Some(f) => {
                    Err(Error::ImportNotAFile { path: f, span: i.span })
                }
                // Error because the path was empty
                None => Err(Error::EmptyImport { span: i.span }),
            }
        })
    }
}
