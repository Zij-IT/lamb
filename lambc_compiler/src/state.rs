mod diagnostics;
mod pathmap;

use std::path::PathBuf;

use miette::{Diagnostic, Severity};

use self::diagnostics::Diagnostics;
pub use self::pathmap::{PathMap, PathRef};

pub struct State {
    pub diagnostics: Diagnostics,
    pathmap: PathMap,
    has_errors: bool,
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State").finish_non_exhaustive()
    }
}

impl Default for State {
    fn default() -> Self {
        Self::new()
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            diagnostics: Diagnostics::default(),
            has_errors: false,
            pathmap: PathMap::new(),
        }
    }

    pub fn add_path<P: Into<PathBuf>>(&mut self, path: P) -> PathRef {
        self.pathmap.insert(path)
    }

    pub fn resolve_path(&self, pr: PathRef) -> &std::path::Path {
        self.pathmap.resolve(pr).expect("There should be only one pathmap being used throughout compilation")
    }

    pub fn add_error<T>(&mut self, err: T, source: Option<PathRef>)
    where
        T: Diagnostic + Send + Sync + 'static,
    {
        self.has_errors = self.has_errors
            || matches!(err.severity(), None | Some(Severity::Error));

        self.diagnostics.add_error(source, err)
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }
}
