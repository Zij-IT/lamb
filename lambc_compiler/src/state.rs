mod diagnostics;
mod pathmap;

use std::path::PathBuf;

use miette::{Diagnostic, Severity};

use self::diagnostics::Diagnostics;
pub use self::pathmap::{PathMap, PathRef};

/// The state of compilation, which contains information relevant to all steps
/// of compilation, such as `diagnostics`.
// todo: the mapping of `Var` to `Ident` should also be located here so that
// it is possible to use the names in error messages post name-resolution.
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
    /// Constructs a new empty `State`.
    pub fn new() -> Self {
        Self {
            diagnostics: Diagnostics::default(),
            has_errors: false,
            pathmap: PathMap::new(),
        }
    }

    /// Adds a source file path, and returns a key representing that source file.
    pub fn add_path<P: Into<PathBuf>>(&mut self, path: P) -> PathRef {
        self.pathmap.insert(path)
    }

    /// Gets the file-path to which the key corresponds.
    pub fn resolve_path(&self, pr: PathRef) -> &std::path::Path {
        self.pathmap.resolve(pr).expect("There should be only one pathmap being used throughout compilation")
    }

    /// Adds an error into the state which is later able to displayed to the user
    pub fn add_error<T>(&mut self, err: T, source: Option<PathRef>)
    where
        T: Diagnostic + Send + Sync + 'static,
    {
        self.has_errors = self.has_errors
            || matches!(err.severity(), None | Some(Severity::Error));

        self.diagnostics.add_error(source, err)
    }

    /// Returns whether there have been errors discovered
    pub fn has_errors(&self) -> bool {
        self.has_errors
    }
}
