mod pathmap;

use std::path::PathBuf;

use miette::{Diagnostic, Report, Severity};

pub use self::pathmap::{PathMap, PathRef};

pub struct State {
    pub diagnostics: Vec<miette::Report>,
    pathmap: PathMap,
    has_errors: bool,
}

impl std::fmt::Debug for State {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State").finish_non_exhaustive()
    }
}

impl State {
    pub fn new() -> Self {
        Self {
            diagnostics: vec![],
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
        let source = source
            .map(|src| self.resolve_path(src))
            .and_then(|path| std::fs::read_to_string(path).ok());

        let report = match source {
            Some(source) => Report::new(err).with_source_code(source),
            None => Report::new(err),
        };

        self.add_report(report)
    }

    pub fn add_report(&mut self, report: Report) {
        self.has_errors = self.has_errors
            || matches!(report.severity(), None | Some(Severity::Error));

        self.diagnostics.push(report)
    }

    pub fn has_errors(&self) -> bool {
        self.has_errors
    }
}
