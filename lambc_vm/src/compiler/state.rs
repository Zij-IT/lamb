use miette::{Diagnostic, Report, Severity};

use crate::gc::LambGc;

pub struct State<'gc> {
    pub gc: &'gc mut LambGc,
    pub diagnostics: Vec<miette::Report>,
    has_errors: bool,
}

impl<'gc> std::fmt::Debug for State<'gc> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("State").finish_non_exhaustive()
    }
}

impl<'gc> State<'gc> {
    pub fn new(gc: &'gc mut LambGc) -> Self {
        Self { gc, diagnostics: vec![], has_errors: false }
    }

    pub fn add_error<T>(&mut self, err: T, source: Option<String>)
    where
        T: Diagnostic + Send + Sync + 'static,
    {
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
