use std::collections::HashMap;

use miette::Diagnostic as MietteDiagnostic;

use crate::PathRef;

pub type MietteReportable = Box<dyn MietteDiagnostic + Send + Sync + 'static>;

/// A collection of error messages relating to a single file
#[derive(Debug, Default, MietteDiagnostic, thiserror::Error)]
#[error("{} errors", .errors.len())]
pub struct Diagnostic {
    #[related]
    pub errors: Vec<MietteReportable>,
}

/// A collection of error messages from various source files.
#[derive(Default)]
pub struct Diagnostics {
    pub inner: HashMap<Option<PathRef>, Diagnostic>,
}

impl Diagnostics {
    pub fn add_error<E>(&mut self, pref: Option<PathRef>, err: E)
    where
        E: MietteDiagnostic + Send + Sync + 'static,
    {
        self.inner.entry(pref).or_default().errors.push(Box::new(err));
    }
}
