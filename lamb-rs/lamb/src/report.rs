use std::{ops::Range, path::Path};

use ariadne::{FileCache, Label, Report, ReportBuilder, ReportKind};
use chumsky::{error::RichReason, prelude::Rich, span::SimpleSpan};

#[derive(Clone)]
struct SrcSpan<'a> {
    path: &'a Path,
    span: Range<usize>,
}

impl<'a> SrcSpan<'a> {
    fn from_simple(span: SimpleSpan, path: &'a Path) -> Self {
        Self {
            span: span.into_range(),
            path,
        }
    }
}

impl<'a> ariadne::Span for SrcSpan<'a> {
    type SourceId = Path;

    fn source(&self) -> &Self::SourceId {
        self.path
    }

    fn start(&self) -> usize {
        self.span.start
    }

    fn end(&self) -> usize {
        self.span.end
    }
}

pub fn errors<'a, S, T, M>(path: S, errs: &[Rich<T>], msg: M)
where
    S: Into<Option<&'a Path>>,
    T: std::fmt::Debug,
    M: ToString,
{
    let path = path.into().unwrap_or(Path::new("repl"));

    errs.iter()
        .fold(
            Report::build(ReportKind::Error, path, 0).with_message(msg),
            |report, err| attach_err(report, err, path),
        )
        .finish()
        .eprint(FileCache::default())
        .unwrap();
}

fn attach_err<'p, 'r, T: std::fmt::Debug>(
    mut report: ReportBuilder<'r, SrcSpan<'p>>,
    err: &Rich<T>,
    path: &'p Path,
) -> ReportBuilder<'r, SrcSpan<'p>> {
    attach_reason(
        &mut report,
        SrcSpan::from_simple(*err.span(), path),
        err.reason(),
    );

    report
}

fn attach_reason<'a, T: std::fmt::Debug>(
    report: &mut ReportBuilder<'_, SrcSpan<'a>>,
    range: SrcSpan<'a>,
    reason: &RichReason<T>,
) {
    match reason {
        RichReason::ExpectedFound { found, .. } => {
            report.add_label(Label::new(range).with_message(format!("Found {found:?}.")));
        }
        RichReason::Custom(s) => report.add_label(Label::new(range).with_message(s)),
        RichReason::Many(reasons) => {
            for reason in reasons {
                attach_reason(report, range.clone(), reason);
            }
        }
    }
}
