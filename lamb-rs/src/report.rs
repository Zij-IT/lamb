use std::{ops::Range, path::Path};

use ariadne::*;
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

pub fn report_errors<P, T, M>(src: P, errs: Vec<Rich<T>>, msg: M)
where
    P: AsRef<Path>,
    T: std::fmt::Debug,
    M: ToString,
{
    let mut builder =
        Report::build(ReportKind::Error, src.as_ref().to_owned(), 0).with_message(msg);

    for err in errs {
        attach_err(&mut builder, err, src.as_ref());
    }

    builder.finish().eprint(FileCache::default()).unwrap()
}

fn attach_err<'a, T: std::fmt::Debug>(
    report: &mut ReportBuilder<SrcSpan<'a>>,
    err: Rich<T>,
    path: &'a Path,
) {
    attach_reason(
        report,
        SrcSpan::from_simple(err.span().clone(), path),
        err.reason(),
    );
}

fn attach_reason<'a, T: std::fmt::Debug>(
    report: &mut ReportBuilder<SrcSpan<'a>>,
    range: SrcSpan<'a>,
    reason: &RichReason<T>,
) {
    match reason {
        RichReason::ExpectedFound { found, .. } => {
            report.add_label(Label::new(range).with_message(format!("Found {found:?}.")))
        }
        RichReason::Custom(s) => report.add_label(Label::new(range).with_message(s)),
        RichReason::Many(reasons) => {
            for reason in reasons {
                attach_reason(report, range.clone(), reason);
            }
        }
    }
}
