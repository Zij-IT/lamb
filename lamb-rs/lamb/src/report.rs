use std::{ops::Range, path::Path};

use ariadne::{Label, Report, ReportKind, Source};
use lamb_parse::SyntaxError;

pub fn errors<'a, S, P, M>(src: S, path: P, errs: &[SyntaxError], msg: M)
where
    S: AsRef<str>,
    P: Into<Option<&'a Path>>,
    M: ToString,
{
    let path = path.into().unwrap_or(Path::new("repl"));

    errs.iter()
        .fold(
            Report::build(ReportKind::Error, path, 0).with_message(msg),
            |mut report, err| {
                let span = SrcSpan::from_span(err.raw_span(), path);
                for reason in err.reasons() {
                    report.add_label(Label::new(span.clone()).with_message(reason));
                }

                report
            },
        )
        .finish()
        .eprint(SrcCache::new(src.as_ref()))
        .unwrap();
}

#[derive(Clone)]
struct SrcSpan<'a> {
    path: &'a Path,
    span: Range<usize>,
}

impl<'a> SrcSpan<'a> {
    fn from_span(span: Range<usize>, path: &'a Path) -> Self {
        Self { path, span }
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

struct SrcCache {
    src: Source,
}

impl SrcCache {
    fn new(src: &str) -> Self {
        Self {
            src: Source::from(src),
        }
    }
}

impl ariadne::Cache<Path> for SrcCache {
    fn fetch(&mut self, _id: &Path) -> Result<&Source, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.src)
    }

    fn display<'a>(&self, id: &'a Path) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id.display()))
    }
}
