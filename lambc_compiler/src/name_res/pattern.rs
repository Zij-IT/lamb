use std::{
    collections::{HashMap, HashSet},
    ops::Deref,
};

use lambc_parse::{Ident, IdentPattern, InnerPattern, Pattern, Span};

use super::{Error, Resolver};
use crate::PathRef;

// collect all idents from inner pattern
pub struct PatternChecker<'r, 's> {
    module: PathRef,
    resolver: &'r mut Resolver<'s>,
}

impl<'r, 's> PatternChecker<'r, 's> {
    pub fn new(module: PathRef, resolver: &'r mut Resolver<'s>) -> Self {
        Self { module, resolver }
    }

    pub fn check(&mut self, pattern: &Pattern<Ident>) {
        self.pattern_names(pattern);
    }

    fn pattern_names<'a>(
        &mut self,
        pattern: &'a Pattern<Ident>,
    ) -> HashMap<Wrapped<'a>, Vec<Span>> {
        // - collect all inner idents from inner pattern into sets
        let all_names: Vec<_> = pattern
            .inner
            .iter()
            .map(|pat| (self.inner_pattern_name(pat), pat.span()))
            .collect();

        // - union all idents into big set
        let mut unioned = HashMap::<_, Vec<_>>::new();

        let flat_map = all_names
            .clone()
            .into_iter()
            .flat_map(|(hmap, _)| hmap.into_iter());

        for (wrap, spans) in flat_map {
            unioned.entry(wrap).or_default().extend(spans);
        }

        // - issue error for each difference between individual sets and the unioned set
        let unkeys = unioned.keys().collect::<HashSet<_>>();
        for (names, pat_span) in all_names {
            let set: HashSet<_> = names.keys().collect();
            for Wrapped(id) in unkeys.difference(&set) {
                self.resolver.state.add_error(
                    Error::MissingBinding {
                        binding: id.raw.clone(),
                        pat_span,
                    },
                    Some(self.module),
                );
            }
        }

        // - return union set of all idents
        unioned
    }

    fn inner_pattern_name<'a>(
        &mut self,
        inner: &'a InnerPattern<Ident>,
    ) -> HashMap<Wrapped<'a>, Vec<Span>> {
        let map = match inner {
            InnerPattern::Array(arr) => {
                let mut map = HashMap::<_, Vec<_>>::new();
                let names = arr
                    .patterns
                    .iter()
                    .flat_map(|p| self.pattern_names(p).into_iter());

                for (name, spans) in names {
                    map.entry(name).or_default().extend(spans);
                }

                map
            }
            InnerPattern::Ident(i) => {
                let IdentPattern { ident, bound, span: _ } = i.deref();

                let mut map =
                    HashMap::from([(Wrapped(ident), vec![ident.span])]);

                if let Some(bound) = bound.as_deref() {
                    let set = self.inner_pattern_name(bound);
                    for (wrapped, spans) in set {
                        map.entry(wrapped).or_default().extend(spans);
                    }
                }

                map
            }
            InnerPattern::Rest(_) | InnerPattern::Literal(_) => {
                return HashMap::new()
            }
        };

        for (Wrapped(id), spans) in &map {
            let iter = spans.iter().map(|s| (id.raw.as_str(), *s));
            self.resolver.report_if_duplicates(self.module, iter);
        }

        map
    }
}

#[derive(Clone)]
struct Wrapped<'a>(&'a Ident);

impl<'a> PartialEq for Wrapped<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.0.raw == other.0.raw
    }
}

impl<'a> Eq for Wrapped<'a> {}

impl<'a> std::hash::Hash for Wrapped<'a> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.0.raw.hash(state);
    }
}
