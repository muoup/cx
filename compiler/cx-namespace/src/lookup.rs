use std::collections::HashSet;

use crate::module::{NamespacePath, QualifiedName};

#[cfg(test)]
mod tests;

pub trait QualifiedLookup {
    type Output;

    fn lookup_exact(&self, lexical_namespace: &NamespacePath, name: &QualifiedName) -> Option<Self::Output>;

    fn resolve_aliases(&self, lexical_namespace: &NamespacePath, namespace: &NamespacePath) -> Vec<NamespacePath>;

    fn priority(&self, lexical_namespace: &NamespacePath, name: &QualifiedName, _value: &Self::Output) -> (u8, bool) {
        (if &name.namespace == lexical_namespace { 2 } else { 0 }, false)
    }

    fn qualified_lookup(&self, lexical_namespace: &NamespacePath, name: &QualifiedName) -> QualifiedLookupResult<Self::Output> {
        let mut names = vec![name.clone()];
        if name.namespace.is_root() {
            names.push(QualifiedName::new(lexical_namespace.clone(), name.name.clone()));
        }
        for length in (0..=name.namespace.segments().len()).rev() {
            let prefix = NamespacePath::new(name.namespace.segments()[..length].to_vec());
            let suffix = NamespacePath::new(name.namespace.segments()[length..].to_vec());
            names.extend(self.resolve_aliases(lexical_namespace, &prefix).into_iter().map(|namespace| {
                QualifiedName::new(namespace.join(suffix.clone()), name.name.clone())
            }));
        }

        let mut seen = HashSet::new();
        let mut candidates = Vec::new();
        let mut priority = (0, false);
        for name in names {
            if !seen.insert(name.clone()) {
                continue;
            }
            let Some(value) = self.lookup_exact(lexical_namespace, &name) else {
                continue;
            };
            let rank = self.priority(lexical_namespace, &name, &value);
            if rank > priority {
                candidates.clear();
                priority = rank;
            }
            if rank == priority {
                candidates.push((name, value));
            }
        }
        match candidates.len() {
            0 => QualifiedLookupResult::NotFound,
            1 => {
                let (resolved_name, value) = candidates.pop().unwrap();
                QualifiedLookupResult::Found { resolved_name, value }
            }
            _ => {
                let mut candidates = candidates.into_iter().map(|(name, _)| name).collect::<Vec<_>>();
                candidates.sort_by_cached_key(ToString::to_string);
                QualifiedLookupResult::Ambiguous { candidates }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum QualifiedLookupResult<Output> {
    NotFound,
    Found { resolved_name: QualifiedName, value: Output },
    Ambiguous { candidates: Vec<QualifiedName> },
}
