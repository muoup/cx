use cx_util::namespace::{NamespacePath, QualifiedName};

use crate::result::QualifiedLookupResult;

pub mod result;

pub trait MIRQualifiedLookup {
    type Output;

    fn lookup_local(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output>;

    fn lookup_exact(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> Option<Self::Output>;

    fn resolve_aliases(
        &self,
        lexical_namespace: &NamespacePath,
        namespace: &NamespacePath,
    ) -> Vec<NamespacePath>;

    fn qualified_lookup(
        &self,
        lexical_namespace: &NamespacePath,
        name: &QualifiedName,
    ) -> QualifiedLookupResult<Self::Output> {
        if let Some(value) = self.lookup_local(lexical_namespace, name) {
            return QualifiedLookupResult::Found {
                resolved_name: name.clone(),
                value,
            };
        }
        
        if let Some(value) = self.lookup_exact(lexical_namespace, name) {
            return QualifiedLookupResult::Found {
                resolved_name: name.clone(),
                value,
            };
        }

        if name.namespace.is_root() && !lexical_namespace.is_root() {
            let lexical_name = QualifiedName {
                namespace: lexical_namespace.clone(),
                name: name.name.clone(),
            };

            if let Some(value) = self.lookup_exact(lexical_namespace, &lexical_name) {
                return QualifiedLookupResult::Found {
                    resolved_name: lexical_name,
                    value,
                };
            }
        }

        let mut resolved = self.resolve_aliases(lexical_namespace, &name.namespace)
            .into_iter()
            .filter_map(|candidate_namespace| {
                let candidate_name = QualifiedName {
                    namespace: candidate_namespace,
                    name: name.name.clone(),
                };

                self.lookup_exact(lexical_namespace, &candidate_name)
                    .map(|value| (candidate_name, value))
            })
            .collect::<Vec<_>>();

        match resolved.len() {
            0 => QualifiedLookupResult::NotFound,
            1 => {
                let (resolved_name, value) = resolved.pop().expect("length checked above");
                QualifiedLookupResult::Found {
                    resolved_name,
                    value,
                }
            }
            _ => {
                let first_candidate = &resolved[0];

                for candidate in &resolved[1..] {
                    if candidate.0 != first_candidate.0 {
                        return QualifiedLookupResult::Ambiguous {
                            candidates: resolved.into_iter().map(|(name, _)| name).collect(),
                        };
                    }
                }
                
                resolved
                    .into_iter()
                    .map(|(name, value)| QualifiedLookupResult::Found { resolved_name: name, value })
                    .next()
                    .expect("length checked above")
            },
        }
    }
}