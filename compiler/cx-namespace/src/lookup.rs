use rustc_hash::{FxHashMap, FxHashSet};

use crate::module::{NamespacePath, QualifiedName};

pub fn candidates(
    source_namespace: &NamespacePath,
    name: &QualifiedName,
    aliases: &FxHashMap<NamespacePath, Vec<NamespacePath>>,
) -> Vec<QualifiedName> {
    let mut names = vec![name.clone()];
    if name.namespace.is_root() {
        names.push(QualifiedName::new(
            source_namespace.clone(),
            name.name.clone(),
        ));
    }
    let segments = name.namespace.segments();
    for length in (0..=segments.len()).rev() {
        let prefix = NamespacePath::new(segments[..length].to_vec());
        let suffix = NamespacePath::new(segments[length..].to_vec());
        for namespace in aliases.get(&prefix).into_iter().flatten() {
            let namespace = namespace.clone().join(suffix.clone());
            names.push(QualifiedName::new(namespace, name.name.clone()));
        }
    }

    let mut seen = FxHashSet::default();
    names.retain(|name| seen.insert(name.clone()));
    names
}
