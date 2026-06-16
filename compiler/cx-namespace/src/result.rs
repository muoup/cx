use cx_util::namespace::QualifiedName;

#[derive(Debug, Clone)]
pub enum QualifiedLookupResult<Output> {
    NotFound,
    Found {
        resolved_name: QualifiedName,
        value: Output,
    },
    Ambiguous {
        candidates: Vec<QualifiedName>,
    },
}