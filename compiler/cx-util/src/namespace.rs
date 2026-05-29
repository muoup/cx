use std::fmt::{Display, Formatter};

use speedy::{Readable, Writable};

use crate::{identifier::CXIdent, module_path::ModulePath};

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct NamespacePath(Vec<CXIdent>);

impl From<ModulePath> for NamespacePath {
    fn from(path: ModulePath) -> Self {
        Self::from_slash_path(path.as_str())
    }
}

impl NamespacePath {
    pub fn root() -> Self {
        Self::default()
    }

    pub fn new(segments: Vec<CXIdent>) -> Self {
        Self(segments)
    }

    pub fn from_slash_path(path: &str) -> Self {
        Self(
            path.split('/')
                .filter(|segment| !segment.is_empty())
                .map(CXIdent::new)
                .collect(),
        )
    }

    pub fn from_scoped_path(path: &str) -> Self {
        Self(
            path.split("::")
                .filter(|segment| !segment.is_empty())
                .map(CXIdent::new)
                .collect(),
        )
    }

    pub fn segments(&self) -> &[CXIdent] {
        &self.0
    }

    pub fn is_root(&self) -> bool {
        self.0.is_empty()
    }

    pub fn child(&self, name: CXIdent) -> Self {
        let mut segments = self.0.clone();
        segments.push(name);
        Self(segments)
    }

    pub fn join(&self, other: &Self) -> Self {
        let mut segments = self.0.clone();
        segments.extend_from_slice(&other.0);
        Self(segments)
    }

    pub fn parent_and_name(&self) -> Option<(Self, CXIdent)> {
        let (name, parent) = self.0.split_last()?;
        Some((Self(parent.to_vec()), name.clone()))
    }

    pub fn as_scope_string(&self) -> String {
        self.0
            .iter()
            .map(CXIdent::as_str)
            .collect::<Vec<_>>()
            .join("::")
    }

    pub fn as_flat_name_with(&self, name: &CXIdent) -> String {
        if self.is_root() {
            name.as_string()
        } else {
            format!("{}::{}", self.as_scope_string(), name)
        }
    }
}

impl Display for NamespacePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_scope_string())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct QualifiedName {
    pub namespace: NamespacePath,
    pub name: CXIdent,
}

impl QualifiedName {
    pub fn new(namespace: NamespacePath, name: CXIdent) -> Self {
        Self { namespace, name }
    }

    pub fn new_raw(name: CXIdent) -> Self {
        Self {
            namespace: NamespacePath::root(),
            name,
        }
    }

    pub fn root(name: CXIdent) -> Self {
        Self {
            namespace: NamespacePath::root(),
            name,
        }
    }

    pub fn as_flat_name(&self) -> String {
        self.namespace.as_flat_name_with(&self.name)
    }

    pub fn raw_name(self) -> Option<CXIdent> {
        if !self.namespace.is_root() {
            return None;
        }

        Some(self.name)
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_flat_name())
    }
}
