use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

use speedy::{Context, Readable, Reader, Writable, Writer};

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

    pub fn strip(&self, prefix: &Self) -> Option<Self> {
        if self.0.len() < prefix.0.len() {
            return None;
        }

        if self.0[..prefix.0.len()] != prefix.0[..] {
            return None;
        }

        Some(Self(self.0[prefix.0.len()..].to_vec()))
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

    pub fn as_slash_string(&self) -> String {
        self.0
            .iter()
            .map(CXIdent::as_str)
            .collect::<Vec<_>>()
            .join("/")
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

#[derive(Debug, Default, Clone, Hash, PartialEq, Eq)]
pub struct EnvironmentNamespace(Arc<NamespacePath>);

impl EnvironmentNamespace {
    pub fn root() -> Self {
        Self::from(NamespacePath::root())
    }

    pub fn new(namespace: NamespacePath) -> Self {
        Self(Arc::new(namespace))
    }

    pub fn from_slash_path(path: &str) -> Self {
        Self::from(NamespacePath::from_slash_path(path))
    }

    pub fn from_scoped_path(path: &str) -> Self {
        Self::from(NamespacePath::from_scoped_path(path))
    }

    pub fn as_namespace_path(&self) -> &NamespacePath {
        self.0.as_ref()
    }

    pub fn identifier(&self) -> String {
        self.as_namespace_path().as_slash_string()
    }
}

impl Deref for EnvironmentNamespace {
    type Target = NamespacePath;

    fn deref(&self) -> &Self::Target {
        self.as_namespace_path()
    }
}

impl From<NamespacePath> for EnvironmentNamespace {
    fn from(value: NamespacePath) -> Self {
        Self::new(value)
    }
}

impl From<EnvironmentNamespace> for NamespacePath {
    fn from(value: EnvironmentNamespace) -> Self {
        value.as_namespace_path().clone()
    }
}

impl From<&EnvironmentNamespace> for NamespacePath {
    fn from(value: &EnvironmentNamespace) -> Self {
        value.as_namespace_path().clone()
    }
}

impl From<ModulePath> for EnvironmentNamespace {
    fn from(value: ModulePath) -> Self {
        Self::from(NamespacePath::from(value))
    }
}

impl From<&NamespacePath> for EnvironmentNamespace {
    fn from(value: &NamespacePath) -> Self {
        Self::from(value.clone())
    }
}

impl From<&EnvironmentNamespace> for EnvironmentNamespace {
    fn from(value: &EnvironmentNamespace) -> Self {
        value.clone()
    }
}

impl Display for EnvironmentNamespace {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(self.as_namespace_path(), f)
    }
}

impl<'a, C: Context> Readable<'a, C> for EnvironmentNamespace {
    fn read_from<R: Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        Ok(Self::from(NamespacePath::read_from(reader)?))
    }
}

impl<C: Context> Writable<C> for EnvironmentNamespace {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + Writer<C>,
    {
        self.as_namespace_path().write_to(writer)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct QualifiedName {
    pub namespace: NamespacePath,
    pub name: CXIdent,
}

impl QualifiedName {
    pub fn new(namespace: impl Into<NamespacePath>, name: CXIdent) -> Self {
        Self {
            namespace: namespace.into(),
            name,
        }
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

    pub fn root_name(self) -> Option<CXIdent> {
        if !self.namespace.is_root() {
            return None;
        }

        Some(self.name)
    }

    pub fn root_name_ref(&self) -> Option<&CXIdent> {
        if !self.namespace.is_root() {
            return None;
        }

        Some(&self.name)
    }

    pub fn child(self, name: CXIdent) -> Self {
        Self {
            namespace: self.namespace.child(self.name),
            name,
        }
    }
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_flat_name())
    }
}
