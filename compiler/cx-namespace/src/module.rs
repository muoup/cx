use std::{fmt::Display, path::{Path, PathBuf}};

use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct ModulePath(PathBuf);

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct NamespacePath {
    segments: Vec<CXIdent>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct QualifiedName {
    pub namespace: NamespacePath,
    pub name: CXIdent,
}

impl AsRef<Path> for ModulePath {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

impl ModulePath {
    pub fn new(path: PathBuf) -> Self {
        Self(path)
    }

    pub fn from_source_path(path: &str) -> Self {
        let path = PathBuf::from(path);
        let path = path.with_extension("");
        Self(path)
    }

    pub fn as_path(&self) -> &PathBuf {
        &self.0
    }

    pub fn with_extension(mut self, extension: &str) -> Self {
        self.0.set_extension(extension);
        self
    }
}

impl NamespacePath {
    pub fn new(segments: Vec<CXIdent>) -> Self {
        Self { segments }
    }

    pub fn root() -> Self {
        Self { segments: Vec::new() }
    }

    pub fn is_root(&self) -> bool {
        self.segments.is_empty()
    }

    pub fn child(mut self, name: CXIdent) -> Self {
        self.segments.push(name);
        self
    }

    pub fn parent(self) -> Option<Self> {
        if self.segments.is_empty() {
            return None;
        }

        let mut segments = self.segments;
        segments.pop();
        Some(Self { segments })
    }

    pub fn segments(&self) -> &[CXIdent] {
        &self.segments
    }

    pub fn parent_and_name(self) -> Option<(Self, CXIdent)> {
        if self.segments.is_empty() {
            return None;
        }

        let mut segments = self.segments;
        let name = segments.pop().unwrap();
        Some((Self { segments }, name))
    }

    pub fn from_str(s: &str) -> Self {
        let segments = s
            .split("::")
            .map(|s| CXIdent::new(s))
            .collect();
        Self { segments }
    }
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

impl Display for NamespacePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, segment) in self.segments.iter().enumerate() {
            if i > 0 {
                write!(f, "::")?;
            }
            write!(f, "{}", segment)?;
        }
        Ok(())
    }    
}

impl Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.namespace.is_root() {
            write!(f, "{}::", self.namespace)?;
        }
        write!(f, "{}", self.name)
    }
}