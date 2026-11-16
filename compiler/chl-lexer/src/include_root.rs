use std::path::{Path, PathBuf};

use rustc_hash::FxHashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeSource {
    Directory(PathBuf),
    Files {
        base: PathBuf,
        files: FxHashSet<PathBuf>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IncludeRoot {
    pub source: IncludeSource,
    pub alias: Option<String>,
}

impl IncludeRoot {
    pub fn directory(path: PathBuf, alias: Option<String>) -> Self {
        Self { source: IncludeSource::Directory(path), alias }
    }

    pub fn resolve(&self, inner: &str) -> Option<PathBuf> {
        let relative = match &self.alias {
            Some(alias) => inner.strip_prefix(alias.as_str())?.strip_prefix('/')?,
            None => inner,
        };
        match &self.source {
            IncludeSource::Directory(directory) => {
                Some(directory.join(relative)).filter(|path| path.is_file())
            }
            IncludeSource::Files { base, files } => {
                files.contains(Path::new(relative)).then(|| base.join(relative))
            }
        }
    }

    pub(crate) fn search_directory(&self) -> Option<&Path> {
        match (&self.source, &self.alias) {
            (IncludeSource::Directory(directory), None) => Some(directory),
            _ => None,
        }
    }
}
