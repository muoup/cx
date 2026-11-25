use std::path::{Path, PathBuf};

use rustc_hash::FxHashSet;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IncludeSource {
    Directory(PathBuf),
    Files {
        base: PathBuf,
        files: FxHashSet<PathBuf>,
    },
    Generated {
        directory: PathBuf,
        library: String,
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
        let relative = self.relative(inner)?;
        match &self.source {
            IncludeSource::Directory(directory) | IncludeSource::Generated { directory, .. } => {
                Some(directory.join(relative)).filter(|path| path.is_file())
            }
            IncludeSource::Files { base, files } => {
                files.contains(Path::new(relative)).then(|| base.join(relative))
            }
        }
    }

    pub(crate) fn search_directory(&self) -> Option<&Path> {
        match (&self.source, &self.alias) {
            (IncludeSource::Directory(directory), None)
            | (IncludeSource::Generated { directory, .. }, None) => Some(directory),
            _ => None,
        }
    }

    pub(crate) fn unbuilt_library(&self, inner: &str) -> Option<&str> {
        match &self.source {
            IncludeSource::Generated { directory, library } if !directory.is_dir() => {
                self.relative(inner).map(|_| library.as_str())
            }
            _ => None,
        }
    }

    fn relative<'a>(&self, inner: &'a str) -> Option<&'a str> {
        match &self.alias {
            Some(alias) => inner.strip_prefix(alias.as_str())?.strip_prefix('/'),
            None => Some(inner),
        }
    }
}
