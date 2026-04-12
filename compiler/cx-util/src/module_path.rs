use speedy::{Readable, Writable};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord, Readable, Writable)]
pub struct ModulePath(String);

pub fn stdlib_directory(inner_path: &str) -> String {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    format!("{manifest_dir}/../../lib/{inner_path}")
}

impl ModulePath {
    pub fn new(path: impl Into<String>) -> Self {
        Self(path.into())
    }

    pub fn from_import_path(path: &str) -> Self {
        Self(path.replace("::", "/"))
    }

    pub fn from_source_path(path: &str) -> Self {
        let stripped = path
            .strip_suffix(".cxl")
            .or_else(|| path.strip_suffix(".cx"))
            .unwrap_or(path);
        Self(stripped.replace('\\', "/"))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn is_std(&self) -> bool {
        self.0 == "std" || self.0.starts_with("std/")
    }

    pub fn is_library_module(&self) -> bool {
        self.is_std()
    }

    pub fn resolve(&self, working_directory: &Path) -> PathBuf {
        if self.is_std() {
            let suffix = self.0.strip_prefix("std/").unwrap_or("");
            let inner = if suffix.is_empty() {
                "std.cx".to_string()
            } else {
                format!("std/{suffix}.cx")
            };
            PathBuf::from(stdlib_directory(&inner))
        } else {
            let path = Path::new(self.as_str());
            if path.is_absolute() {
                path.to_path_buf()
            } else {
                working_directory.join(path).with_extension("cx")
            }
        }
    }

    pub fn with_extension(&self, working_directory: &Path, extension: &str) -> PathBuf {
        self.resolve(working_directory).with_extension(extension)
    }
}

impl AsRef<str> for ModulePath {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
