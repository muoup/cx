pub mod db;
pub mod directories;
pub mod internal_storage;
pub mod jobs;

use crate::db::ModuleData;
use crate::directories::file_path;
use speedy::{Context, Readable, Writable};
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{LazyLock, Mutex};

/*
 *  Returns a unique identifier for each time this program is compiled.
 */
pub fn compilation_hash() -> u64 {
    struct PlaceHolder;

    // TODO: Find a more defined way to generate a unique hash for each time the compiler is compiled.

    static LAZY_STATIC: LazyLock<u64> = LazyLock::new(|| {
        let type_id = std::any::TypeId::of::<PlaceHolder>();
        let mut hasher = DefaultHasher::new();
        type_id.hash(&mut hasher);
        hasher.finish()
    });

    *LAZY_STATIC
}

#[derive(Debug)]
pub struct GlobalCompilationContext {
    pub config: CompilerConfig,
    pub module_db: ModuleData,

    pub linking_files: Mutex<HashSet<PathBuf>>,
}

impl Drop for GlobalCompilationContext {
    fn drop(&mut self) {
        self.module_db.store_data(self);
    }
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub output: PathBuf,
}

#[derive(Default, Debug, Copy, Clone, Hash)]
pub enum OptimizationLevel {
    #[default]
    O0,
    O1,
    O2,
    O3,
    Osize,
    Ofast,
}

#[derive(Debug, Clone, Copy, Hash)]
pub enum CompilerBackend {
    Cranelift,
    LLVM,
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct CompilationUnit {
    identifier: Rc<String>,
    path: Rc<Path>,
}

impl<'a, C: Context> Readable<'a, C> for CompilationUnit {
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let identifier: String = String::read_from(reader)?;
        let path: PathBuf = PathBuf::from(String::read_from(reader)?);

        Ok(Self {
            identifier: identifier.into(),
            path: path.into_boxed_path().into(),
        })
    }
}

impl<C: Context> Writable<C> for CompilationUnit {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + speedy::Writer<C>,
    {
        self.identifier.as_str().write_to(writer)?;
        self.path.to_str().unwrap().write_to(writer)?;
        Ok(())
    }
}

impl Hash for CompilationUnit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.path.to_str().unwrap())
    }
}

impl CompilationUnit {
    pub fn from_str(path: &str) -> Self {
        let path = if path.ends_with(".cx") {
            &path[..path.len() - 3]
        } else {
            path
        };

        let path_buf = PathBuf::from(file_path(path)).with_extension("cx");

        Self {
            identifier: Rc::new(path.to_string()),
            path: path_buf.into_boxed_path().into(),
        }
    }

    pub fn identifier(&self) -> &str {
        self.identifier.as_str()
    }

    pub fn to_string(&self) -> String {
        self.path.to_str().unwrap().to_string()
    }

    pub fn as_str(&self) -> &str {
        self.path.to_str().unwrap()
    }

    pub fn as_path(&self) -> &Path {
        &self.path
    }

    pub fn with_extension(&self, extension: &str) -> PathBuf {
        self.path.with_extension(extension)
    }

    pub fn is_std_lib(&self) -> bool {
        self.identifier.starts_with("std/")
    }
}
