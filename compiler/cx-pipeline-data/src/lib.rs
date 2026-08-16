pub mod config;
pub mod db;
pub mod directories;
pub mod internal_storage;
pub mod jobs;

use crate::config::{CXProjectConfig, LinkEntry};
use crate::db::ModuleData;
pub use cx_target::ArchitectureConfig;
use cx_util::module_path::ModulePath;
use cx_util::namespace::{EnvironmentNamespace, NamespacePath};
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
    pub module_mode: bool,
    pub module_db: ModuleData,

    pub linking_files: Mutex<HashSet<PathBuf>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CompilationMode {
    Executable,
    Object,
    Library,
}

#[derive(Debug, Clone)]
pub struct CompilerConfig {
    pub architecture: ArchitectureConfig,
    pub backend: CompilerBackend,
    pub compilation_mode: CompilationMode,
    pub optimization_level: OptimizationLevel,
    pub project_config: Option<CXProjectConfig>,

    pub output: PathBuf,
    pub working_directory: PathBuf,
    pub internal_directory: PathBuf,

    pub link_entries: Vec<LinkEntry>,
    pub native_objects: Vec<PathBuf>,
    pub include_dirs: Vec<PathBuf>,

    pub unsafe_mode: bool,
    pub verbose: bool,
    pub module_mode: bool,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationUnit {
    namespace: EnvironmentNamespace,
    path: Rc<Path>,
}

impl<'a, C: Context> Readable<'a, C> for CompilationUnit {
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let namespace = EnvironmentNamespace::read_from(reader)?;
        let path: PathBuf = PathBuf::from(String::read_from(reader)?);

        Ok(Self {
            namespace,
            path: path.into_boxed_path().into(),
        })
    }
}

impl<C: Context> Writable<C> for CompilationUnit {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + speedy::Writer<C>,
    {
        self.namespace.write_to(writer)?;
        self.path.to_str().unwrap().write_to(writer)?;
        Ok(())
    }
}

impl Hash for CompilationUnit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.namespace.hash(state);
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
        Self::from_rooted(path, Path::new("."))
    }

    pub fn from_rooted(path: &str, working_directory: &Path) -> Self {
        let module_path = ModulePath::from_source_path(path);
        let extension = if path.ends_with(".cxh") {
            "cxh"
        } else if path.ends_with(".c") {
            "c"
        } else {
            "cx"
        };
        Self::from_module_path_with_extension(module_path, working_directory, extension)
    }

    pub fn from_module_path(module_path: ModulePath, working_directory: &Path) -> Self {
        Self::from_module_path_with_extension(module_path, working_directory, "cx")
    }

    pub fn from_module_path_with_extension(
        module_path: ModulePath,
        working_directory: &Path,
        extension: &str,
    ) -> Self {
        let path_buf = module_path.with_extension(working_directory, extension);

        Self {
            namespace: EnvironmentNamespace::from(module_path),
            path: path_buf.into_boxed_path().into(),
        }
    }

    pub fn namespace(&self) -> &EnvironmentNamespace {
        &self.namespace
    }

    pub fn identifier(&self) -> String {
        self.namespace.identifier()
    }

    pub fn to_string(&self) -> String {
        self.path.to_str().unwrap().to_string()
    }

    pub fn to_namespace_path(&self) -> NamespacePath {
        self.namespace.as_namespace_path().clone()
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
        let identifier = self.identifier();
        identifier == "std" || identifier.starts_with("std/")
    }
}

impl From<&CompilationUnit> for EnvironmentNamespace {
    fn from(value: &CompilationUnit) -> Self {
        value.namespace.clone()
    }
}

impl From<CompilationUnit> for EnvironmentNamespace {
    fn from(value: CompilationUnit) -> Self {
        value.namespace
    }
}
