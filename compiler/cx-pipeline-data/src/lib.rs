pub mod config;
pub mod db;
pub mod hmir;
pub mod internal_storage;
pub mod jobs;
pub mod package;

use crate::config::LinkEntry;
use crate::db::ModuleData;
use crate::package::{PackageId, Packages};
use cx_namespace::module::{ModulePath, NamespacePath};
pub use cx_target::ArchitectureConfig;

use speedy::{Context, Readable, Writable};
use rustc_hash::FxHashSet;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::PathBuf;
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

    pub linking_files: Mutex<BTreeSet<PathBuf>>,
    pub library_symbols: Mutex<BTreeMap<PathBuf, Vec<String>>>,
    pub headers: Mutex<BTreeMap<PathBuf, String>>,
}

impl GlobalCompilationContext {
    pub fn new(config: CompilerConfig) -> Self {
        Self {
            config,
            module_db: ModuleData::new(),
            linking_files: Mutex::new(BTreeSet::new()),
            library_symbols: Mutex::new(BTreeMap::new()),
            headers: Mutex::new(BTreeMap::new()),
        }
    }
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
    pub require_explicit_return: Option<bool>,

    pub output: PathBuf,
    pub internal_directory: PathBuf,
    pub packages: Packages,

    pub link_entries: Vec<LinkEntry>,
    pub native_objects: Vec<PathBuf>,
    pub header_modules: FxHashSet<PathBuf>,

    pub unsafe_flags: UnsafeFlags,
    pub verbose: bool,
    pub dump: bool,
    pub module_mode: bool,
    pub record_types: bool,
}

#[derive(Debug, Default, Clone, Copy)]
pub struct UnsafeFlags {
    pub no_assert: bool,
    pub no_analysis: bool,
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CompilerBackend {
    Cranelift,
    LLVM,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompilationUnit {
    package: PackageId,
    module: ModulePath,
    namespace: NamespacePath,
}

impl CompilationUnit {
    pub fn new(package: PackageId, module: ModulePath, namespace: NamespacePath) -> Self {
        Self { package, module, namespace }
    }

    pub fn package(&self) -> PackageId {
        self.package
    }

    pub fn module(&self) -> &ModulePath {
        &self.module
    }

    pub fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub fn is_std_lib(&self) -> bool {
        self.package == PackageId::STD
    }
}

impl<'a, C: Context> Readable<'a, C> for CompilationUnit {
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let package = PackageId::read_from(reader)?;
        let module = PathBuf::from(String::read_from(reader)?);
        let namespace = NamespacePath::read_from(reader)?;
        Ok(Self {
            package,
            module: ModulePath::new(module),
            namespace,
        })
    }
}

impl<C: Context> Writable<C> for CompilationUnit {
    fn write_to<W>(&self, writer: &mut W) -> Result<(), C::Error>
    where
        W: ?Sized + speedy::Writer<C>,
    {
        self.package.write_to(writer)?;
        self.module
            .as_path()
            .to_str()
            .ok_or_else(|| speedy::Error::custom("Module path is not valid UTF-8"))?
            .write_to(writer)?;
        self.namespace.write_to(writer)
    }
}

impl Hash for CompilationUnit {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.package.hash(state);
        self.module.hash(state);
        self.namespace.hash(state);
    }
}

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if !self.namespace.is_root() {
            return write!(f, "{}", self.namespace);
        }
        let path = self.module.as_path();
        write!(f, "{}", path.file_name().unwrap_or(path.as_os_str()).to_string_lossy())
    }
}