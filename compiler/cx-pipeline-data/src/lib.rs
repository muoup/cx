pub mod config;
pub mod db;
pub mod internal_storage;
pub mod jobs;

use crate::config::{CXProjectConfig, LinkEntry};
use crate::db::ModuleData;
use cx_namespace::module::{ModulePath, NamespacePath};
pub use cx_target::ArchitectureConfig;
use cx_util::identifier::CXIdent;
use speedy::{Context, Readable, Writable};
use std::collections::HashSet;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
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
    pub project_config: Option<CXProjectConfig>,

    pub output: PathBuf,
    pub working_directory: PathBuf,
    pub internal_directory: PathBuf,

    pub link_entries: Vec<LinkEntry>,
    pub native_objects: Vec<PathBuf>,
    pub include_dirs: Vec<PathBuf>,
    pub predefined_macros: Vec<(String, String)>,

    pub unsafe_mode: bool,
    pub verbose: bool,
    pub dump: bool,
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
    module: ModulePath,
    namespace: NamespacePath,
}

impl CompilationUnit {
    pub fn new(
        project_root: &Path,
        module_path: ModulePath,
        namespace_override: Option<NamespacePath>,
    ) -> Self {
        let module_path = if module_path.as_path().is_absolute() {
            module_path
        } else {
            ModulePath::new(project_root.join(module_path.as_path()))
        };
        let diff = module_path
            .as_path()
            .strip_prefix(project_root)
            .unwrap_or(module_path.as_path())
            .with_extension("");

        let namespace = namespace_override.unwrap_or_else(|| {
            NamespacePath::new(
                diff.components()
                    .map(|c| CXIdent::from(c.as_os_str().to_string_lossy().to_string()))
                    .collect(),
            )
        });

        CompilationUnit {
            module: module_path,
            namespace,
        }
    }

    pub fn module(&self) -> &ModulePath {
        &self.module
    }

    pub fn namespace(&self) -> &NamespacePath {
        &self.namespace
    }

    pub fn is_std_lib(&self) -> bool {
        self.namespace
            .segments()
            .get(0)
            .map(|s| s.as_str() == "std")
            .unwrap_or(false)
    }
}

impl<'a, C: Context> Readable<'a, C> for CompilationUnit {
    fn read_from<R: speedy::Reader<'a, C>>(reader: &mut R) -> Result<Self, C::Error> {
        let module = PathBuf::from(String::read_from(reader)?);
        let namespace = NamespacePath::read_from(reader)?;
        Ok(Self {
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
        self.module.hash(state);
        self.namespace.hash(state);
    }
}

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Unit {}", self.namespace())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use speedy::{Readable, Writable};

    #[test]
    fn compilation_unit_round_trip_preserves_module_and_namespace() {
        let module = ModulePath::new(PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("Cargo.toml"));
        let namespace = NamespacePath::from_str("custom::logical::module");
        let unit = CompilationUnit::new(
            Path::new(env!("CARGO_MANIFEST_DIR")),
            module,
            Some(namespace),
        );
        let encoded = unit.write_to_vec().unwrap();
        let decoded = CompilationUnit::read_from_buffer(&encoded).unwrap();

        assert_eq!(decoded, unit);
    }
}
