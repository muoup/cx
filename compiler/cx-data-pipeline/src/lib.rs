pub mod jobs;
pub mod db;
pub mod directories;
pub mod internal_storage;

use std::collections::HashSet;
use std::fmt::Display;
use std::hash::{DefaultHasher, Hash, Hasher};
use crate::db::ModuleData;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::{LazyLock, Mutex};
use serde::{Deserialize, Serialize};
use crate::directories::file_path;

/*
 *  Returns a unique identifier for each time this program is compiled.
 */
pub fn compilation_hash() -> u64 {
    struct PlaceHolder;
    
    static lazy_static: LazyLock<u64> = LazyLock::new(|| {
        let type_id = std::any::TypeId::of::<PlaceHolder>();
        let mut hasher = DefaultHasher::new();
        type_id.hash(&mut hasher);
        hasher.finish()
    });
    
    *lazy_static
}

#[derive(Debug, Clone, PartialEq, Eq, Ord, PartialOrd)]
pub struct CompilationUnit {
    identifier: Rc<String>,
    path: Rc<Path>
}

impl Serialize for CompilationUnit {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(self.as_str())
    }
}

impl<'de> Deserialize<'de> for CompilationUnit {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let data = String::deserialize(deserializer)?;
        
        Ok(CompilationUnit::from_str(&data))
    }
}

impl Hash for CompilationUnit {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
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
        
        let path_buf = PathBuf::from(file_path(path)).with_extension("");
        
        Self {
            identifier: Rc::new(path.to_string()),
            path: path_buf.into_boxed_path().into()
        }
    }

    pub fn to_string(&self) -> String {
        self.path.to_str().unwrap().to_string()
    }
    
    pub fn as_str(&self) -> &str {
        self.path.to_str().unwrap()
    }
    
    pub fn to_path(&self) -> &Path {
        &self.path
    }
    
    pub fn with_extension(&self, extension: &str) -> PathBuf {
        self.path.with_extension(extension)
    }
}

#[derive(Debug)]
pub struct GlobalCompilationContext {
    pub config: CompilerConfig,
    pub module_db: ModuleData,
    
    pub linking_files: Mutex<HashSet<PathBuf>>
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

#[cfg(feature = "backend-llvm")]
#[derive(Default, Debug, Clone, Copy, Hash)]
pub enum CompilerBackend {
    #[default]
    LLVM,
    Cranelift
}

#[cfg(not(feature = "backend-llvm"))]
#[derive(Default, Debug, Clone, Copy, Hash)]
pub enum CompilerBackend {
    #[default]
    Cranelift,
    LLVM
}

pub fn libary_path_prefix() -> String {
    let path = std::env::current_exe()
        .expect("Failed to get current executable path")
        .parent()
        .expect("Failed to get parent directory of executable")
        .to_str()
        .expect("Failed to convert path to string")
        .to_string();

    if cfg!(feature = "test") {
        format!("{path}/../../../lib/")
    } else {
        format!("{path}/../../lib/")
    }
}

pub fn cx_path_str(path: &str) -> String {
    if path.starts_with("std") {
        let current_exe = std::env::current_exe()
            .expect("Failed to get current executable path");

        if cfg!(feature = "test") {
            format!("{}/../../../lib/{}.cx", current_exe.parent().unwrap().display(), &path)
        } else {
            format!("{}/../../lib/{}.cx", current_exe.parent().unwrap().display(), &path)
        }
    } else {
        format!("{path}.cx")
    }
}