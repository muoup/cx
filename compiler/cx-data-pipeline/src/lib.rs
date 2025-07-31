pub mod jobs;
pub mod db;
pub mod directories;
pub mod internal_storage;

use std::collections::HashSet;
use std::fmt::Display;
use crate::db::ModuleData;
use std::path::{Path, PathBuf};
use std::rc::Rc;
use std::sync::Mutex;
use serde::{Deserialize, Serialize};
use crate::directories::file_path;

#[derive(Debug, Clone, PartialEq, Eq, Hash, Ord, PartialOrd)]
pub struct CompilationUnit {
    data: PathBuf
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

impl Display for CompilationUnit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.data.to_str().unwrap())
    }
}

impl CompilationUnit {
    pub fn from_str(path: &str) -> Self {
        Self::from_path(PathBuf::from(file_path(path)))
    }
    
    pub fn from_path(path: PathBuf) -> Self {
        CompilationUnit { 
            data: path.with_extension("") 
        }
    }
    
    pub fn to_string(&self) -> String {
        self.data.to_str().unwrap().to_string()
    }
    
    pub fn as_str(&self) -> &str {
        self.data.to_str().unwrap()
    }
    
    pub fn to_path(&self) -> &Path {
        &self.data
    }
    
    pub fn with_extension(&self, extension: &str) -> Self {
        let mut new_path = self.data.clone();
        new_path.set_extension(extension);
        CompilationUnit { data: new_path }
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