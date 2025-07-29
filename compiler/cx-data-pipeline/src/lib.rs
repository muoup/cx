pub mod jobs;
pub mod db;

use crate::db::ModuleData;
use std::path::PathBuf;
use std::rc::Rc;
use std::sync::Mutex;

pub type CompilationUnit = Rc<str>;

pub struct GlobalCompilationContext {
    pub config: CompilerConfig,
    pub module_db: Mutex<ModuleData>
}

pub struct CompilerConfig {
    pub backend: CompilerBackend,
    pub optimization_level: OptimizationLevel,
    pub output: PathBuf,
}

#[derive(Default, Debug, Copy, Clone)]
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
#[derive(Default, Debug, Clone, Copy)]
pub enum CompilerBackend {
    #[default]
    LLVM,
    Cranelift
}

#[cfg(not(feature = "backend-llvm"))]
#[derive(Default, Debug, Clone, Copy)]
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