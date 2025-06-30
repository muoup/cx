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
        format!("{}/../../../lib/", path)
    } else {
        format!("{}/../../lib/", path)
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
        format!("{}.cx", path)
    }
}