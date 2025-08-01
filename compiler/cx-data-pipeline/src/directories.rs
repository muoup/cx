use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};
use crate::{compilation_hash, CompilationUnit, GlobalCompilationContext};

pub fn file_path(path: &str) -> String {
    if path.starts_with("std") {
        let current_exe = std::env::current_exe()
            .expect("Failed to get current executable path");

        if cfg!(feature = "test") {
            format!("{}/../../../lib/{}", current_exe.parent().unwrap().display(), &path)
        } else {
            format!("{}/../../lib/{}", current_exe.parent().unwrap().display(), &path)
        }
    } else {
        format!("{path}")
    }
}

pub fn internal_directory(context: &GlobalCompilationContext, unit: &CompilationUnit) -> PathBuf {
    let mut profile_hash = DefaultHasher::new();
    context.config.backend.hash(&mut profile_hash);
    context.config.optimization_level.hash(&mut profile_hash);
    compilation_hash().hash(&mut profile_hash);
    let profile_hash = profile_hash.finish().to_string();

    let mut complete_path = PathBuf::from(".internal");
    complete_path.push(profile_hash);
    
    let mut identifier_string = unit.identifier.to_string();
    identifier_string.push_str(".cx");
    complete_path.push(identifier_string);

    std::fs::create_dir_all(&complete_path)
        .expect(format!("Failed to create internal directory: {}", complete_path.display()).as_str());

    complete_path
}