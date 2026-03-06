use crate::{CompilationUnit, GlobalCompilationContext, compilation_hash};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Path, PathBuf};

pub fn stdlib_directory(inner_path: &str) -> String {   
    // {project_root}/compiler/cx-pipeline-data
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    
    format!("{manifest_dir}/../../lib/{inner_path}")
}

pub fn file_path(path: &str, working_directory: &Path) -> PathBuf {
    if path.starts_with("std") {
        PathBuf::from(stdlib_directory(&format!("{}/{}.cx", "std", &path[3..])))
    } else {
        let path = Path::new(path);

        if path.is_absolute() {
            path.to_path_buf()
        } else {
            working_directory.join(path)
        }
    }
}

pub fn internal_directory(context: &GlobalCompilationContext, unit: &CompilationUnit) -> PathBuf {
    let mut profile_hash = DefaultHasher::new();
    context.config.backend.hash(&mut profile_hash);
    context.config.optimization_level.hash(&mut profile_hash);
    compilation_hash().hash(&mut profile_hash);
    let profile_hash = profile_hash.finish().to_string();

    let mut complete_path = context.config.internal_directory.clone();
    complete_path.push(profile_hash);

    let mut identifier_string = unit.identifier().to_string();
    identifier_string.push_str(".cx");
    complete_path.push(identifier_string);

    let parent = complete_path.parent().unwrap_or(&complete_path);
    std::fs::create_dir_all(parent).unwrap_or_else(|_| {
        panic!(
            "Failed to create internal directory: {}",
            parent.display()
        )
    });

    complete_path
}
