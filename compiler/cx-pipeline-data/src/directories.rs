use crate::{CompilationUnit, GlobalCompilationContext, compilation_hash};
use std::hash::{DefaultHasher, Hash, Hasher};
use std::path::{Component, Path, PathBuf};

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

    let identifier_path = Path::new(unit.identifier());
    for component in identifier_path.components() {
        match component {
            Component::Normal(part) => complete_path.push(part),
            Component::CurDir => {}
            Component::ParentDir => complete_path.push("__parent__"),
            Component::RootDir | Component::Prefix(_) => {}
        }
    }
    complete_path.set_extension("cx");

    let parent = complete_path.parent().unwrap_or(&complete_path);
    std::fs::create_dir_all(parent).unwrap_or_else(|_| {
        panic!(
            "Failed to create internal directory: {}",
            parent.display()
        )
    });

    complete_path
}
