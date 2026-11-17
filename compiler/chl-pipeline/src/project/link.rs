use std::path::PathBuf;

use chl_pipeline_data::config::{LinkEntry, ProjectConfig};

pub struct LinkInputs {
    pub own_objects: Vec<PathBuf>,
    pub dependency_objects: Vec<PathBuf>,
    pub dependencies: Vec<usize>,
    pub ffi_libs: Vec<usize>,
    pub links: Vec<LinkEntry>,
}

pub fn link_inputs(project: &ProjectConfig, target: usize) -> LinkInputs {
    let package = project.targets[target].package;
    let dependencies = project
        .dep_closure(package)
        .into_iter()
        .filter(|other| *other != package)
        .collect::<Vec<_>>();
    let ffi_libs = project.targets[target]
        .ffi_includes
        .iter()
        .map(|include| include.target)
        .collect::<Vec<_>>();

    let mut dependency_objects = Vec::new();
    for &dependency in &dependencies {
        extend_unique(&mut dependency_objects, &project.packages[dependency].objects);
    }
    let mut links = Vec::new();
    for index in std::iter::once(package).chain(dependencies.iter().copied()) {
        extend_unique(&mut links, &project.packages[index].links);
    }
    for &ffi_lib in &ffi_libs {
        extend_unique(&mut links, &link_inputs(project, ffi_lib).links);
    }
    LinkInputs {
        own_objects: project.packages[package].objects.clone(),
        dependency_objects,
        dependencies,
        ffi_libs,
        links,
    }
}

fn extend_unique<T: Clone + PartialEq>(into: &mut Vec<T>, items: &[T]) {
    for item in items {
        if !into.contains(item) {
            into.push(item.clone());
        }
    }
}
