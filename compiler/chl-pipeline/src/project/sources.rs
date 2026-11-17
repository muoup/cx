use std::path::{Path, PathBuf};

use chl_log::CHLResult;
use chl_pipeline_data::config::{ProjectConfig, TargetKind};

use crate::sources::expand_patterns;

pub fn target_sources(project: &ProjectConfig, target: usize) -> CHLResult<Vec<PathBuf>> {
    let target = &project.targets[target];
    let mut matches = target.matches.clone();
    if target.kind == TargetKind::FfiLib {
        matches.extend(target.headers.iter().cloned());
    }
    expand(&project.packages[target.package].root, &matches, &target.excludes)
}

pub fn header_sources(project: &ProjectConfig, target: usize) -> CHLResult<Vec<PathBuf>> {
    let target = &project.targets[target];
    expand(&project.packages[target.package].root, &target.headers, &target.excludes)
}

pub fn library_sources(project: &ProjectConfig, package: usize) -> CHLResult<Vec<PathBuf>> {
    let package = &project.packages[package];
    match &package.library {
        Some(library) => expand(&package.root, &library.matches, &library.excludes),
        None => Ok(Vec::new()),
    }
}

pub fn is_c(path: &Path) -> bool {
    path.extension().is_some_and(|extension| extension == "c")
}

fn expand(root: &Path, matches: &[String], excludes: &[String]) -> CHLResult<Vec<PathBuf>> {
    if matches.is_empty() {
        return Ok(Vec::new());
    }
    let mut sources = Vec::new();
    for path in expand_patterns(root, matches, excludes)? {
        let path = absolute(root.join(path));
        if !sources.contains(&path) {
            sources.push(path);
        }
    }
    Ok(sources)
}

fn absolute(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or(path)
}
