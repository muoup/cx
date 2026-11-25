use std::path::{Path, PathBuf};

use chl_lexer::{IncludeRoot, IncludeSource};
use chl_log::CHLResult;
use chl_pipeline_data::config::{
    IncludeSpec, IncludeSpecSource, MountTarget, ProfileSettings, ProjectConfig, TargetKind,
};
use chl_pipeline_data::package::{Package, PackageId, Packages};
use chl_util::identifier::CHLIdent;

use super::sources::{is_c, library_sources, target_sources};
use crate::sources::expand_headers;

pub struct ProjectPackages {
    pub packages: Packages,
    ids: Vec<PackageId>,
}

impl ProjectPackages {
    pub fn new(
        project: &ProjectConfig,
        profile: &ProfileSettings,
        profile_directory: &Path,
    ) -> CHLResult<Self> {
        let mut packages = Packages::new(profile.defines.clone());
        let mut ids = Vec::new();
        for (index, package) in project.packages.iter().enumerate() {
            let includes = package.includes.iter().map(include_root).collect::<CHLResult<Vec<_>>>()?;
            let exports = library_sources(project, index)?.into_iter().filter(|path| !is_c(path)).collect();
            ids.push(packages.push(Package {
                includes,
                defines: profile.defines.iter().chain(&package.defines).cloned().collect(),
                exports: Some(exports),
                ..Package::new(Some(CHLIdent::new(package.name.as_str())), package.source_root.clone())
            }));
        }
        for (package, id) in project.packages.iter().zip(&ids) {
            packages.get_mut(*id).mounts = package
                .mounts
                .iter()
                .map(|mount| {
                    let target = match mount.target {
                        MountTarget::Std => PackageId::STD,
                        MountTarget::Package(index) => ids[index],
                    };
                    (CHLIdent::new(mount.prefix.as_str()), target)
                })
                .collect();
        }
        for (index, target) in project.targets.iter().enumerate() {
            if target.ffi_includes.is_empty() {
                continue;
            }
            let roots = target
                .ffi_includes
                .iter()
                .map(|include| IncludeRoot {
                    source: IncludeSource::Generated {
                        directory: header_directory(project, profile_directory, include.target),
                        library: project.targets[include.target].name.clone(),
                    },
                    alias: include.prefix.clone(),
                })
                .collect::<Vec<_>>();
            let unit_includes = &mut packages.get_mut(ids[target.package]).unit_includes;
            for source in target_sources(project, index)?.into_iter().filter(|path| is_c(path)) {
                let existing = unit_includes.entry(source).or_default();
                for root in &roots {
                    if !existing.contains(root) {
                        existing.push(root.clone());
                    }
                }
            }
        }
        Ok(Self { packages, ids })
    }

    pub fn id(&self, package: usize) -> PackageId {
        self.ids[package]
    }
}

pub fn header_directory(project: &ProjectConfig, profile_directory: &Path, ffi_lib: usize) -> PathBuf {
    let target = &project.targets[ffi_lib];
    profile_directory.join("include").join(&project.packages[target.package].name).join(&target.name)
}

pub fn artifact_path(project: &ProjectConfig, profile_directory: &Path, target: usize) -> PathBuf {
    let config = &project.targets[target];
    let name = match config.kind {
        TargetKind::Executable => config.name.clone(),
        TargetKind::FfiLib => format!("{}.o", config.name),
    };
    match config.package {
        0 => profile_directory.join(name),
        package => profile_directory.join("deps").join(&project.packages[package].name).join(name),
    }
}

fn include_root(spec: &IncludeSpec) -> CHLResult<IncludeRoot> {
    let source = match &spec.source {
        IncludeSpecSource::Directory(directory) => IncludeSource::Directory(directory.clone()),
        IncludeSpecSource::Glob { base, pattern } => {
            let (fixed, files) = expand_headers(base, pattern)?;
            IncludeSource::Files { base: fixed, files: files.into_iter().collect() }
        }
    };
    Ok(IncludeRoot { source, alias: spec.alias.clone() })
}
