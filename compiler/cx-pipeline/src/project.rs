use std::path::{Path, PathBuf};

use cx_log::{CXResult, catalogue::driver as catalogue};
use cx_pipeline_data::config::{
    DependencyRef, LinkEntry, Profile, ProjectConfig, TargetConfig, TargetKind,
};
use cx_pipeline_data::{CompilationMode, CompilerConfig};
use rustc_hash::FxHashSet;

use crate::compile;
use crate::log::pipeline_error;
use crate::sources::expand_patterns;

pub struct TargetSources {
    pub sources: Vec<PathBuf>,
    pub exported: FxHashSet<PathBuf>,
}

#[derive(Default)]
pub struct TargetUsage {
    pub include: Vec<PathBuf>,
    pub defines: Vec<(String, String)>,
    pub objects: Vec<PathBuf>,
    pub link: Vec<LinkEntry>,
    pub external: FxHashSet<PathBuf>,
}

pub fn output_directory(internal_directory: &Path, profile: Profile) -> PathBuf {
    internal_directory.join("output").join(profile.name())
}

pub fn artifact(internal_directory: &Path, profile: Profile, target: &TargetConfig) -> PathBuf {
    let directory = output_directory(internal_directory, profile);
    match target.kind {
        TargetKind::Executable => directory.join(&target.name),
        TargetKind::Library => directory.join(format!("{}.o", target.name)),
    }
}

pub fn header_directory(internal_directory: &Path, profile: Profile, target: &TargetConfig) -> PathBuf {
    output_directory(internal_directory, profile)
        .join("include")
        .join(&target.name)
}

pub fn target_sources(project: &ProjectConfig, target: &TargetConfig) -> CXResult<TargetSources> {
    let absolute = |paths: Vec<PathBuf>| paths.into_iter().map(|path| project.root.join(path));
    let patterns = target
        .sources
        .iter()
        .map(|source| source.pattern.clone())
        .collect::<Vec<_>>();
    let sources = absolute(expand_patterns(&project.root, &patterns)?).collect::<Vec<_>>();

    let exported_patterns = target
        .sources
        .iter()
        .filter(|source| source.exported)
        .map(|source| source.pattern.clone())
        .collect::<Vec<_>>();
    let exported = match exported_patterns.is_empty() {
        true => FxHashSet::default(),
        false => absolute(expand_patterns(&project.root, &exported_patterns)?)
            .filter(|path| sources.contains(path))
            .collect(),
    };
    Ok(TargetSources { sources, exported })
}

pub fn target_usage(
    project: &ProjectConfig,
    internal_directory: &Path,
    profile: Profile,
    target: &TargetConfig,
) -> CXResult<TargetUsage> {
    let mut usage = TargetUsage {
        include: target.usage.include.clone(),
        defines: target.usage.defines.clone(),
        objects: target.usage.objects.clone(),
        link: target.usage.link.clone(),
        external: FxHashSet::default(),
    };
    compile_usage(project, internal_directory, profile, &target.deps, &mut FxHashSet::default(), &mut usage);
    link_usage(project, internal_directory, profile, &target.deps, &mut FxHashSet::default(), &mut usage)?;
    Ok(usage)
}

fn compile_usage(
    project: &ProjectConfig,
    internal_directory: &Path,
    profile: Profile,
    deps: &[DependencyRef],
    visited: &mut FxHashSet<DependencyRef>,
    usage: &mut TargetUsage,
) {
    for &dependency in deps {
        if !visited.insert(dependency) {
            continue;
        }
        match dependency {
            DependencyRef::Bundle(index) => {
                let bundle = &project.dependencies[index];
                extend_unique(&mut usage.include, &bundle.usage.include);
                extend_unique(&mut usage.defines, &bundle.usage.defines);
                compile_usage(project, internal_directory, profile, &bundle.deps, visited, usage);
            }
            DependencyRef::Target(index) => {
                let library = &project.targets[index];
                extend_unique(&mut usage.include, &[header_directory(internal_directory, profile, library)]);
            }
        }
    }
}

fn link_usage(
    project: &ProjectConfig,
    internal_directory: &Path,
    profile: Profile,
    deps: &[DependencyRef],
    visited: &mut FxHashSet<DependencyRef>,
    usage: &mut TargetUsage,
) -> CXResult<()> {
    for &dependency in deps {
        if !visited.insert(dependency) {
            continue;
        }
        let (bundle, deps) = match dependency {
            DependencyRef::Bundle(index) => {
                let bundle = &project.dependencies[index];
                (&bundle.usage, &bundle.deps)
            }
            DependencyRef::Target(index) => {
                let library = &project.targets[index];
                extend_unique(&mut usage.objects, &[artifact(internal_directory, profile, library)]);
                usage.external.extend(target_sources(project, library)?.sources);
                (&library.usage, &library.deps)
            }
        };
        extend_unique(&mut usage.objects, &bundle.objects);
        extend_unique(&mut usage.link, &bundle.link);
        link_usage(project, internal_directory, profile, deps, visited, usage)?;
    }
    Ok(())
}

fn extend_unique<T: Clone + PartialEq>(items: &mut Vec<T>, additions: &[T]) {
    for addition in additions {
        if !items.contains(addition) {
            items.push(addition.clone());
        }
    }
}

pub fn required_targets(project: &ProjectConfig, selected: &[usize]) -> Vec<usize> {
    fn visit(project: &ProjectConfig, deps: &[DependencyRef], required: &mut FxHashSet<usize>) {
        for dependency in deps {
            let deps = match *dependency {
                DependencyRef::Bundle(index) => &project.dependencies[index].deps,
                DependencyRef::Target(index) => {
                    if !required.insert(index) {
                        continue;
                    }
                    &project.targets[index].deps
                }
            };
            visit(project, deps, required);
        }
    }

    let mut required = FxHashSet::default();
    for &index in selected {
        if required.insert(index) {
            visit(project, &project.targets[index].deps, &mut required);
        }
    }
    let mut required = required.into_iter().collect::<Vec<_>>();
    required.sort();
    required
}

pub fn build(
    base: &CompilerConfig,
    project: &ProjectConfig,
    profile: Profile,
    selected: &[usize],
) -> CXResult<Vec<PathBuf>> {
    let internal_directory = &base.internal_directory;
    let mut outputs = Vec::new();

    for index in required_targets(project, selected) {
        let target = &project.targets[index];
        let sources = target_sources(project, target)?;
        let usage = target_usage(project, internal_directory, profile, target)?;
        let output = artifact(internal_directory, profile, target);
        create_directory(output.parent().expect("artifact has a parent directory"))?;

        let mut config = base.clone();
        config.output = output.clone();
        config.source_root = project.source_root.clone();
        config.internal_directory = internal_directory.join(profile.name()).join(&target.name);
        config.module_mode = true;
        config.include_dirs.extend(usage.include);
        config.predefined_macros.extend(usage.defines);
        config.native_objects.extend(usage.objects);
        config.link_entries.extend(usage.link);
        config.external_modules = usage.external;
        config.compilation_mode = match target.kind {
            TargetKind::Executable => CompilationMode::Executable,
            TargetKind::Library => CompilationMode::Library,
        };
        config.header_modules = sources.exported;

        let kind = match target.kind {
            TargetKind::Executable => "executable",
            TargetKind::Library => "library",
        };
        eprintln!(
            "Building {kind} '{}' ({}, {} sources)",
            target.name,
            profile.name(),
            sources.sources.len()
        );
        let headers = compile(config, &sources.sources)?;

        let header_directory = header_directory(internal_directory, profile, target);
        for (source, header) in headers {
            let relative = source.strip_prefix(&project.source_root).unwrap_or(&source);
            let path = header_directory.join(relative).with_extension("h");
            create_directory(path.parent().expect("header has a parent directory"))?;
            std::fs::write(&path, header).map_err(|error| {
                pipeline_error(
                    &catalogue::FILE_OPERATION,
                    (
                        "write".into(),
                        "header".into(),
                        Some(format!("{}", path.display())),
                        format!("{}", error),
                    ),
                )
            })?;
        }

        if selected.contains(&index) {
            outputs.push(output);
        }
    }
    Ok(outputs)
}

fn create_directory(path: &Path) -> CXResult<()> {
    std::fs::create_dir_all(path).map_err(|error| {
        pipeline_error(
            &catalogue::FILE_OPERATION,
            (
                "create".into(),
                "output directory".into(),
                Some(format!("{}", path.display())),
                format!("{}", error),
            ),
        )
    })
}
