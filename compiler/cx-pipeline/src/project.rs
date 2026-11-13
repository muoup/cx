use std::path::{Path, PathBuf};

use cx_lexer::{IncludeRoot, IncludeSource};
use cx_log::{CXResult, catalogue::driver as catalogue};
use cx_pipeline_data::config::{
    IncludeSpec, IncludeSpecSource, LinkEntry, MountTarget, ProfileConfig, ProfileSettings,
    ProjectConfig, TargetConfig, TargetKind,
};
use cx_pipeline_data::internal_storage::resource_path;
use cx_pipeline_data::package::{Package, PackageId, Packages};
use cx_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};
use cx_util::identifier::CXIdent;
use rustc_hash::FxHashSet;

use crate::linker::{link_executable, link_relocatable};
use crate::log::pipeline_error;
use crate::scheduler::imported_units;
use crate::sources::{expand_headers, expand_patterns};
use crate::{library_roots, reporting, root_unit, run_pipeline};

const RESERVED_TARGETS: &[&str] = &["obj", "include"];

pub struct ProjectPackages {
    pub packages: Packages,
    ids: Vec<PackageId>,
}

struct TargetPlan<'a> {
    target: &'a TargetConfig,
    units: Vec<CompilationUnit>,
    headers: FxHashSet<PathBuf>,
}

impl ProjectPackages {
    pub fn new(project: &ProjectConfig, profile: &ProfileSettings) -> CXResult<Self> {
        let mut packages = Packages::new(profile.defines.clone());
        let mut ids = Vec::new();
        for package in &project.packages {
            let includes = package
                .usage
                .includes
                .iter()
                .map(include_root)
                .collect::<CXResult<Vec<_>>>()?;
            ids.push(packages.push(Package {
                name: Some(CXIdent::new(package.name.as_str())),
                source_root: package.source_root.clone(),
                mounts: Vec::new(),
                includes,
                defines: profile.defines.iter().chain(&package.usage.defines).cloned().collect(),
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
                    (CXIdent::new(mount.prefix.as_str()), target)
                })
                .collect();
        }
        Ok(Self { packages, ids })
    }

    pub fn id(&self, package: usize) -> PackageId {
        self.ids[package]
    }

    fn index(&self, id: PackageId) -> Option<usize> {
        self.ids.iter().position(|other| *other == id)
    }
}

pub fn include_root(spec: &IncludeSpec) -> CXResult<IncludeRoot> {
    let source = match &spec.source {
        IncludeSpecSource::Directory(directory) => IncludeSource::Directory(directory.clone()),
        IncludeSpecSource::Glob { base, pattern } => {
            let (fixed, files) = expand_headers(base, pattern)?;
            IncludeSource::Files { base: fixed, files: files.into_iter().collect() }
        }
    };
    Ok(IncludeRoot { source, alias: spec.alias.clone() })
}

pub fn target_sources(project: &ProjectConfig, target: &TargetConfig) -> CXResult<Vec<PathBuf>> {
    let root = &project.packages[target.package].root;
    Ok(expand_patterns(root, &target.matches, &target.excludes)?
        .into_iter()
        .map(|path| absolute(root.join(path)))
        .collect())
}

fn target_headers(
    project: &ProjectConfig,
    target: &TargetConfig,
    sources: &[PathBuf],
) -> CXResult<FxHashSet<PathBuf>> {
    if target.headers.is_empty() {
        return Ok(FxHashSet::default());
    }
    let root = &project.packages[target.package].root;
    Ok(expand_patterns(root, &target.headers, &[])?
        .into_iter()
        .map(|path| absolute(root.join(path)))
        .filter(|path| sources.contains(path))
        .collect())
}

pub fn build(
    base: &CompilerConfig,
    project: &ProjectConfig,
    profile: &ProfileConfig,
    selected: &[usize],
) -> CXResult<Vec<PathBuf>> {
    let profile_directory = base.internal_directory.join(&profile.name);
    let packages = ProjectPackages::new(project, &profile.settings)?;

    let mut config = base.clone();
    config.packages = packages.packages.clone();
    config.internal_directory = profile_directory.join("obj");
    config.module_mode = true;

    let mut plans = Vec::new();
    for &index in selected {
        let target = &project.targets[index];
        if RESERVED_TARGETS.contains(&target.name.as_str()) {
            return Err(pipeline_error(
                &catalogue::INVALID_CONFIG,
                format!("target name '{}' is reserved for the build directory", target.name),
            ));
        }
        let sources = target_sources(project, target)?;
        let headers = target_headers(project, target, &sources)?;
        let package = packages.id(target.package);
        let units = sources
            .iter()
            .map(|source| root_unit(&config, package, source))
            .collect::<CXResult<Vec<_>>>()?;
        config.header_modules.extend(headers.iter().cloned());
        plans.push(TargetPlan { target, units, headers });
    }

    let mut roots = Vec::new();
    for unit in plans.iter().flat_map(|plan| &plan.units) {
        if !roots.contains(unit) {
            roots.push(unit.clone());
        }
    }

    for plan in &plans {
        let kind = match plan.target.kind {
            TargetKind::Executable => "executable",
            TargetKind::Library => "library",
        };
        eprintln!(
            "Building {kind} '{}' ({}, {} sources)",
            plan.target.name,
            profile.name,
            plan.units.len()
        );
    }

    let context = GlobalCompilationContext::new(config);
    reporting(context.config.verbose, |reporter| {
        run_pipeline(&context, &roots, reporter)?;
        let mut outputs = Vec::new();
        for plan in &plans {
            reporter.link_status(&format!("Linking '{}'", plan.target.name));
            outputs.push(link_target(&context, project, &packages, &profile_directory, plan)?);
        }
        Ok(outputs)
    })
}

fn link_target(
    context: &GlobalCompilationContext,
    project: &ProjectConfig,
    packages: &ProjectPackages,
    profile_directory: &Path,
    plan: &TargetPlan,
) -> CXResult<PathBuf> {
    let reachable = reachable_units(context, &plan.units);
    let cx_objects = reachable
        .iter()
        .map(|unit| resource_path(context, unit, ".o"))
        .collect::<Vec<_>>();

    let mut used = vec![plan.target.package];
    used.extend(reachable.iter().filter_map(|unit| packages.index(unit.package())));
    used.sort();
    used.dedup();
    let (native_objects, link) = native_inputs(project, &used);

    create_directory(profile_directory)?;
    match plan.target.kind {
        TargetKind::Executable => {
            let output = profile_directory.join(&plan.target.name);
            let objects = cx_objects.into_iter().chain(native_objects).collect::<Vec<_>>();
            link_executable(&output, &objects, &link)?;
            Ok(output)
        }
        TargetKind::Library => {
            let output = profile_directory.join(format!("{}.o", plan.target.name));
            link_relocatable(&output, &cx_objects, &library_roots(context, &plan.units))?;
            write_headers(context, project, profile_directory, plan, &link)?;
            Ok(output)
        }
    }
}

pub fn native_inputs(project: &ProjectConfig, packages: &[usize]) -> (Vec<PathBuf>, Vec<LinkEntry>) {
    let mut objects = Vec::<PathBuf>::new();
    let mut link = Vec::<LinkEntry>::new();
    for usage in packages.iter().map(|index| &project.packages[*index].usage) {
        for object in &usage.objects {
            if !objects.contains(object) {
                objects.push(object.clone());
            }
        }
        for entry in &usage.link {
            if !link.contains(entry) {
                link.push(entry.clone());
            }
        }
    }
    (objects, link)
}

fn reachable_units(context: &GlobalCompilationContext, roots: &[CompilationUnit]) -> Vec<CompilationUnit> {
    let mut seen = roots.iter().cloned().collect::<FxHashSet<_>>();
    let mut reachable = roots.to_vec();
    let mut index = 0;
    while index < reachable.len() {
        for unit in imported_units(context, &reachable[index]) {
            if seen.insert(unit.clone()) {
                reachable.push(unit);
            }
        }
        index += 1;
    }
    reachable
}

fn write_headers(
    context: &GlobalCompilationContext,
    project: &ProjectConfig,
    profile_directory: &Path,
    plan: &TargetPlan,
    link: &[LinkEntry],
) -> CXResult<()> {
    let source_root = &project.packages[plan.target.package].source_root;
    let directory = profile_directory.join("include").join(&plan.target.name);
    let headers = context.headers.lock().expect("Deadlock on headers mutex");
    for source in &plan.headers {
        let Some(body) = headers.get(source) else {
            continue;
        };
        let relative = source.strip_prefix(source_root).unwrap_or(source);
        let path = directory.join(relative).with_extension("h");
        create_directory(path.parent().expect("header has a parent directory"))?;
        std::fs::write(&path, cx_c_header::render_header(body, link)).map_err(|error| {
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
    Ok(())
}

fn absolute(path: PathBuf) -> PathBuf {
    path.canonicalize().unwrap_or(path)
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
