pub mod link;
pub mod packages;
pub mod sources;

use std::path::{Path, PathBuf};

use chl_log::{CHLResult, catalogue::driver as catalogue};
use chl_pipeline_data::config::{LinkEntry, ProfileConfig, ProjectConfig, TargetKind};
use chl_pipeline_data::internal_storage::resource_path;
use chl_pipeline_data::jobs::CompilationStep;
use chl_pipeline_data::package::PackageId;
use chl_pipeline_data::{CompilationUnit, CompilerConfig, GlobalCompilationContext};

use self::link::{LinkInputs, link_inputs};
use self::packages::{ProjectPackages, artifact_path, header_directory};
use self::sources::{header_sources, is_c, library_sources, target_sources};
use crate::linker::{link_executable, link_relocatable};
use crate::log::pipeline_error;
use crate::pipeline::Pipeline;
use crate::{reachable_units, reporting, root_unit, run_pipeline};

const RESERVED_TARGETS: &[&str] = &["obj", "include", "deps"];

struct TargetPlan {
    target: usize,
    inputs: LinkInputs,
    roots: Vec<CompilationUnit>,
}

struct HeaderPlan {
    ffi_lib: usize,
    modules: Vec<PathBuf>,
    links: Vec<LinkEntry>,
}

pub fn build(
    base: &CompilerConfig,
    project: &ProjectConfig,
    profile: &ProfileConfig,
    selected: &[usize],
) -> CHLResult<Vec<PathBuf>> {
    let profile_directory = base.internal_directory.join(&profile.name);
    let packages = ProjectPackages::new(project, &profile.settings, &profile_directory)?;

    let mut config = base.clone();
    config.packages = packages.packages.clone();
    config.internal_directory = profile_directory.join("obj");
    config.module_mode = true;

    let mut order = Vec::new();
    for &target in selected {
        build_order(project, target, &mut order);
    }
    let mut plans = Vec::new();
    for &target in &order {
        let name = &project.targets[target].name;
        if RESERVED_TARGETS.contains(&name.as_str()) {
            return Err(pipeline_error(
                &catalogue::INVALID_CONFIG,
                format!("target name '{name}' is reserved for the build directory"),
            ));
        }
        let inputs = link_inputs(project, target);
        let roots = target_roots(project, &packages, &config, target, &inputs)?;
        plans.push(TargetPlan { target, inputs, roots });
    }

    let mut headers = Vec::new();
    for plan in &plans {
        if project.targets[plan.target].kind == TargetKind::FfiLib {
            let modules = header_sources(project, plan.target)?;
            config.header_modules.extend(modules.iter().cloned());
            headers.push(HeaderPlan { ffi_lib: plan.target, modules, links: plan.inputs.links.clone() });
        }
    }

    let mut roots = Vec::new();
    for unit in plans.iter().flat_map(|plan| &plan.roots) {
        if !roots.contains(unit) {
            roots.push(unit.clone());
        }
    }
    let staged = order.iter().any(|target| !project.targets[*target].ffi_includes.is_empty());
    let (first, second): (Vec<_>, Vec<_>) = match staged {
        true => roots.into_iter().partition(|unit| !is_c(unit.module().as_path())),
        false => (roots, Vec::new()),
    };

    for plan in &plans {
        let target = &project.targets[plan.target];
        let kind = match target.kind {
            TargetKind::Executable => "executable",
            TargetKind::FfiLib => "ffi-lib",
        };
        eprintln!("Building {kind} '{}' ({}, {} sources)", target.name, profile.name, plan.roots.len());
    }

    let context = GlobalCompilationContext::new(config);
    reporting(context.config.verbose, |reporter| {
        let mut pipeline = Pipeline::new(&context, CompilationStep::Codegen);
        run_pipeline(&mut pipeline, &first, reporter)?;
        for header in &headers {
            write_headers(&context, project, &profile_directory, header)?;
        }
        run_pipeline(&mut pipeline, &second, reporter)?;

        let mut outputs = Vec::new();
        for plan in &plans {
            let target = &project.targets[plan.target];
            reporter.link_status(&format!("Linking '{}'", target.name));
            let package = packages.id(target.package);
            let output = link_target(&context, project, &profile_directory, plan, package)?;
            if selected.contains(&plan.target) {
                outputs.push(output);
            }
        }
        Ok(outputs)
    })
}

fn build_order(project: &ProjectConfig, target: usize, order: &mut Vec<usize>) {
    if order.contains(&target) {
        return;
    }
    for include in &project.targets[target].ffi_includes {
        build_order(project, include.target, order);
    }
    order.push(target);
}

fn target_roots(
    project: &ProjectConfig,
    packages: &ProjectPackages,
    config: &CompilerConfig,
    target: usize,
    inputs: &LinkInputs,
) -> CHLResult<Vec<CompilationUnit>> {
    let package = project.targets[target].package;
    let mut roots = Vec::new();
    for source in target_sources(project, target)? {
        roots.push(root_unit(config, packages.id(package), &source)?);
    }
    for &dependency in &inputs.dependencies {
        for source in library_sources(project, dependency)?.into_iter().filter(|path| is_c(path)) {
            roots.push(root_unit(config, packages.id(dependency), &source)?);
        }
    }
    Ok(roots)
}

fn link_target(
    context: &GlobalCompilationContext,
    project: &ProjectConfig,
    profile_directory: &Path,
    plan: &TargetPlan,
    package: PackageId,
) -> CHLResult<PathBuf> {
    let target = &project.targets[plan.target];
    let reachable = reachable_units(context, &plan.roots);
    let output = artifact_path(project, profile_directory, plan.target);
    create_directory(output.parent().expect("artifacts live in the profile directory"))?;
    let ffi_libs = plan
        .inputs
        .ffi_libs
        .iter()
        .map(|ffi_lib| artifact_path(project, profile_directory, *ffi_lib))
        .collect::<Vec<_>>();

    match target.kind {
        TargetKind::Executable => {
            let objects = reachable
                .iter()
                .map(|unit| resource_path(context, unit, ".o"))
                .chain(plan.inputs.own_objects.iter().cloned())
                .chain(plan.inputs.dependency_objects.iter().cloned())
                .chain(ffi_libs)
                .collect::<Vec<_>>();
            link_executable(&output, &objects, &plan.inputs.links)?;
        }
        TargetKind::FfiLib => {
            let (own, dependencies): (Vec<_>, Vec<_>) =
                reachable.iter().partition(|unit| unit.package() == package);
            let own = own
                .iter()
                .map(|unit| resource_path(context, unit, ".o"))
                .chain(plan.inputs.own_objects.iter().cloned())
                .collect::<Vec<_>>();
            let dependencies = dependencies
                .iter()
                .map(|unit| resource_path(context, unit, ".o"))
                .chain(plan.inputs.dependency_objects.iter().cloned())
                .chain(ffi_libs)
                .collect::<Vec<_>>();
            link_relocatable(&output, &own, &dependencies, &plan.inputs.links)?;
        }
    }
    Ok(output)
}

fn write_headers(
    context: &GlobalCompilationContext,
    project: &ProjectConfig,
    profile_directory: &Path,
    header: &HeaderPlan,
) -> CHLResult<()> {
    let package = project.targets[header.ffi_lib].package;
    let source_root = &project.packages[package].source_root;
    let directory = header_directory(project, profile_directory, header.ffi_lib);
    let headers = context.headers.lock().expect("Deadlock on headers mutex");
    for source in &header.modules {
        let Some(body) = headers.get(source) else {
            continue;
        };
        let relative = source.strip_prefix(source_root).unwrap_or(source);
        let path = directory.join(relative).with_extension("h");
        create_directory(path.parent().expect("header has a parent directory"))?;
        std::fs::write(&path, chl_c_header::render_header(body, &header.links)).map_err(|error| {
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

fn create_directory(path: &Path) -> CHLResult<()> {
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
