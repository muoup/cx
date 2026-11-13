use crate::{help::Topic, log::error};
use cx_log::CXResult;
use std::path::PathBuf;
use std::process::{Command as ProcessCommand, ExitCode};

use cx_pipeline::project;
use cx_pipeline_data::config::{
    find_project_root, load_project, ProjectConfig, TargetKind, CONFIG_FILE, DEBUG_PROFILE,
};
use cx_pipeline_data::package::Packages;
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, UnsafeFlags,
};

use crate::{
    args::{self, BuildArgs, RunArgs},
    setup_internal_directory,
};

pub(crate) fn run_project(args: RunArgs) -> CXResult<ExitCode> {
    let project = load(Topic::Run)?;
    let selected = match args.build.targets.first() {
        Some(name) => {
            let index = target_index(&project, name, Topic::Run)?;
            if project.targets[index].kind == TargetKind::Library {
                return Err(error(
                    format!("'{name}' is a library and cannot be run"),
                    Some(Topic::Run),
                ));
            }
            index
        }
        None => match executables(&project).as_slice() {
            [index] => *index,
            [] => {
                return Err(error(
                    "the project has no executable to run",
                    Some(Topic::Run),
                ))
            }
            several => {
                let names = several
                    .iter()
                    .map(|index| format!("  {}", project.targets[*index].name))
                    .collect::<Vec<_>>()
                    .join("\n");
                return Err(error(
                    format!("the project has several executables; pass one to 'cx run'\n{names}"),
                    Some(Topic::Run),
                ));
            }
        },
    };

    let outputs = build(&project, args.build, &[selected])?;
    let executable = &outputs[0];
    let status = ProcessCommand::new(executable)
        .args(&args.executable_args)
        .status()
        .map_err(|err| {
            error(
                format!("failed to run {}: {err}", executable.display()),
                None,
            )
        })?;
    Ok(ExitCode::from(status.code().unwrap_or(1) as u8))
}

pub fn build_project(args: BuildArgs, topic: Topic) -> CXResult<Vec<PathBuf>> {
    let project = load(topic)?;
    let selected = match args.targets.is_empty() {
        true => executables(&project),
        false => args
            .targets
            .iter()
            .map(|name| target_index(&project, name, topic))
            .collect::<CXResult<Vec<_>>>()?,
    };
    if selected.is_empty() {
        return Err(error("the project has no executables to build", Some(topic)));
    }
    build(&project, args, &selected)
}

fn executables(project: &ProjectConfig) -> Vec<usize> {
    project
        .targets
        .iter()
        .enumerate()
        .filter(|(_, target)| target.kind == TargetKind::Executable)
        .map(|(index, _)| index)
        .collect()
}

fn load(topic: Topic) -> CXResult<ProjectConfig> {
    let invocation_directory = std::env::current_dir()
        .map_err(|err| error(format!("failed to get current directory: {err}"), None))?;
    let root = find_project_root(&invocation_directory).ok_or_else(|| {
        error(
            format!("no {CONFIG_FILE} found; this command requires a project file"),
            Some(topic),
        )
    })?;
    load_project(&root)
}

fn target_index(project: &ProjectConfig, name: &str, topic: Topic) -> CXResult<usize> {
    project
        .targets
        .iter()
        .position(|target| target.name == name)
        .ok_or_else(|| error(format!("unknown target '{name}'"), Some(topic)))
}

fn build(project: &ProjectConfig, args: BuildArgs, selected: &[usize]) -> CXResult<Vec<PathBuf>> {
    let name = args.profile.as_deref().unwrap_or(DEBUG_PROFILE);
    let profile = project
        .profile(name)
        .ok_or_else(|| error(format!("unknown profile '{name}'"), None))?;
    let settings = &profile.settings;
    let backend = args
        .backend
        .or(settings.backend)
        .unwrap_or_else(args::default_backend);
    check_backend(backend)?;

    let base = CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend,
        compilation_mode: CompilationMode::Executable,
        optimization_level: args
            .optimization_level
            .or(settings.optimization)
            .unwrap_or_default(),
        require_explicit_return: args
            .require_explicit_return
            .or(settings.require_explicit_return),

        output: project.root.clone(),
        internal_directory: setup_internal_directory(&project.root)?,
        packages: Packages::new(Vec::new()),

        link_entries: vec![],
        native_objects: vec![],
        header_modules: Default::default(),

        unsafe_flags: UnsafeFlags {
            no_assert: args.unsafe_flags.no_assert || settings.no_assert.unwrap_or(false),
            no_analysis: args.unsafe_flags.no_analysis || settings.no_analysis.unwrap_or(false),
        },
        verbose: args.verbose,
        dump: args.dump,
        module_mode: true,
        record_types: false,
    };

    project::build(&base, project, profile, selected)
}

fn check_backend(backend: CompilerBackend) -> CXResult<()> {
    match backend {
        CompilerBackend::Cranelift => Ok(()),
        #[cfg(feature = "backend-llvm")]
        CompilerBackend::LLVM => Ok(()),
        #[cfg(not(feature = "backend-llvm"))]
        CompilerBackend::LLVM => Err(error(
            "LLVM backend is not enabled in this build; rebuild cx with the 'backend-llvm' feature",
            None,
        )),
    }
}
