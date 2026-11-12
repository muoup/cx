use crate::{help::Topic, log::error};
use cx_log::CXResult;
use std::path::PathBuf;
use std::process::{Command as ProcessCommand, ExitCode};

use cx_pipeline::project;
use cx_pipeline_data::config::{
    find_project_root, load_project, Profile, ProjectConfig, TargetKind, CONFIG_FILE,
};
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, UnsafeFlags,
};

use crate::{
    args::{self, BuildArgs, RunArgs},
    setup_internal_directory,
};

pub(crate) fn run_project(args: RunArgs) -> CXResult<ExitCode> {
    let project = load(Topic::Run)?;
    let selected = match args.build.target.as_deref() {
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
        None => {
            let executables = project
                .targets
                .iter()
                .enumerate()
                .filter(|(_, target)| target.default && target.kind == TargetKind::Executable)
                .map(|(index, _)| index)
                .collect::<Vec<_>>();
            match executables.as_slice() {
                [index] => *index,
                [] => {
                    return Err(error(
                        "the project has no default executable to run",
                        Some(Topic::Run),
                    ))
                }
                _ => {
                    let names = executables
                        .iter()
                        .map(|index| format!("  {}", project.targets[*index].name))
                        .collect::<Vec<_>>()
                        .join("\n");
                    return Err(error(
                        format!("the project has several default executables; pass one to 'cx run'\n{names}"),
                        Some(Topic::Run),
                    ));
                }
            }
        }
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
    let selected = match args.target.as_deref() {
        Some(name) => vec![target_index(&project, name, topic)?],
        None => project
            .targets
            .iter()
            .enumerate()
            .filter(|(_, target)| target.default)
            .map(|(index, _)| index)
            .collect(),
    };
    if selected.is_empty() {
        return Err(error("the project has no default targets", Some(topic)));
    }
    build(&project, args, &selected)
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
    let profile = match args.release {
        true => Profile::Release,
        false => Profile::Debug,
    };
    let settings = project.profile(profile);
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
        source_root: project.source_root.clone(),
        internal_directory: setup_internal_directory(&project.root)?,

        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
        predefined_macros: vec![],
        external_modules: Default::default(),
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
