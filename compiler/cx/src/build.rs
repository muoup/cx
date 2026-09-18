use crate::{help::Topic, log::error};
use cx_log::CXResult;
use std::process::ExitCode;

use cx_pipeline::project_compilation;
use cx_pipeline_data::{
    config::find_and_load_config, ArchitectureConfig, CompilationMode, CompilerBackend,
    CompilerConfig, OptimizationLevel,
};
use std::path::PathBuf;
use std::process::Command as ProcessCommand;

use crate::{
    args::{self, BuildArgs, RunArgs},
    setup_internal_directory,
};

pub(crate) fn run_project(args: RunArgs) -> CXResult<ExitCode> {
    let binaries_built = build_project(args.build, Topic::Run)?;
    let executable = match binaries_built.as_slice() {
        [executable] => executable,
        [] => {
            return Err(error(
                "'cx run' did not build any executable binaries",
                Some(Topic::Run),
            ))
        }
        _ => {
            let binaries = binaries_built
                .iter()
                .map(|binary| format!("  {}", binary.display()))
                .collect::<Vec<_>>()
                .join("\n");
            return Err(error(format!("'cx run' built multiple executable binaries; pass a target to select one\n{binaries}"), Some(Topic::Run)));
        }
    };
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
    let invocation_directory = std::env::current_dir()
        .map_err(|err| error(format!("failed to get current directory: {err}"), None))?;
    let (project_root, config) = find_and_load_config(&invocation_directory)
        .map_err(|err| error(err, None))?
        .ok_or_else(|| {
            error(
                "no cx.toml found; this command requires a project file",
                Some(topic),
            )
        })?;

    // Resolve build settings: CLI overrides cx.toml [build] section
    let build_section = config.build.as_ref();

    let backend = match args.backend {
        Some(backend) => backend,
        None => build_section
            .and_then(|build| build.backend.as_deref())
            .map(parse_backend)
            .transpose()?
            .unwrap_or_else(args::default_backend),
    };
    let optimization_level = match args.optimization_level {
        Some(level) => level,
        None => build_section
            .and_then(|build| build.optimization.as_deref())
            .map(parse_optimization)
            .transpose()?
            .unwrap_or_default(),
    };

    let require_explicit_return = args
        .require_explicit_return
        .or_else(|| build_section.and_then(|build| build.require_explicit_return));

    let internal_directory = setup_internal_directory(&project_root)?;

    let base_config = CompilerConfig {
        architecture: ArchitectureConfig::native(),
        output: project_root.clone(), // placeholder, overridden per-target
        unsafe_mode: args.unsafe_mode,
        verbose: args.verbose,
        dump: args.dump,
        working_directory: project_root.clone(),
        compilation_mode: CompilationMode::Executable,
        module_mode: true,
        project_config: Some(config.clone()),

        internal_directory,
        backend,
        optimization_level,
        require_explicit_return,

        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
        predefined_macros: vec![],
    };

    project_compilation(base_config, &config, args.target.as_deref())
}

fn parse_backend(s: &str) -> CXResult<CompilerBackend> {
    match s {
        "cranelift" => Ok(CompilerBackend::Cranelift),
        #[cfg(feature = "backend-llvm")]
        "llvm" => Ok(CompilerBackend::LLVM),
        #[cfg(not(feature = "backend-llvm"))]
        "llvm" => Err(error(
            "LLVM backend is not enabled in this build; rebuild cx with the 'backend-llvm' feature",
            None,
        )),
        other => Err(error(
            format!("unknown backend in cx.toml: '{other}'"),
            None,
        )),
    }
}

fn parse_optimization(s: &str) -> CXResult<OptimizationLevel> {
    match s {
        "O0" => Ok(OptimizationLevel::O0),
        "O1" => Ok(OptimizationLevel::O1),
        "O2" => Ok(OptimizationLevel::O2),
        "O3" => Ok(OptimizationLevel::O3),
        "Osize" => Ok(OptimizationLevel::Osize),
        "Ofast" => Ok(OptimizationLevel::Ofast),
        other => Err(error(
            format!("unknown optimization level in cx.toml: '{other}'"),
            None,
        )),
    }
}
