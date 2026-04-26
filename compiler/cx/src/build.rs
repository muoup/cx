use cx_pipeline::project_compilation;
use cx_pipeline_data::{CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel, config::find_and_load_config};

use crate::{args::{self, BuildArgs}, setup_internal_directory};

pub(crate) fn run_build_mode(args: BuildArgs) {
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");

    let (project_root, config) = find_and_load_config(&invocation_directory).unwrap_or_else(|| {
        eprintln!("Error: No cx.toml found. `cx build` requires a cx.toml project file.");
        std::process::exit(1);
    });

    // Resolve build settings: CLI overrides cx.toml [build] section
    let build_section = config.build.as_ref();

    let backend = args.backend.unwrap_or_else(|| {
        build_section
            .and_then(|b| b.backend.as_ref())
            .map(|s| {
                parse_backend(s).unwrap_or_else(|e| {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                })
            })
            .unwrap_or_else(args::default_backend)
    });

    let optimization_level = args.optimization_level.unwrap_or_else(|| {
        build_section
            .and_then(|b| b.optimization.as_ref())
            .map(|s| {
                parse_optimization(s).unwrap_or_else(|e| {
                    eprintln!("Error: {e}");
                    std::process::exit(1);
                })
            })
            .unwrap_or_default()
    });

    let analysis = args
        .analysis
        .unwrap_or_else(|| build_section.and_then(|b| b.analysis).unwrap_or(false));

    let internal_directory = setup_internal_directory(&project_root);

    let base_config = CompilerConfig {
        backend,
        optimization_level,
        output: project_root.clone(), // placeholder, overridden per-target
        analysis,
        verbose: args.verbose,
        working_directory: project_root.clone(),
        internal_directory,
        compilation_mode: CompilationMode::Executable,
        module_mode: true,
        project_config: Some(config.clone()),
        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
    };

    match project_compilation(base_config, &config, args.target.as_deref()) {
        Ok(_) => {}
        Err(err) => {
            err.pretty_print();
            std::process::exit(1);
        }
    }
}

fn parse_backend(s: &str) -> Result<CompilerBackend, String> {
    match s {
        "cranelift" => Ok(CompilerBackend::Cranelift),
        #[cfg(feature = "backend-llvm")]
        "llvm" => Ok(CompilerBackend::LLVM),
        #[cfg(not(feature = "backend-llvm"))]
        "llvm" => Err(
            "LLVM backend is not enabled in this build. Recompile cx with the `backend-llvm` feature to use backend = \"llvm\"."
                .to_string(),
        ),
        other => Err(format!("Unknown backend in cx.toml: '{}'", other)),
    }
}

fn parse_optimization(s: &str) -> Result<OptimizationLevel, String> {
    match s {
        "O0" => Ok(OptimizationLevel::O0),
        "O1" => Ok(OptimizationLevel::O1),
        "O2" => Ok(OptimizationLevel::O2),
        "O3" => Ok(OptimizationLevel::O3),
        "Osize" => Ok(OptimizationLevel::Osize),
        "Ofast" => Ok(OptimizationLevel::Ofast),
        other => Err(format!(
            "Unknown optimization level in cx.toml: '{}'",
            other
        )),
    }
}