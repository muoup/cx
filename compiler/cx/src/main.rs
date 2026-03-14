mod args;

use args::Command;
use cx_pipeline::{project_compilation, standard_compilation};
use cx_pipeline_data::config::find_and_load_config;
use cx_pipeline_data::{CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel};
use std::path::{Path, PathBuf};

fn default_backend() -> CompilerBackend {
    #[cfg(feature = "backend-llvm")]
    {
        CompilerBackend::LLVM
    }
    #[cfg(not(feature = "backend-llvm"))]
    {
        CompilerBackend::Cranelift
    }
}

fn parse_backend(s: &str) -> Result<CompilerBackend, String> {
    match s {
        "cranelift" => Ok(CompilerBackend::Cranelift),
        "llvm" => Ok(CompilerBackend::LLVM),
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
        other => Err(format!("Unknown optimization level in cx.toml: '{}'", other)),
    }
}

fn setup_internal_directory(working_directory: &Path) -> PathBuf {
    let internal_directory = working_directory.join(".internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");
    std::fs::write(internal_directory.join("compiler-dump.data"), "")
        .expect("Failed to clear dump file");
    internal_directory
}

fn run_file_mode(args: args::FileArgs) {
    let path = Path::new(&args.input_file);
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");

    // Try to find cx.toml for project root
    let (working_directory, project_config) =
        if let Some((root, config)) = find_and_load_config(&invocation_directory) {
            (root, Some(config))
        } else {
            let wd = path
                .parent()
                .filter(|parent| !parent.as_os_str().is_empty())
                .map(|parent| invocation_directory.join(parent))
                .unwrap_or_else(|| invocation_directory.clone());
            (wd, None)
        };

    let file_name = path
        .file_name()
        .expect("Failed to get file name from path")
        .to_str()
        .expect("Failed to convert file name to string");

    let internal_directory = setup_internal_directory(&working_directory);

    let output_file = PathBuf::from(&args.output_file);
    let output = if output_file.is_absolute() {
        output_file
    } else {
        invocation_directory.join(output_file)
    };

    let compiler_config = CompilerConfig {
        backend: args.backend,
        optimization_level: args.optimization_level,
        output,
        analysis: args.analysis,
        verbose: args.verbose,
        working_directory,
        internal_directory,
        compilation_mode: CompilationMode::Binary,
        project_config,
        link_entries: vec![],
    };

    match standard_compilation(compiler_config, PathBuf::from(file_name).as_path()) {
        Ok(_) => {}
        Err(err) => {
            println!();
            err.pretty_print();
            std::process::exit(1);
        }
    }
}

fn run_build_mode(args: args::BuildArgs) {
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
            .map(|s| parse_backend(s).unwrap_or_else(|e| { eprintln!("Error: {e}"); std::process::exit(1); }))
            .unwrap_or_else(default_backend)
    });

    let optimization_level = args.optimization_level.unwrap_or_else(|| {
        build_section
            .and_then(|b| b.optimization.as_ref())
            .map(|s| parse_optimization(s).unwrap_or_else(|e| { eprintln!("Error: {e}"); std::process::exit(1); }))
            .unwrap_or_default()
    });

    let analysis = args.analysis.unwrap_or_else(|| {
        build_section.and_then(|b| b.analysis).unwrap_or(false)
    });

    let internal_directory = setup_internal_directory(&project_root);

    let base_config = CompilerConfig {
        backend,
        optimization_level,
        output: project_root.clone(), // placeholder, overridden per-target
        analysis,
        verbose: args.verbose,
        working_directory: project_root.clone(),
        internal_directory,
        compilation_mode: CompilationMode::Binary,
        project_config: Some(config.clone()),
        link_entries: vec![],
    };

    match project_compilation(base_config, &config, args.target.as_deref()) {
        Ok(_) => {}
        Err(err) => {
            err.pretty_print();
            std::process::exit(1);
        }
    }
}

fn run_init_mode(args: args::InitArgs) {
    let project_dir = Path::new(&args.project_name);

    if project_dir.exists() {
        eprintln!("Error: Directory '{}' already exists.", args.project_name);
        std::process::exit(1);
    }

    std::fs::create_dir_all(project_dir).unwrap_or_else(|e| {
        eprintln!("Error: Failed to create directory '{}': {}", args.project_name, e);
        std::process::exit(1);
    });

    let cx_toml = format!(
        r#"[project]
name = "{name}"

[build]
backend = "cranelift"
optimization = "O0"

[workspace.targets.default]
binaries = [
  {{ name = "{name}", entry = "main.cx" }},
]
"#,
        name = args.project_name
    );

    let main_cx = r#"import std::io;

i32 main() {
    io::println("Hello, world!");
    return 0;
}
"#;

    std::fs::write(project_dir.join("cx.toml"), cx_toml).unwrap_or_else(|e| {
        eprintln!("Error: Failed to write cx.toml: {}", e);
        std::process::exit(1);
    });

    std::fs::write(project_dir.join("main.cx"), main_cx).unwrap_or_else(|e| {
        eprintln!("Error: Failed to write main.cx: {}", e);
        std::process::exit(1);
    });

    println!("Created project '{}'", args.project_name);
    println!("  {}/cx.toml", args.project_name);
    println!("  {}/main.cx", args.project_name);
}

fn main() {
    let command = match args::parse_args() {
        Ok(cmd) => cmd,
        Err(err) => {
            eprintln!("Error: {err}");
            std::process::exit(1);
        }
    };

    match command {
        Command::CompileFile(args) => run_file_mode(args),
        Command::Build(args) => run_build_mode(args),
        Command::Init(args) => run_init_mode(args),
    }
}
