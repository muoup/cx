mod args;
mod build;
mod help;
mod init;

use args::Command;
use cx_pipeline::{link_object_files, standard_compilation};
use cx_pipeline_data::{ArchitectureConfig, CompilationMode, CompilerConfig};
use std::path::{Path, PathBuf};

use crate::{
    build::{build_project, run_project},
    init::init_project,
};

fn setup_internal_directory(working_directory: &Path) -> PathBuf {
    let internal_directory = working_directory.join(".internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");
    let legacy_dump = internal_directory.join("compiler-dump.data");
    if legacy_dump.exists() {
        std::fs::remove_file(legacy_dump).expect("Failed to remove legacy dump file");
    }
    internal_directory
}

fn resolve_invocation_path(invocation_directory: &Path, path: impl Into<PathBuf>) -> PathBuf {
    let path = path.into();
    if path.is_absolute() {
        path
    } else {
        invocation_directory.join(path)
    }
}

fn default_object_output(invocation_directory: &Path, input_file: &str) -> PathBuf {
    let stem = Path::new(input_file)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("a");
    invocation_directory.join(format!("{stem}.o"))
}

fn intermediate_object_output(
    internal_directory: &Path,
    index: usize,
    input_file: &str,
) -> PathBuf {
    let stem = Path::new(input_file)
        .file_stem()
        .and_then(|stem| stem.to_str())
        .unwrap_or("a");
    internal_directory
        .join("objects")
        .join(format!("{index}-{stem}.o"))
}

fn compiler_config(
    args: &args::FileArgs,
    output: PathBuf,
    mode: CompilationMode,
) -> CompilerConfig {
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");
    let working_directory = invocation_directory.clone();
    let internal_directory = setup_internal_directory(&working_directory);

    compiler_config_with_dirs(args, output, mode, working_directory, internal_directory)
}

fn compiler_config_with_dirs(
    args: &args::FileArgs,
    output: PathBuf,
    mode: CompilationMode,
    working_directory: PathBuf,
    internal_directory: PathBuf,
) -> CompilerConfig {
    let include_dirs = args
        .include_dirs
        .iter()
        .map(|path| resolve_invocation_path(&working_directory, path))
        .collect();

    CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend: args.backend,
        optimization_level: args.optimization_level,
        output,
        unsafe_mode: args.unsafe_mode,
        verbose: args.verbose,
        dump: args.dump,
        working_directory,
        internal_directory,
        compilation_mode: mode,
        include_dirs,
        predefined_macros: args.predefined_macros.clone(),
        require_explicit_return: args.require_explicit_return,

        module_mode: false,
        project_config: None,
        link_entries: vec![],
        native_objects: vec![],
    }
}

fn run_standard_compilation(config: CompilerConfig, path: &Path) -> Result<(), ()> {
    standard_compilation(config, path).map_err(|err| {
        err.print().expect("Failed to write error message");
    })
}

fn run_file_mode(args: args::FileArgs) -> Result<(), ()> {
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");

    if args.compile_only {
        for input_file in &args.input_files {
            let output = args
                .output_file
                .as_ref()
                .map(|output| resolve_invocation_path(&invocation_directory, output))
                .unwrap_or_else(|| default_object_output(&invocation_directory, input_file));
            let config = compiler_config(&args, output, CompilationMode::Object);
            run_standard_compilation(config, Path::new(input_file))?;
        }
        return Ok(());
    }

    let output = args
        .output_file
        .as_ref()
        .map(|output| resolve_invocation_path(&invocation_directory, output))
        .unwrap_or_else(|| invocation_directory.join("a.out"));

    if args.input_files.len() == 1 {
        let config = compiler_config(&args, output, CompilationMode::Executable);
        run_standard_compilation(config, Path::new(&args.input_files[0]))?;
        return Ok(());
    }

    let working_directory = invocation_directory.clone();
    let internal_directory = setup_internal_directory(&working_directory);
    let mut object_files = Vec::with_capacity(args.input_files.len());

    for (index, input_file) in args.input_files.iter().enumerate() {
        let object_output = intermediate_object_output(&internal_directory, index, input_file);
        let config = compiler_config_with_dirs(
            &args,
            object_output.clone(),
            CompilationMode::Object,
            working_directory.clone(),
            internal_directory.clone(),
        );

        run_standard_compilation(config, Path::new(input_file))?;
        object_files.push(object_output);
    }

    link_object_files(&output, &object_files).map_err(|err| {
        err.print().expect("Failed to write error message");
    })?;

    Ok(())
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
        Command::CompileFile(args) => {
            if run_file_mode(args).is_err() {
                std::process::exit(1);
            }
        }
        Command::Build(args) => {
            build_project(args);
        }
        Command::Run(args) => run_project(args),
        Command::Init(args) => init_project(args),
    }
}
