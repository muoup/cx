mod args;
mod build;
mod init;

use args::Command;
use cx_pipeline::standard_compilation;
use cx_pipeline_data::{CompilationMode, CompilerConfig};
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

use crate::{build::run_build_mode, init::run_init_mode};

fn setup_internal_directory(working_directory: &Path) -> PathBuf {
    let internal_directory = working_directory.join(".internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");
    std::fs::write(internal_directory.join("compiler-dump.data"), "")
        .expect("Failed to clear dump file");
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
    CompilerConfig {
        backend: args.backend,
        optimization_level: args.optimization_level,
        output,
        analysis: args.analysis,
        verbose: args.verbose,
        working_directory,
        internal_directory,
        compilation_mode: mode,
        module_mode: false,
        project_config: None,
        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
    }
}

fn run_standard_compilation(config: CompilerConfig, path: &Path) {
    match standard_compilation(config, path) {
        Ok(_) => {}
        Err(err) => {
            println!();
            err.pretty_print();
            std::process::exit(1);
        }
    }
}

fn link_objects(output: &Path, objects: &[PathBuf]) {
    if let Some(parent) = output.parent() {
        std::fs::create_dir_all(parent).expect("Failed to create output directory");
    }

    let mut command = ProcessCommand::new("gcc");
    command.arg("-Wl,--gc-sections").arg("-o").arg(output);
    command.args(objects);

    let linker_output = command.output().expect("Failed to execute linker");

    if !linker_output.status.success() {
        eprintln!(
            "[Linker] Failed to link files: {}",
            String::from_utf8_lossy(&linker_output.stderr)
        );
        eprintln!("[Linker] Command: {command:?}");
        std::process::exit(1);
    }
}

fn run_file_mode(args: args::FileArgs) {
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");

    if args.compile_only {
        for input_file in &args.input_files {
            let output = args
                .output_file
                .as_ref()
                .map(|output| resolve_invocation_path(&invocation_directory, output))
                .unwrap_or_else(|| default_object_output(&invocation_directory, input_file));
            let config = compiler_config(&args, output, CompilationMode::Object);
            run_standard_compilation(config, Path::new(input_file));
        }
        return;
    }

    let output = args
        .output_file
        .as_ref()
        .map(|output| resolve_invocation_path(&invocation_directory, output))
        .unwrap_or_else(|| invocation_directory.join("a.out"));

    if args.input_files.len() == 1 {
        let config = compiler_config(&args, output, CompilationMode::Executable);
        run_standard_compilation(config, Path::new(&args.input_files[0]));
        return;
    }

    let working_directory = invocation_directory.clone();
    let internal_directory = setup_internal_directory(&working_directory);
    let mut objects = Vec::new();

    for (index, input_file) in args.input_files.iter().enumerate() {
        let object = intermediate_object_output(&internal_directory, index, input_file);
        let config = compiler_config_with_dirs(
            &args,
            object.clone(),
            CompilationMode::Object,
            working_directory.clone(),
            internal_directory.clone(),
        );

        run_standard_compilation(config, Path::new(input_file));
        objects.push(object);
    }

    link_objects(&output, &objects);
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
