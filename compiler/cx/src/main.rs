mod args;
mod init;
mod build;

use args::Command;
use cx_pipeline::standard_compilation;
use cx_pipeline_data::{CompilationMode, CompilerConfig};
use std::path::{Path, PathBuf};

use crate::{build::run_build_mode, init::run_init_mode};

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
    let working_directory = invocation_directory.clone();
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
        compilation_mode: if args.compile_only {
            CompilationMode::Object
        } else {
            CompilationMode::Executable
        },
        module_mode: false,
        project_config: None,
        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
    };

    match standard_compilation(compiler_config, path) {
        Ok(_) => {}
        Err(err) => {
            println!();
            err.pretty_print();
            std::process::exit(1);
        }
    }
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
