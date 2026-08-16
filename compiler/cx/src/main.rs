mod args;

use cx_pipeline::standard_compilation;
use cx_pipeline_data::CompilerConfig;
use std::path::{Path, PathBuf};

fn main() {
    let args = match args::parse_args() {
        Ok(args) => args,
        Err(err) => {
            eprintln!("Error: {err}");
            std::process::exit(1);
        }
    };

    let path = Path::new(&args.input_file);
    let invocation_directory = std::env::current_dir().expect("Failed to get current directory");
    let working_directory = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
        .map(|parent| invocation_directory.join(parent))
        .unwrap_or_else(|| invocation_directory.clone());
    let file_name = path
        .file_name()
        .expect("Failed to get file name from path")
        .to_str()
        .expect("Failed to convert file name to string");

    let internal_directory = working_directory.join(".internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");
    std::fs::write(internal_directory.join("compiler-dump.data"), "")
        .expect("Failed to clear dump file");

    let output_file = PathBuf::from(&args.output_file);
    let output = if output_file.is_absolute() {
        output_file
    } else {
        working_directory.join(output_file)
    };

    let compiler_config = CompilerConfig {
        backend: args.backend,
        optimization_level: args.optimization_level,
        output,
        analysis: args.analysis,
        working_directory,
        internal_directory,
    };

    standard_compilation(compiler_config, PathBuf::from(file_name).as_path())
        .expect("Compilation failed");

    println!("Compilation complete!");
}
