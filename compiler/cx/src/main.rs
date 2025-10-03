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
    if let Some(parent) = path.parent() {
        if !parent.as_os_str().is_empty() {
            std::env::set_current_dir(parent).expect("Failed to set current directory");
        }
    }
    let file_name = path
        .file_name()
        .expect("Failed to get file name from path")
        .to_str()
        .expect("Failed to convert file name to string");

    std::fs::create_dir_all(".internal").expect("Failed to create internal directory");
    std::fs::write(".internal/compiler-dump.data", "").expect("Failed to clear dump file");

    let compiler_config = CompilerConfig {
        backend: args.backend,
        optimization_level: args.optimization_level,
        output: PathBuf::from(&args.output_file),
    };

    standard_compilation(compiler_config, PathBuf::from(file_name).as_path())
        .expect("Compilation failed");

    println!("Compilation complete!");
}
