mod args;

use std::path::Path;
use cx_exec_pipeline::debug_compile;

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
            std::env::set_current_dir(parent)
                .expect("Failed to set current directory");
        }
    }

    let file_name = path.file_name().unwrap().to_str().unwrap();

    std::fs::create_dir_all(".internal")
        .expect("Failed to create internal directory");
    std::fs::write(".internal/compiler-dump.data", "")
        .expect("Failed to clear dump file");

    debug_compile(file_name, &args.output_file, args.backend, args.optimization_level);

    println!("Compilation complete!");
}