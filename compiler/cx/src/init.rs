use std::path::Path;

use crate::args;

pub(crate) fn run_init_mode(args: args::InitArgs) {
    let project_dir = Path::new(&args.project_name);

    if project_dir.exists() {
        eprintln!("Error: Directory '{}' already exists.", args.project_name);
        std::process::exit(1);
    }

    std::fs::create_dir_all(project_dir).unwrap_or_else(|e| {
        eprintln!(
            "Error: Failed to create directory '{}': {}",
            args.project_name, e
        );
        std::process::exit(1);
    });

    let cx_toml = format!(
        r#"[project]
name = "{name}"

[build]
backend = "{backend}"
optimization = "O0"

[workspace.targets.default]
binaries = [
  {{ name = "{name}", entry = "main.cx" }},
]
"#,
        name = args.project_name,
        backend = args::default_backend_name(),
    );

    let main_cx = r#"import std::io;

i32 main() {
    println("Hello, world!");
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
