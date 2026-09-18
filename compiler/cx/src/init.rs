use std::path::Path;

use crate::{args, log::error};
use cx_log::CXResult;

pub(crate) fn init_project(args: args::InitArgs) -> CXResult<()> {
    let project_dir = Path::new(&args.project_name);

    if project_dir.exists() {
        return Err(error(
            format!("directory '{}' already exists", args.project_name),
            None,
        ));
    }
    std::fs::create_dir_all(project_dir).map_err(|err| {
        error(
            format!("failed to create directory '{}': {err}", args.project_name),
            None,
        )
    })?;

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

    let main_cx = r#"import std::io as std;

i32 main() {
    std::println("Hello, world!");
    return 0;
}
"#;

    std::fs::write(project_dir.join("cx.toml"), cx_toml)
        .map_err(|err| error(format!("failed to write cx.toml: {err}"), None))?;
    std::fs::write(project_dir.join("main.cx"), main_cx)
        .map_err(|err| error(format!("failed to write main.cx: {err}"), None))?;

    println!("Created project '{}'", args.project_name);
    println!("  {}/cx.toml", args.project_name);
    println!("  {}/main.cx", args.project_name);
    Ok(())
}
