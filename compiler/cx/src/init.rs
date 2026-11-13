use std::path::Path;

use crate::{args, log::error};
use cx_log::CXResult;
use cx_pipeline_data::config::CONFIG_FILE;

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

    let package = args
        .project_name
        .chars()
        .map(|character| match character.is_ascii_alphanumeric() {
            true => character,
            false => '_',
        })
        .collect::<String>();
    let package = match package.starts_with(|character: char| character.is_ascii_digit()) {
        true => format!("_{package}"),
        false => package,
    };
    let cx_lua = format!(
        "package(\"{package}\")\nstdlib(\"std\")\n\nlocal main = target(\"{name}\", \"executable\")\nmain:match(\"main.cx\")\n",
        name = args.project_name,
    );

    let main_cx = r#"import std::io as std;

i32 main() {
    std::println("Hello, world!");
    return 0;
}
"#;

    std::fs::write(project_dir.join(CONFIG_FILE), cx_lua)
        .map_err(|err| error(format!("failed to write {CONFIG_FILE}: {err}"), None))?;
    std::fs::write(project_dir.join("main.cx"), main_cx)
        .map_err(|err| error(format!("failed to write main.cx: {err}"), None))?;

    println!("Created project '{}'", args.project_name);
    println!("  {}/{CONFIG_FILE}", args.project_name);
    println!("  {}/main.cx", args.project_name);
    Ok(())
}
