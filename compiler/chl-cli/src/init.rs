use std::path::Path;

use crate::{args, log::error};
use chl_log::CHLResult;
use chl_pipeline_data::config::CONFIG_FILE;

pub(crate) fn init_project(args: args::InitArgs) -> CHLResult<()> {
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
    let chl_lua = format!(
        "package(\"{package}\")\nstdlib(\"std\")\n\nlocal main = target(\"{name}\", \"executable\")\nmain:match(\"main.chl\")\n",
        name = args.project_name,
    );

    let main_chl = r#"import std::io as std;

i32 main() {
    std::println("Hello, world!");
    return 0;
}
"#;

    std::fs::write(project_dir.join(CONFIG_FILE), chl_lua)
        .map_err(|err| error(format!("failed to write {CONFIG_FILE}: {err}"), None))?;
    std::fs::write(project_dir.join("main.chl"), main_chl)
        .map_err(|err| error(format!("failed to write main.chl: {err}"), None))?;

    println!("Created project '{}'", args.project_name);
    println!("  {}/{CONFIG_FILE}", args.project_name);
    println!("  {}/main.chl", args.project_name);
    Ok(())
}
