use crate::pipeline_error;
use chl_log::CHLResult;
use chl_log::catalogue::driver as catalogue;
use chl_pipeline_data::config::{LinkEntry, LinkKind};
use crate::objects::{defined_globals, write_dependent_libraries};
use std::collections::BTreeSet;
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn link_relocatable(
    output: &Path,
    own: &[PathBuf],
    dependencies: &[PathBuf],
    links: &[LinkEntry],
) -> CHLResult<()> {
    let mut inputs = own.iter().chain(dependencies).cloned().collect::<Vec<_>>();
    let deplibs = output.with_extension("deplibs.o");
    if let (false, Some(first)) = (links.is_empty(), inputs.first()) {
        write_dependent_libraries(&deplibs, first, links)?;
        inputs.push(deplibs.clone());
    }

    let mut command = Command::new("ld");
    command.arg("-r").arg("-o").arg(output).args(&inputs);
    let result = run(&mut command, "relocatable");
    let _ = std::fs::remove_file(&deplibs);
    result?;

    let mut private = BTreeSet::new();
    for dependency in dependencies {
        private.extend(defined_globals(dependency)?);
    }
    if private.is_empty() {
        return Ok(());
    }
    let list = output.with_extension("private");
    std::fs::write(&list, private.into_iter().collect::<Vec<_>>().join("\n")).map_err(|error| {
        pipeline_error(
            &catalogue::FILE_OPERATION,
            (
                "write".into(),
                "private symbol list".into(),
                Some(format!("{}", list.display())),
                format!("{}", error),
            ),
        )
    })?;
    let mut command = Command::new("objcopy");
    command.arg(format!("--localize-symbols={}", list.display())).arg(output);
    let result = run(&mut command, "relocatable");
    let _ = std::fs::remove_file(&list);
    result
}

pub(crate) fn link_executable(output: &Path, objects: &[PathBuf], link_entries: &[LinkEntry]) -> CHLResult<()> {
    let mut command = Command::new("gcc");
    command.arg("-Wl,--gc-sections");
    command.arg("-o").arg(output);
    command.args(objects);

    for entry in link_entries {
        match entry.kind {
            LinkKind::System | LinkKind::Dynamic => {
                command.arg(format!("-l{}", entry.name));
            }
            LinkKind::Static => {
                command.arg("-Wl,-Bstatic");
                command.arg(format!("-l{}", entry.name));
                command.arg("-Wl,-Bdynamic");
            }
        }
    }

    run(&mut command, "executable")
}

fn run(command: &mut Command, kind: &str) -> CHLResult<()> {
    let output = command
        .output()
        .map_err(|error| pipeline_error(&catalogue::LINKER_EXECUTION, format!("{}", error)))?;
    match output.status.success() {
        true => Ok(()),
        false => Err(pipeline_error(
            &catalogue::LINKING_FAILED,
            (kind.into(), format!("{}", String::from_utf8_lossy(&output.stderr))),
        )),
    }
}
