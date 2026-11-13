use crate::pipeline_error;
use cx_log::CXResult;
use cx_log::catalogue::driver as catalogue;
use cx_pipeline_data::config::{LinkEntry, LinkKind};
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn link_relocatable(output: &Path, objects: &[PathBuf], roots: &[String]) -> CXResult<()> {
    let mut command = Command::new("ld");
    command.arg("-r").arg("--gc-sections");
    for root in roots {
        command.arg(format!("--undefined={root}"));
    }
    command.arg("-o").arg(output);
    command.args(objects);
    run(&mut command, "relocatable")?;

    let keep = output.with_extension("symbols");
    std::fs::write(&keep, roots.join("\n")).map_err(|error| {
        pipeline_error(
            &catalogue::FILE_OPERATION,
            (
                "write".into(),
                "library symbol list".into(),
                Some(format!("{}", keep.display())),
                format!("{}", error),
            ),
        )
    })?;
    let mut command = Command::new("objcopy");
    command.arg(format!("--keep-global-symbols={}", keep.display())).arg(output);
    let result = run(&mut command, "relocatable");
    let _ = std::fs::remove_file(&keep);
    result
}

pub(crate) fn link_executable(output: &Path, objects: &[PathBuf], link_entries: &[LinkEntry]) -> CXResult<()> {
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

fn run(command: &mut Command, kind: &str) -> CXResult<()> {
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
