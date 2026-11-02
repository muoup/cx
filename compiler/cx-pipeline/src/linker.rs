use crate::pipeline_error;
use crate::progress::ProgressReporter;
use cx_log::CXResult;
use cx_log::catalogue::driver as catalogue;
use cx_pipeline_data::GlobalCompilationContext;
use cx_pipeline_data::config::LinkKind;
use std::path::{Path, PathBuf};
use std::process::Command;

pub(crate) fn link_relocatable(
    context: &GlobalCompilationContext,
    roots: &[String],
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    reporter.link_status("Linking (relocatable)");

    let output = &context.config.output;
    let mut command = Command::new("ld");
    command.arg("-r").arg("--gc-sections");
    for root in roots {
        command.arg(format!("--undefined={root}"));
    }
    command.arg("-o").arg(output);
    command.args(context.linking_files.lock().unwrap().iter());
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

pub(crate) fn link(
    context: &GlobalCompilationContext,
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    reporter.link_status("Linking");

    let mut cmd = Command::new("gcc");
    cmd.arg("-Wl,--gc-sections");
    cmd.arg("-o")
        .arg(context.config.output.to_str().expect("Output path invalid"));

    let linking_files = context.linking_files.lock().unwrap();

    for file in linking_files.iter() {
        cmd.arg(file.to_str().expect("File path invalid"));
    }

    for file in &context.config.native_objects {
        cmd.arg(file);
    }

    for entry in &context.config.link_entries {
        match entry.kind {
            LinkKind::System | LinkKind::Dynamic => {
                cmd.arg(format!("-l{}", entry.name));
            }
            LinkKind::Static => {
                cmd.arg("-Wl,-Bstatic");
                cmd.arg(format!("-l{}", entry.name));
                cmd.arg("-Wl,-Bdynamic");
            }
        }
    }

    run(&mut cmd, "executable")
}

pub(crate) fn link_objects(output: &Path, object_files: &[PathBuf]) -> CXResult<()> {
    let mut cmd = Command::new("gcc");
    cmd.arg("-Wl,--gc-sections");
    cmd.arg("-o").arg(output);
    cmd.args(object_files);
    run(&mut cmd, "executable")
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
