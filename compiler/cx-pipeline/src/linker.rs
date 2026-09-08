use crate::pipeline_error;
use crate::progress::ProgressReporter;
use cx_log::CXResult;
use cx_log::catalogue::driver as catalogue;
use cx_pipeline_data::GlobalCompilationContext;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Relocatable link: combines all .o files into a single .o (ld -r).
/// Uses --gc-sections to strip unreferenced functions, keeping only those
/// reachable from the exported symbols.
pub(crate) fn link_relocatable(
    context: &GlobalCompilationContext,
    exported_symbols: &[String],
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    reporter.link_status("[Linking (relocatable)]");

    let mut cmd = Command::new("ld");
    cmd.arg("-r");
    cmd.arg("--gc-sections");

    // Mark exported symbols as roots so --gc-sections doesn't strip them
    for sym in exported_symbols {
        cmd.arg(format!("--undefined={}", sym));
    }

    cmd.arg("-o")
        .arg(context.config.output.to_str().expect("Output path invalid"));

    let linking_files = context.linking_files.lock().unwrap();

    for file in linking_files.iter() {
        cmd.arg(file.to_str().expect("File path invalid"));
    }

    for file in &context.config.native_objects {
        cmd.arg(file);
    }

    let output = cmd
        .output()
        .map_err(|e| pipeline_error(&catalogue::FAILED_TO_EXECUTE_LINKER, format!("{}", e)))?;

    if output.status.success() {
        Ok(())
    } else {
        Err(pipeline_error(
            &catalogue::RELOCATABLE_LINKING_FAILED,
            format!("{}", String::from_utf8_lossy(&output.stderr)),
        ))
    }
}

pub(crate) fn link(
    context: &GlobalCompilationContext,
    reporter: &mut ProgressReporter,
) -> CXResult<()> {
    reporter.link_status("[Linking]");

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

    // Add link entries from config
    for entry in &context.config.link_entries {
        match entry.kind.as_str() {
            "system" => {
                cmd.arg(format!("-l{}", entry.name));
            }
            "static" => {
                cmd.arg("-Wl,-Bstatic");
                cmd.arg(format!("-l{}", entry.name));
                cmd.arg("-Wl,-Bdynamic");
            }
            "dynamic" => {
                cmd.arg(format!("-l{}", entry.name));
            }
            other => {
                return Err(pipeline_error(
                    &catalogue::UNKNOWN_LINK_KIND_FOR_LIBRARY,
                    (format!("{}", other), format!("{}", entry.name)),
                ));
            }
        }
    }

    let output = cmd
        .output()
        .map_err(|e| pipeline_error(&catalogue::FAILED_TO_EXECUTE_LINKER, format!("{}", e)))?;

    if output.status.success() {
        Ok(())
    } else {
        Err(pipeline_error(
            &catalogue::LINKING_FAILED,
            format!("{}", String::from_utf8_lossy(&output.stderr)),
        ))
    }
}

pub(crate) fn link_objects(output: &Path, object_files: &[PathBuf]) -> CXResult<()> {
    let mut cmd = Command::new("gcc");
    cmd.arg("-Wl,--gc-sections");
    cmd.arg("-o").arg(output);
    cmd.args(object_files);

    let command_output = cmd
        .output()
        .map_err(|e| pipeline_error(&catalogue::FAILED_TO_EXECUTE_LINKER, format!("{}", e)))?;

    if command_output.status.success() {
        Ok(())
    } else {
        Err(pipeline_error(
            &catalogue::LINKING_FAILED,
            format!("{}", String::from_utf8_lossy(&command_output.stderr)),
        ))
    }
}
