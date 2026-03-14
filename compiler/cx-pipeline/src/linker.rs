use crate::progress::ProgressReporter;
use cx_pipeline_data::GlobalCompilationContext;
use cx_util::{CXError, CXResult};
use std::process::Command;

pub(crate) fn link(context: &GlobalCompilationContext, reporter: &ProgressReporter) -> CXResult<()> {
    reporter.link_status("[Linking]");

    let mut cmd = Command::new("gcc");
    cmd.arg("-o")
        .arg(context.config.output.to_str().expect("Output path invalid"));

    let linking_files = context.linking_files.lock().unwrap();

    for file in linking_files.iter() {
        cmd.arg(file.to_str().expect("File path invalid"));
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
                return CXError::create_result(format!(
                    "Unknown link kind '{}' for library '{}'",
                    other, entry.name
                ));
            }
        }
    }

    let output = cmd.output()
        .map_err(|e| CXError::create_boxed(format!("Failed to execute linker: {}", e)))?;

    if output.status.success() {
        Ok(())
    } else {
        eprintln!(
            "[Linker] Failed to link files: {}",
            String::from_utf8_lossy(&output.stderr)
        );
        eprintln!("[Linker] Command: {cmd:?}");
        CXError::create_result(format!(
            "Linking failed: {}",
            String::from_utf8_lossy(&output.stderr)
        ))
    }
}
