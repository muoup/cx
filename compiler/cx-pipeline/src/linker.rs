use cx_pipeline_data::GlobalCompilationContext;
use cx_util::{CXError, CXResult};
use std::process::Command;

pub(crate) fn link(context: &GlobalCompilationContext) -> CXResult<()> {
    let mut cmd = Command::new("gcc");
    cmd.arg("-o")
        .arg(context.config.output.to_str().expect("Output path invalid"));

    let linking_files = context.linking_files.lock().unwrap();

    for file in linking_files.iter() {
        cmd.arg(file.to_str().expect("File path invalid"));
    }

    let output = cmd.output()
        .map_err(|e| CXError::create_boxed(format!("Failed to execute linker: {}", e)))?;

    if output.status.success() {
        println!(
            "[Linker] Successfully linked files into {}",
            context.config.output.display()
        );
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
