use std::path::Path;
use std::process::Command;
use std::time::{Duration, Instant};

pub struct ExecutionResult {
    pub status_code: Option<i32>,
    pub success: bool,
    pub stdout: String,
    pub stderr: String,
    pub elapsed: Duration,
}

pub fn run_binary(path: &Path, working_directory: &Path) -> Result<ExecutionResult, String> {
    let start = Instant::now();
    let output = Command::new(path)
        .current_dir(working_directory)
        .output()
        .map_err(|error| format!("failed to run {}: {error}", path.display()))?;

    Ok(ExecutionResult {
        status_code: output.status.code(),
        success: output.status.success(),
        stdout: String::from_utf8(output.stdout)
            .map_err(|_| format!("{} stdout was not valid UTF-8", path.display()))?,
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        elapsed: start.elapsed(),
    })
}
