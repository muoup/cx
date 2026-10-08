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
    run_command(Command::new(path).current_dir(working_directory))
}

pub fn run_command(command: &mut Command) -> Result<ExecutionResult, String> {
    let program = command.get_program().to_string_lossy().into_owned();
    let start = Instant::now();
    let output = command
        .output()
        .map_err(|error| format!("failed to run {program}: {error}"))?;
    let elapsed = start.elapsed();

    if output.status.code().is_none() {
        return Err(format!(
            "{program} terminated with {}:\n{}",
            output.status,
            String::from_utf8_lossy(&output.stderr),
        ));
    }

    Ok(ExecutionResult {
        status_code: output.status.code(),
        success: output.status.success(),
        stdout: String::from_utf8(output.stdout)
            .map_err(|_| format!("{program} stdout was not valid UTF-8"))?,
        stderr: String::from_utf8_lossy(&output.stderr).into_owned(),
        elapsed,
    })
}
