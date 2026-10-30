use crate::process::run_command;
use serde::Serialize;
use std::path::Path;
use std::process::Command;
use std::time::Duration;

/// The C compiler that cx is compared against.
#[derive(Serialize, Clone)]
pub struct ReferenceCompiler {
    pub command: String,
    pub version: String,
}

impl ReferenceCompiler {
    pub fn detect(command: &str) -> Result<Self, String> {
        let version = run_command(Command::new(command).arg("--version"))?;
        if !version.success {
            return Err(format!("{command} --version failed:\n{}", version.stderr));
        }

        Ok(Self {
            command: command.to_string(),
            version: version
                .stdout
                .lines()
                .next()
                .unwrap_or_default()
                .to_string(),
        })
    }

    pub fn command(&self) -> Command {
        let mut command = Command::new(&self.command);
        command.arg("-w");
        command
    }

    pub fn compile(
        &self,
        source: &Path,
        flags: &[&str],
        output: &Path,
    ) -> Result<Duration, String> {
        let compilation = run_command(
            self.command()
                .args(flags)
                .arg(source)
                .arg("-o")
                .arg(output)
                .arg("-lm"),
        )?;
        if !compilation.success {
            return Err(compilation.stderr);
        }
        Ok(compilation.elapsed)
    }
}
