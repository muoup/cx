use crate::case::{Case, Sources};
use crate::report::ReferenceCompiler;
use cx_test_support::{
    backend_name, compile_file_at, run_command, CompilationFailure, CompilationMode,
    CompilationResult, CompilerBackend, OptimizationLevel, ProjectBinary, TestTempDir,
};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::Duration;

/// The level single-file cases are built at; projects name their own in `cx.toml`.
const FILE_OPTIMIZATION: OptimizationLevel = OptimizationLevel::O2;

/// What builds a case: cx with one of its backends, or the C compiler it is compared against.
#[derive(Clone, Copy)]
pub enum Toolchain<'a> {
    Cx(CompilerBackend),
    Reference(&'a ReferenceCompiler),
}

pub struct Build {
    /// The binary, which does not exist for a case that is only compiled.
    pub output: PathBuf,
    pub elapsed: Duration,
}

impl ReferenceCompiler {
    pub fn detect(command: &str) -> Result<Self, String> {
        let version = run_command(Command::new(command).arg("--version"))?;
        if !version.success {
            return Err(format!("{command} --version failed:\n{}", version.stderr));
        }

        Ok(Self {
            command: command.to_string(),
            version: version.stdout.lines().next().unwrap_or_default().to_string(),
        })
    }
}

impl Toolchain<'_> {
    pub fn label(&self) -> String {
        match self {
            Toolchain::Cx(backend) => backend_name(*backend).to_string(),
            Toolchain::Reference(reference) => reference.command.clone(),
        }
    }

    pub fn is_reference(&self) -> bool {
        matches!(self, Toolchain::Reference(_))
    }

    pub fn builds(&self, case: &Case) -> bool {
        match self {
            Toolchain::Cx(backend) => case.backends.contains(backend),
            Toolchain::Reference(_) => match &case.sources {
                Sources::File(file) => is_c_source(file),
                Sources::Project { binary, .. } => {
                    binary.sources.iter().all(|source| is_c_source(source))
                }
            },
        }
    }

    pub fn build(&self, case: &Case, temp_dir: &TestTempDir) -> Result<Build, String> {
        match self {
            Toolchain::Cx(backend) => {
                let compilation = match &case.sources {
                    Sources::File(file) => compile_file_at(
                        file,
                        *backend,
                        FILE_OPTIMIZATION,
                        CompilationMode::Executable,
                        temp_dir,
                    ),
                    Sources::Project { binary, link } => {
                        let mode = if *link {
                            CompilationMode::Executable
                        } else {
                            CompilationMode::Object
                        };
                        binary.compile(*backend, mode, temp_dir)
                    }
                };
                let CompilationResult { output, elapsed } = compilation
                    .map_err(|failure: CompilationFailure| failure.rendered)?;
                Ok(Build { output, elapsed })
            }
            Toolchain::Reference(reference) => {
                let output = temp_dir.path().join("case.out");
                let mut command = Command::new(&reference.command);
                command.arg("-w");
                match &case.sources {
                    Sources::File(file) => {
                        command
                            .arg(optimization_flag(FILE_OPTIMIZATION))
                            .arg(file)
                            .arg("-o")
                            .arg(&output)
                            .arg("-lm");
                    }
                    Sources::Project { binary, link } => {
                        project_arguments(&mut command, binary, *link, &output);
                        // The objects of a compile-only build land in the working directory.
                        command.current_dir(temp_dir.path());
                    }
                }

                let compilation = run_command(&mut command)?;
                if !compilation.success {
                    return Err(compilation.stderr);
                }
                Ok(Build {
                    output,
                    elapsed: compilation.elapsed,
                })
            }
        }
    }
}

fn project_arguments(command: &mut Command, binary: &ProjectBinary, link: bool, output: &Path) {
    command.arg(optimization_flag(binary.optimization_level));
    for include_dir in &binary.include_dirs {
        command.arg("-I").arg(include_dir);
    }
    command.args(binary.sources.iter().map(|source| binary.root.join(source)));

    if !link {
        command.arg("-c");
        return;
    }
    command.args(&binary.native_objects).arg("-o").arg(output);
    for entry in &binary.link_entries {
        command.arg(format!("-l{}", entry.name));
    }
}

fn optimization_flag(level: OptimizationLevel) -> &'static str {
    match level {
        OptimizationLevel::O0 => "-O0",
        OptimizationLevel::O1 => "-O1",
        OptimizationLevel::O2 => "-O2",
        OptimizationLevel::O3 => "-O3",
        OptimizationLevel::Osize => "-Os",
        OptimizationLevel::Ofast => "-Ofast",
    }
}

fn is_c_source(path: &Path) -> bool {
    path.extension().is_some_and(|extension| extension == "c")
}
