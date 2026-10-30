use crate::case::{Case, Sources};
use cx_test_support::{
    backend_name, compile_file_at, run_command, CompilationFailure, CompilationMode,
    CompilationResult, CompilerBackend, OptimizationLevel, ProjectBinary, ReferenceCompiler,
    TestTempDir,
};
use std::num::NonZeroUsize;
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::time::{Duration, Instant};

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
                let (binary, link) = match &case.sources {
                    Sources::File(file) => {
                        let flags = [optimization_flag(FILE_OPTIMIZATION)];
                        let elapsed = reference.compile(file, &flags, &output)?;
                        return Ok(Build { output, elapsed });
                    }
                    Sources::Project { binary, link } => (binary, *link),
                };

                let start = Instant::now();
                let objects = compile_objects(reference, binary, temp_dir.path())?;
                if link {
                    link_objects(reference, binary, &objects, &output)?;
                }
                Ok(Build {
                    output,
                    elapsed: start.elapsed(),
                })
            }
        }
    }
}

// Compiles each source in its own process, as many at once as cx runs jobs, so that the reference
// is built the way a parallel make would build it
fn compile_objects(
    reference: &ReferenceCompiler,
    binary: &ProjectBinary,
    directory: &Path,
) -> Result<Vec<PathBuf>, String> {
    let objects = (0..binary.sources.len())
        .map(|index| directory.join(format!("{index}.o")))
        .collect::<Vec<_>>();
    let workers = std::thread::available_parallelism().map_or(1, NonZeroUsize::get);
    let next = AtomicUsize::new(0);

    std::thread::scope(|scope| {
        let compile = || loop {
            let index = next.fetch_add(1, Ordering::Relaxed);
            let Some(source) = binary.sources.get(index) else {
                return Ok(());
            };
            let mut command = reference.command();
            command.arg(optimization_flag(binary.optimization_level));
            for include_dir in &binary.include_dirs {
                command.arg("-I").arg(include_dir);
            }
            command
                .arg("-c")
                .arg(binary.root.join(source))
                .arg("-o")
                .arg(&objects[index]);
            let compilation = run_command(&mut command)?;
            if !compilation.success {
                return Err(compilation.stderr);
            }
        };
        let handles = (0..workers)
            .map(|_| scope.spawn(compile))
            .collect::<Vec<_>>();
        handles
            .into_iter()
            .map(|handle| handle.join().expect("reference compile worker panicked"))
            .collect::<Result<(), String>>()
    })?;

    Ok(objects)
}

fn link_objects(
    reference: &ReferenceCompiler,
    binary: &ProjectBinary,
    objects: &[PathBuf],
    output: &Path,
) -> Result<(), String> {
    let mut command = reference.command();
    command
        .args(objects)
        .args(&binary.native_objects)
        .arg("-o")
        .arg(output);
    for entry in &binary.link_entries {
        command.arg(format!("-l{}", entry.name));
    }
    let linking = run_command(&mut command)?;
    if !linking.success {
        return Err(linking.stderr);
    }
    Ok(())
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
