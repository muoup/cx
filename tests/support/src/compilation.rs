use cx_log::CXResult;
use cx_pipeline::standard_compilation;
use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel,
};
use std::fmt::{Display, Formatter};
use std::path::{Path, PathBuf};
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::{Duration, Instant};

static TEMP_ID: AtomicU64 = AtomicU64::new(0);

pub struct TestTempDir {
    path: PathBuf,
}

impl TestTempDir {
    pub fn new(test_name: &str) -> Self {
        let unique_id = TEMP_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join("cx-test-suite").join(format!(
            "{}-{}-{}",
            sanitize_name(test_name),
            std::process::id(),
            unique_id
        ));

        std::fs::create_dir_all(&path).expect("failed to create temporary test directory");
        Self { path }
    }

    pub fn path(&self) -> &Path {
        &self.path
    }

    pub(crate) fn internal_directory(&self) -> PathBuf {
        let internal_directory = self.path.join("internal");
        std::fs::create_dir_all(&internal_directory).expect("failed to create internal directory");
        internal_directory
    }
}

impl Drop for TestTempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.path).ok();
    }
}

pub struct CompilationResult {
    pub output: PathBuf,
    pub elapsed: Duration,
}

pub struct CompilationFailure {
    pub code: String,
    pub message: String,
    pub rendered: String,
    pub elapsed: Duration,
}

impl Display for CompilationFailure {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.code, self.message)
    }
}

pub fn compiler_config(
    backend: CompilerBackend,
    output: PathBuf,
    working_directory: &Path,
    internal_directory: &Path,
    compilation_mode: CompilationMode,
) -> CompilerConfig {
    CompilerConfig {
        architecture: ArchitectureConfig::native(),
        backend,
        optimization_level: match backend {
            CompilerBackend::Cranelift => OptimizationLevel::O0,
            CompilerBackend::LLVM => OptimizationLevel::O1,
        },
        require_explicit_return: None,
        output,
        unsafe_mode: false,
        compilation_mode,
        verbose: false,
        dump: false,
        working_directory: working_directory.to_path_buf(),
        internal_directory: internal_directory.to_path_buf(),
        module_mode: true,
        project_config: None,
        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
        predefined_macros: vec![],
    }
}

pub fn compile_file(
    input: &Path,
    backend: CompilerBackend,
    compilation_mode: CompilationMode,
    temp_dir: &TestTempDir,
) -> Result<CompilationResult, CompilationFailure> {
    let config = file_config(input, backend, compilation_mode, temp_dir);
    timed_compilation(config, |config| {
        standard_compilation(config, base_file_name(input))
    })
}

/// [`compile_file`] at `optimization_level` instead of the backend's test default.
pub fn compile_file_at(
    input: &Path,
    backend: CompilerBackend,
    optimization_level: OptimizationLevel,
    compilation_mode: CompilationMode,
    temp_dir: &TestTempDir,
) -> Result<CompilationResult, CompilationFailure> {
    let mut config = file_config(input, backend, compilation_mode, temp_dir);
    config.optimization_level = optimization_level;
    timed_compilation(config, |config| {
        standard_compilation(config, base_file_name(input))
    })
}

fn file_config(
    input: &Path,
    backend: CompilerBackend,
    compilation_mode: CompilationMode,
    temp_dir: &TestTempDir,
) -> CompilerConfig {
    let working_directory = input
        .parent()
        .expect("test source should have a parent directory");
    compiler_config(
        backend,
        temp_dir.path().join("case.out"),
        working_directory,
        &temp_dir.internal_directory(),
        compilation_mode,
    )
}

pub(crate) fn timed_compilation(
    config: CompilerConfig,
    compile: impl FnOnce(CompilerConfig) -> CXResult<()>,
) -> Result<CompilationResult, CompilationFailure> {
    let output = config.output.clone();
    let start = Instant::now();

    match compile(config) {
        Ok(()) => Ok(CompilationResult {
            output,
            elapsed: start.elapsed(),
        }),
        Err(error) => {
            let mut rendered = Vec::new();
            error
                .output(&mut rendered)
                .expect("failed to render compiler error");

            Err(CompilationFailure {
                code: error.code(),
                message: error.message(),
                rendered: String::from_utf8_lossy(&rendered).into_owned(),
                elapsed: start.elapsed(),
            })
        }
    }
}

pub fn base_file_name(input: &Path) -> &Path {
    Path::new(
        input
            .file_name()
            .expect("missing file name for test case")
            .to_str()
            .expect("test file name was not valid UTF-8"),
    )
}

fn sanitize_name(name: &str) -> String {
    name.chars()
        .map(|character| {
            if character.is_ascii_alphanumeric() {
                character
            } else {
                '_'
            }
        })
        .collect()
}
