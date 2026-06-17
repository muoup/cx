use cx_pipeline::standard_compilation;
use cx_pipeline_data::{CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel};

use std::io;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FailureStage {
    Parse,
    Typecheck,
    Analysis,
    Linking,
}

struct TestTempDir {
    path: PathBuf,
}

impl TestTempDir {
    fn new(test_name: &str) -> Self {
        let unique_id = TEMP_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir()
            .join("cx-end-to-end-tests")
            .join(format!(
                "{}-{}-{}",
                sanitize_name(test_name),
                std::process::id(),
                unique_id
            ));

        std::fs::create_dir_all(&path).expect("Failed to create temp test directory");
        Self { path }
    }

    fn path(&self) -> &Path {
        &self.path
    }
}

impl Drop for TestTempDir {
    fn drop(&mut self) {
        std::fs::remove_dir_all(&self.path).ok();
    }
}

fn sanitize_name(name: &str) -> String {
    name.chars()
        .map(|c| if c.is_ascii_alphanumeric() { c } else { '_' })
        .collect()
}

fn base_file_name(input: &Path) -> &Path {
    Path::new(
        input
            .file_name()
            .expect("Missing file name for test case")
            .to_str()
            .expect("Failed to convert test file name to string"),
    )
}

fn compiler_config(
    backend: CompilerBackend,
    output: PathBuf,
    working_directory: &Path,
    internal_directory: &Path,
    analysis: bool,
    compilation_mode: CompilationMode,
) -> CompilerConfig {
    CompilerConfig {
        backend,
        optimization_level: match backend {
            CompilerBackend::Cranelift => OptimizationLevel::O0,
            CompilerBackend::LLVM => OptimizationLevel::O1,
        },
        output,
        analysis,
        compilation_mode,

        verbose: false,
        working_directory: working_directory.to_path_buf(),
        internal_directory: internal_directory.to_path_buf(),
        module_mode: true,
        project_config: None,
        link_entries: vec![],
        native_objects: vec![],
        include_dirs: vec![],
    }
}

fn classify_failure_stage(message: &str) -> Option<FailureStage> {
    if message.starts_with("PARSER ERROR") {
        Some(FailureStage::Parse)
    } else if message.starts_with("TYPE ERROR") {
        Some(FailureStage::Typecheck)
    } else if message.starts_with("ANALYSIS ERROR") {
        Some(FailureStage::Analysis)
    } else if message.contains("Linking failed") {
        Some(FailureStage::Linking)
    } else {
        None
    }
}

fn expect_compile_success(input: &Path, analysis: bool) {
    let test_label = input
        .strip_prefix(test_root())
        .unwrap_or(input)
        .display()
        .to_string();
    let working_directory = input
        .parent()
        .expect("Test case should have a parent directory");
    let temp_dir = TestTempDir::new(&test_label);
    let internal_directory = temp_dir.path().join("internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");

    let config = compiler_config(
        CompilerBackend::Cranelift,
        temp_dir.path().join("case.out"),
        working_directory,
        &internal_directory,
        analysis,
        CompilationMode::Object,
    );

    standard_compilation(config, base_file_name(input)).unwrap_or_else(|err| {
        err.print(&mut io::stdout()).unwrap();

        panic!("Expected compilation success but got failure");
    });
}

fn expect_failure(input: &Path, analysis: bool, expected_stage: FailureStage) {
    let test_label = input
        .strip_prefix(test_root())
        .unwrap_or(input)
        .display()
        .to_string();
    let working_directory = input
        .parent()
        .expect("Test case should have a parent directory");
    let temp_dir = TestTempDir::new(&test_label);
    let internal_directory = temp_dir.path().join("internal");
    std::fs::create_dir_all(&internal_directory).expect("Failed to create internal directory");

    let config = compiler_config(
        CompilerBackend::Cranelift,
        temp_dir.path().join("case.out"),
        working_directory,
        &internal_directory,
        analysis,
        CompilationMode::Object,
    );

    let err = match standard_compilation(config, base_file_name(input)) {
        Ok(_) => panic!("Expected compilation failure but got success"),
        Err(err) => err,
    };

    let message = err.error_message();
    let actual_stage = classify_failure_stage(message.as_str());

    if actual_stage != Some(expected_stage) {
        err.print(&mut io::stdout()).unwrap();
        panic!(
            "\nExpected failure stage: {:?}\nActual failure stage: {:?}\n\n",
            expected_stage, actual_stage
        );
    }
}

fn run_binary(path: &Path) -> Result<String, String> {
    let output = Command::new(path)
        .output()
        .map_err(|_| format!("Failed to run output binary: {}", path.display()))?;

    String::from_utf8(output.stdout)
        .map_err(|_| "Executable output was not valid UTF-8".to_string())
}

fn test_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

#[allow(dead_code)]
fn run_end_to_end_test(input: &Path) {
    let expected_output = input.with_extension("cx-output");
    assert!(
        expected_output.exists(),
        "[{}] Missing expected output file",
        expected_output.display()
    );

    let expected_output =
        std::fs::read_to_string(&expected_output).expect("Failed to read expected output");
    let working_directory = input
        .parent()
        .expect("Test case should have a parent directory");
    let test_label = input
        .strip_prefix(test_root())
        .unwrap_or(input)
        .display()
        .to_string();

    let mut failures = Vec::new();

    if let Err(failure) = run_backend_end_to_end(
        input,
        &expected_output,
        working_directory,
        &test_label,
        CompilerBackend::Cranelift,
    ) {
        failures.push(failure);
    }

    if cfg!(feature = "backend-llvm") {
        if let Err(failure) = run_backend_end_to_end(
            input,
            &expected_output,
            working_directory,
            &test_label,
            CompilerBackend::LLVM,
        ) {
            failures.push(failure);
        }
    }

    if !failures.is_empty() {
        panic!(
            "End-to-end backend failures for {}:\n\n{}",
            input.display(),
            failures.join("\n\n")
        );
    }
}

fn run_backend_end_to_end(
    input: &Path,
    expected_output: &str,
    working_directory: &Path,
    test_label: &str,
    backend: CompilerBackend,
) -> Result<(), String> {
    let backend_name = match backend {
        CompilerBackend::Cranelift => "Cranelift",
        CompilerBackend::LLVM => "LLVM",
    };

    let temp = TestTempDir::new(&format!("{test_label}-{backend_name}"));
    let internal = temp.path().join("internal");
    std::fs::create_dir_all(&internal).expect("Failed to create internal directory");
    let output = temp.path().join("case.out");
    let config = compiler_config(
        backend,
        output.clone(),
        working_directory,
        &internal,
        false,
        CompilationMode::Executable,
    );

    if let Err(err) = standard_compilation(config, base_file_name(input)) {
        return Err(format!(
            "{backend_name} compilation failed:\n{}",
            err.error_message()
        ));
    }

    let actual_output =
        run_binary(&output).map_err(|err| format!("{backend_name} execution failed: {err}"))?;

    if expected_output != actual_output {
        return Err(format!(
            "{backend_name} output mismatch:\nexpected:\n{expected_output:?}\nactual:\n{actual_output:?}",
        ));
    }

    Ok(())
}

#[allow(dead_code)]
fn run_compile_only_test(input: &Path, analysis: bool) {
    expect_compile_success(input, analysis);
}

#[allow(dead_code)]
fn run_parse_error_test(input: &Path) {
    expect_failure(input, false, FailureStage::Parse);
}

#[allow(dead_code)]
fn run_type_error_test(input: &Path) {
    expect_failure(input, false, FailureStage::Typecheck);
}

#[allow(dead_code)]
fn run_verifier_error_test(input: &Path) {
    expect_failure(input, true, FailureStage::Analysis);
}

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
