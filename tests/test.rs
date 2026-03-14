use cx_pipeline::standard_compilation;
use cx_pipeline_data::{CompilerBackend, CompilerConfig, OptimizationLevel};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::atomic::{AtomicU64, Ordering};

static TEMP_ID: AtomicU64 = AtomicU64::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FailureStage {
    Parse,
    Typecheck,
    Analysis,
}

struct TestTempDir {
    path: PathBuf,
}

impl TestTempDir {
    fn new(test_name: &str) -> Self {
        let unique_id = TEMP_ID.fetch_add(1, Ordering::Relaxed);
        let path = std::env::temp_dir().join("cx-end-to-end-tests").join(format!(
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
) -> CompilerConfig {
    CompilerConfig {
        backend,
        optimization_level: match backend {
            CompilerBackend::Cranelift => OptimizationLevel::O0,
            CompilerBackend::LLVM => OptimizationLevel::O1,
        },
        output,
        analysis,
        working_directory: working_directory.to_path_buf(),
        internal_directory: internal_directory.to_path_buf(),
    }
}

fn classify_failure_stage(message: &str) -> Option<FailureStage> {
    if message.starts_with("PARSER ERROR")
    {
        Some(FailureStage::Parse)
    } else if message.starts_with("TYPE ERROR")
    {
        Some(FailureStage::Typecheck)
    } else if message.starts_with("ANALYSIS ERROR")
    {
        Some(FailureStage::Analysis)
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
    );

    standard_compilation(config, base_file_name(input))
        .unwrap_or_else(|err| {
            err.pretty_print();
            std::process::exit(1);
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
    );

    let message = match standard_compilation(config, base_file_name(input)) {
        Ok(_) => panic!("Expected compilation failure but got success"),
        Err(err) => Some(err.error_message()),
    };

    let actual_stage = message.as_ref()
        .map(|msg| classify_failure_stage(msg))
        .flatten();
    
    if actual_stage != Some(expected_stage) {
        panic!(
            "\nExpected failure stage: {}\nActual failure stage: {}\n\nMessage: {}",
            format!("{:?}", expected_stage),
            actual_stage.map(|s| format!("{:?}", s)).unwrap_or("UNKNOWN STAGE".to_string()),
            message.unwrap_or("No error message".to_string())
        );
    }
}

fn run_binary(path: &Path) -> String {
    let output = Command::new(path)
        .output()
        .unwrap_or_else(|_| panic!("Failed to run output binary: {}", path.display()));
    String::from_utf8(output.stdout).expect("Executable output was not valid UTF-8")
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

    let cranelift_temp = TestTempDir::new(&format!("{test_label}-cranelift"));
    let cranelift_internal = cranelift_temp.path().join("internal");
    std::fs::create_dir_all(&cranelift_internal).expect("Failed to create internal directory");
    let cranelift_output = cranelift_temp.path().join("case.out");
    let cranelift_config = compiler_config(
        CompilerBackend::Cranelift,
        cranelift_output.clone(),
        working_directory,
        &cranelift_internal,
        false,
    );

    standard_compilation(cranelift_config, base_file_name(input))
        .unwrap_or_else(|err| {
            err.pretty_print();
            std::process::exit(1);
        });
    assert_eq!(
        expected_output,
        run_binary(&cranelift_output),
        "Cranelift output mismatch for {}",
        input.display()
    );

    if cfg!(feature = "backend-llvm") {
        let llvm_temp = TestTempDir::new(&format!("{test_label}-llvm"));
        let llvm_internal = llvm_temp.path().join("internal");
        std::fs::create_dir_all(&llvm_internal).expect("Failed to create internal directory");
        let llvm_output = llvm_temp.path().join("case.out");
        let llvm_config = compiler_config(
            CompilerBackend::LLVM,
            llvm_output.clone(),
            working_directory,
            &llvm_internal,
            false,
        );

        standard_compilation(llvm_config, base_file_name(input))
            .unwrap_or_else(|err| {
                err.pretty_print();
                std::process::exit(1);
            });
        assert_eq!(
            expected_output,
            run_binary(&llvm_output),
            "LLVM output mismatch for {}",
            input.display()
        );
    }
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
