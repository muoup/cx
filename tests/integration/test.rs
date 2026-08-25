use cx_test_support::{
    assert_stdout, backend_name, compile_file, expected_stdout, run_binary, CompilationFailure,
    CompilationMode, CompilerBackend, TestTempDir,
};

use std::path::Path;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum FailureStage {
    Parse,
    Typecheck,
    Analysis,
    Linking,
}

fn expect_compile_success(input: &Path) {
    let test_label = test_label(input);
    let temp_dir = TestTempDir::new(&test_label);

    if let Err(failure) = compile_file(
        input,
        CompilerBackend::Cranelift,
        CompilationMode::Object,
        &temp_dir,
    ) {
        panic!(
            "Expected compilation success but got failure for {}:\n{}\n{}",
            input.display(),
            failure,
            failure.rendered
        );
    }
}

fn expect_failure(input: &Path, expected_stage: FailureStage) {
    let test_label = test_label(input);
    let temp_dir = TestTempDir::new(&test_label);
    let failure = match compile_file(
        input,
        CompilerBackend::Cranelift,
        CompilationMode::Object,
        &temp_dir,
    ) {
        Ok(_) => panic!(
            "Expected compilation failure but got success for {}",
            input.display()
        ),
        Err(failure) => failure,
    };

    let actual_stage = classify_failure_stage(&failure);
    if actual_stage != Some(expected_stage) {
        panic!(
            "\nExpected failure stage: {:?}\nActual failure stage: {:?}\n\n{}",
            expected_stage, actual_stage, failure.rendered
        );
    }
}

fn classify_failure_stage(failure: &CompilationFailure) -> Option<FailureStage> {
    let code = failure.code.as_str();
    let message = failure.message.as_str();

    if code.starts_with("PARSER ERROR") || message.starts_with("PARSER ERROR") {
        Some(FailureStage::Parse)
    } else if code.starts_with("TYPE ERROR")
        || code.starts_with("COMPTIME ERROR")
        || code.starts_with("CONST EVAL ERROR")
        || message.starts_with("TYPE ERROR")
        || message.starts_with("COMPTIME ERROR")
        || message.starts_with("CONST EVAL ERROR")
    {
        Some(FailureStage::Typecheck)
    } else if code.starts_with("ANALYSIS ERROR") || message.starts_with("ANALYSIS ERROR") {
        Some(FailureStage::Analysis)
    } else if code.contains("Linking failed") || message.contains("Linking failed") {
        Some(FailureStage::Linking)
    } else {
        None
    }
}

fn run_end_to_end_test(input: &Path) {
    let expected_output = expected_stdout(input)
        .unwrap_or_else(|error| panic!("Failed to parse {}: {error}", input.display()))
        .unwrap_or_else(|| panic!("Missing inline or sidecar stdout for {}", input.display()));
    let test_label = test_label(input);
    let mut failures = Vec::new();

    if let Err(failure) = run_backend_end_to_end(
        input,
        &expected_output,
        &test_label,
        CompilerBackend::Cranelift,
    ) {
        failures.push(failure);
    }

    if cfg!(feature = "backend-llvm") {
        if let Err(failure) =
            run_backend_end_to_end(input, &expected_output, &test_label, CompilerBackend::LLVM)
        {
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
    test_label: &str,
    backend: CompilerBackend,
) -> Result<(), String> {
    let backend_name = backend_name(backend);
    let temp_dir = TestTempDir::new(&format!("{test_label}-{backend_name}"));
    let compilation = compile_file(input, backend, CompilationMode::Executable, &temp_dir)
        .map_err(|failure| format!("{backend_name} compilation failed:\n{}", failure.rendered))?;
    let execution = run_binary(&compilation.output, &test_root().join("fixtures"))?;

    assert_stdout(expected_output, &execution.stdout, backend_name)
}

fn test_label(input: &Path) -> String {
    input
        .strip_prefix(test_root())
        .unwrap_or(input)
        .display()
        .to_string()
}

fn test_root() -> &'static Path {
    Path::new(env!("CARGO_MANIFEST_DIR"))
}

#[allow(dead_code)]
fn run_compile_only_test(input: &Path) {
    expect_compile_success(input);
}

#[allow(dead_code)]
fn run_parse_error_test(input: &Path) {
    expect_failure(input, FailureStage::Parse);
}

#[allow(dead_code)]
fn run_type_error_test(input: &Path) {
    expect_failure(input, FailureStage::Typecheck);
}

#[allow(dead_code)]
fn run_verifier_error_test(input: &Path) {
    expect_failure(input, FailureStage::Analysis);
}

include!(concat!(env!("OUT_DIR"), "/generated_tests.rs"));
