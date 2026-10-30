use cx_test_support::{
    assert_stdout, backend_name, compile_file, expected_stdout, run_binary, run_command,
    CompilationFailure, CompilationMode, CompilerBackend, ReferenceCompiler, TestTempDir,
};

use std::path::Path;
use std::process::Command;
use std::sync::OnceLock;

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

    if failure.code.as_bytes().get(1) == Some(&b'X') {
        panic!(
            "\nExpected failure stage: {:?}\nGot an internal compiler error instead\n\n{}",
            expected_stage, failure.rendered
        );
    }

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
    if code.len() != 5 || !code[1..].bytes().all(|byte| byte.is_ascii_digit()) {
        return None;
    }
    match code.as_bytes()[0] {
        b'P' => Some(FailureStage::Parse),
        b'T' | b'M' => Some(FailureStage::Typecheck),
        b'A' => Some(FailureStage::Analysis),
        b'D' if matches!(code, "D0001" | "D0002" | "D0003" | "D0004") => {
            Some(FailureStage::Linking)
        }
        _ => None,
    }
}

struct Expectation {
    stdout: String,
    status: Option<i32>,
}

const REFERENCE_FLAGS: &[&str] = &[
    "-std=c99",
    "-O0",
    "-fsanitize=undefined,address",
    "-fno-sanitize-recover=all",
];

fn reference_compiler() -> &'static ReferenceCompiler {
    static REFERENCE: OnceLock<ReferenceCompiler> = OnceLock::new();
    REFERENCE.get_or_init(|| {
        let command = std::env::var("CX_TEST_CC").unwrap_or_else(|_| "cc".to_string());
        ReferenceCompiler::detect(&command).unwrap_or_else(|error| {
            panic!("C fixtures need a reference compiler (CX_TEST_CC): {error}")
        })
    })
}

fn expectation(input: &Path) -> Expectation {
    let stdout = expected_stdout(input)
        .unwrap_or_else(|error| panic!("Failed to parse {}: {error}", input.display()));
    if input.extension().is_some_and(|extension| extension == "c") {
        if stdout.is_some() {
            panic!(
                "{} hardcodes its stdout, but C fixtures are compared against the reference compiler",
                input.display()
            );
        }
        return reference_execution(input);
    }

    let stdout = stdout
        .unwrap_or_else(|| panic!("Missing inline or sidecar stdout for {}", input.display()));
    Expectation {
        stdout,
        status: None,
    }
}

fn reference_execution(input: &Path) -> Expectation {
    let reference = reference_compiler();
    let temp_dir = TestTempDir::new(&format!("{}-reference", test_label(input)));
    let output = temp_dir.path().join("case.out");
    if let Err(error) = reference.compile(input, REFERENCE_FLAGS, &output) {
        panic!(
            "{} rejected {}:\n{error}",
            reference.command,
            input.display()
        );
    }

    let mut command = Command::new(&output);
    command
        .current_dir(test_root().join("fixtures"))
        .env("ASAN_OPTIONS", "detect_leaks=0");
    let execution = run_command(&mut command)
        .unwrap_or_else(|error| panic!("Reference build of {} failed: {error}", input.display()));
    if execution.stderr.contains("Sanitizer") || execution.stderr.contains("runtime error:") {
        panic!(
            "{} has undefined behavior under {}:\n{}",
            input.display(),
            reference.command,
            execution.stderr
        );
    }

    Expectation {
        stdout: execution.stdout,
        status: execution.status_code,
    }
}

fn run_end_to_end_test(input: &Path, llvm_only: bool) {
    let expected = expectation(input);
    let test_label = test_label(input);
    let mut failures = Vec::new();

    if !llvm_only {
        if let Err(failure) =
            run_backend_end_to_end(input, &expected, &test_label, CompilerBackend::Cranelift)
        {
            failures.push(failure);
        }
    }

    if cfg!(feature = "backend-llvm") {
        if let Err(failure) =
            run_backend_end_to_end(input, &expected, &test_label, CompilerBackend::LLVM)
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
    expected: &Expectation,
    test_label: &str,
    backend: CompilerBackend,
) -> Result<(), String> {
    let backend_name = backend_name(backend);
    let temp_dir = TestTempDir::new(&format!("{test_label}-{backend_name}"));
    let compilation = compile_file(input, backend, CompilationMode::Executable, &temp_dir)
        .map_err(|failure| format!("{backend_name} compilation failed:\n{}", failure.rendered))?;
    let execution = run_binary(&compilation.output, &test_root().join("fixtures"))?;

    assert_stdout(&expected.stdout, &execution.stdout, backend_name)?;
    match expected.status {
        Some(status) if execution.status_code != Some(status) => Err(format!(
            "{backend_name} exit status mismatch:\nexpected: {status}\nactual: {:?}",
            execution.status_code
        )),
        _ => Ok(()),
    }
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
