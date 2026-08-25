mod compilation;
mod format;
mod process;

pub use compilation::{
    base_file_name, compile_file, compiler_config, CompilationFailure, CompilationResult,
    TestTempDir,
};
pub use cx_pipeline_data::{
    ArchitectureConfig, CompilationMode, CompilerBackend, CompilerConfig, OptimizationLevel,
};
pub use format::{expected_stdout, parse_file, FormatError, StdoutExpectation, TestSpec};
pub use process::{run_binary, ExecutionResult};

pub fn assert_stdout(expected: &str, actual: &str, label: &str) -> Result<(), String> {
    if expected == actual {
        return Ok(());
    }

    Err(format!(
        "{label} stdout mismatch:\nexpected:\n{expected:?}\nactual:\n{actual:?}"
    ))
}

pub fn backend_name(backend: CompilerBackend) -> &'static str {
    match backend {
        CompilerBackend::Cranelift => "cranelift",
        CompilerBackend::LLVM => "llvm",
    }
}
