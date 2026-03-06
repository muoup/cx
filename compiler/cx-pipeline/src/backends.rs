use cx_lmir::LMIRUnit;
use cx_pipeline_data::OptimizationLevel;

#[cfg(feature = "backend-llvm")]
pub(crate) fn llvm_compile(
    bytecode: &LMIRUnit,
    output: &str,
    optimization_level: OptimizationLevel,
) -> Option<Vec<u8>> {
    cx_backend_llvm::lmir_aot_codegen(bytecode, output, optimization_level)
}

#[cfg(not(feature = "backend-llvm"))]
pub(crate) fn llvm_compile(_: &LMIRUnit, _: &str, _: OptimizationLevel) -> Option<Vec<u8>> {
    panic!(
        "LLVM backend is not enabled. Please enable the `backend-llvm` feature to compile with its implementation."
    );
}

pub(crate) fn cranelift_compile(bytecode: &LMIRUnit, output: &str) -> Option<Vec<u8>> {
    cx_backend_cranelift::lmir_aot_codegen(bytecode, output)
}
