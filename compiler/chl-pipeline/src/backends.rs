use chl_lmir::LMIRUnit;
use chl_log::CHLResult;
use chl_pipeline_data::OptimizationLevel;

#[cfg(feature = "backend-llvm")]
pub(crate) fn llvm_compile(
    bytecode: &LMIRUnit,
    output: &str,
    optimization_level: OptimizationLevel,
) -> CHLResult<Vec<u8>> {
    chl_backend_llvm::lmir_aot_codegen(bytecode, output, optimization_level)
}

#[cfg(not(feature = "backend-llvm"))]
pub(crate) fn llvm_compile(_: &LMIRUnit, _: &str, _: OptimizationLevel) -> CHLResult<Vec<u8>> {
    panic!(
        "LLVM backend is not enabled. Please enable the `backend-llvm` feature to compile with its implementation."
    );
}

pub(crate) fn cranelift_compile(bytecode: &LMIRUnit, output: &str) -> CHLResult<Vec<u8>> {
    chl_backend_cranelift::lmir_aot_codegen(bytecode, output)
}
