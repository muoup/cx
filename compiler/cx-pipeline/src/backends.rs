use cx_data_mir::ProgramMIR;
use cx_data_pipeline::OptimizationLevel;

#[cfg(feature = "backend-llvm")]
pub(crate) fn llvm_compile(bytecode: &ProgramMIR, output: &str, optimization_level: OptimizationLevel) -> Option<Vec<u8>> {
    cx_backend_llvm::bytecode_aot_codegen(bytecode, output, optimization_level)
}

#[cfg(not(feature = "backend-llvm"))]
pub(crate) fn llvm_compile(_: &ProgramMIR, _: &str, _: OptimizationLevel) -> Option<Vec<u8>> {
    panic!("LLVM backend is not enabled. Please enable the `backend-llvm` feature to compile with its implementation.");
}

pub(crate) fn cranelift_compile(bytecode: &ProgramMIR, output: &str) -> Option<Vec<u8>> {
    cx_backend_cranelift::bytecode_aot_codegen(bytecode, output)
}