use cx_data_bytecode::ProgramBytecode;
use cx_data_pipeline::OptimizationLevel;

#[cfg(feature = "backend-llvm")]
pub(crate) fn llvm_compile(bytecode: &ProgramBytecode, output: &str, optimization_level: OptimizationLevel) -> Option<()> {
    cx_backend_llvm::bytecode_aot_codegen(bytecode, output, optimization_level)
}

#[cfg(not(feature = "backend-llvm"))]
pub(crate) fn llvm_compile(_: &ProgramBytecode, _: &str, _: OptimizationLevel) -> Option<()> {
    panic!("LLVM backend is not enabled. Please enable the `backend-llvm` feature to compile with its implementation.");
}

pub(crate) fn cranelift_compile(bytecode: &ProgramBytecode, output: &str) -> Option<()> {
    cx_backend_cranelift::bytecode_aot_codegen(bytecode, output)
}