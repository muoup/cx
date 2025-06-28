#[derive(Default, Debug, Copy, Clone)]
pub enum OptimizationLevel {
    #[default]
    O0,
    O1,
    O2,
    O3,
    Osize,
    Ofast,
}

#[cfg(feature = "backend-llvm")]
#[derive(Default, Debug, Clone, Copy)]
pub enum CompilerBackend {
    #[default]
    LLVM,
    Cranelift
}

#[cfg(not(feature = "backend-llvm"))]
#[derive(Default, Debug, Clone, Copy)]
pub enum CompilerBackend {
    #[default]
    Cranelift,
    LLVM
}