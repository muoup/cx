use cx_util::{
    identifier::CXIdent,
    namespace::{mangle_namespace_symbol, NamespacePath, QualifiedName},
};

/// Stable source identity for a runtime function referenced by compiler-generated LMIR.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LMIRCompilerFunction {
    pub module_path: &'static str,
    pub name: &'static str,
}

impl LMIRCompilerFunction {
    pub fn qualified_name(self) -> QualifiedName {
        QualifiedName::new(
            NamespacePath::from_scoped_path(self.module_path),
            CXIdent::new(self.name),
        )
    }

    /// Compiler runtime modules are imported modules, so their exported symbols
    /// use the standard namespace mangling scheme.
    pub fn symbol_name(self) -> String {
        mangle_namespace_symbol(&self.qualified_name())
    }
}

pub const ASSERTION: LMIRCompilerFunction = LMIRCompilerFunction {
    module_path: "std::intrinsic::assertion",
    name: "__compiler_assert",
};

pub const COMPILER_FUNCTIONS: &[LMIRCompilerFunction] = &[ASSERTION];
