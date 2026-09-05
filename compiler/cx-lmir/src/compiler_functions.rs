use cx_namespace::{
    mangling::mangle_namespace_symbol,
    module::{NamespacePath, QualifiedName},
};
use cx_util::identifier::CXIdent;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LMIRCompilerFunction {
    pub module_path: &'static str,
    pub name: &'static str,
}

impl LMIRCompilerFunction {
    pub fn qualified_name(self) -> QualifiedName {
        QualifiedName::new(
            NamespacePath::from_str(self.module_path),
            CXIdent::new(self.name),
        )
    }

    pub fn symbol_name(self) -> String {
        mangle_namespace_symbol(&QualifiedName {
            namespace: NamespacePath::from_str(self.module_path),
            name: CXIdent::new(self.name),
        })
    }
}

pub const ASSERTION: LMIRCompilerFunction = LMIRCompilerFunction {
    module_path: "std::intrinsic::assertion",
    name: "__compiler_assert",
};

pub const COMPILER_FUNCTIONS: &[LMIRCompilerFunction] = &[ASSERTION];
