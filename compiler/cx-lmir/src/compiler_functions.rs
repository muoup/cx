use cx_namespace::{
    mangling::mangle_namespace_symbol,
    module::{NamespacePath, QualifiedName},
};
use cx_target::ArchitectureConfig;
use cx_util::identifier::CXIdent;

use crate::{LMIRABISlot, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI, LMIRReturnABI, LinkageType, types::LMIRType};

#[derive(Debug, Clone, Copy)]
pub struct LMIRCompilerFunction {
    pub module_path: &'static str,
    pub name: &'static str,

    pub prototype_factory: fn(ArchitectureConfig) -> LMIRFunctionPrototype,
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

    prototype_factory: assertion_prototype,
};

pub const COMPILER_FUNCTIONS: &[LMIRCompilerFunction] = &[ASSERTION];

fn assertion_prototype(arch: ArchitectureConfig) -> LMIRFunctionPrototype {
    let pointer = LMIRType::default_pointer(&arch);
    
    LMIRFunctionPrototype {
        name: CXIdent::new(ASSERTION.symbol_name()),
        linkage: LinkageType::External,
        signature: LMIRFunctionSignature {
            return_type: LMIRType::unit(),
            return_abi: LMIRReturnABI::Void,
            params: vec![
                LMIRParameter {
                    name: Some(CXIdent::new("condition")),
                    _type: LMIRType::bool(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: LMIRType::bool(),
                            offset: 0,
                        }],
                    },
                },
                LMIRParameter {
                    name: Some(CXIdent::new("message")),
                    _type: pointer.clone(),
                    abi: LMIRParameterABI::Direct {
                        slots: vec![LMIRABISlot {
                            _type: pointer,
                            offset: 0,
                        }],
                    },
                },
            ],
            var_args: false,
        },
    }
}
