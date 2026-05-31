use cx_ast::ast::function::CXFunctionKind;

use crate::mir::data::MIRTemplateInput;

pub mod intrinsic_types;
pub mod mir;
pub mod symbol;
pub mod program;
pub mod registry;

mod format;

pub struct MIRGenerationRequest {
    pub module_origin: Option<String>,
    pub kind: CXFunctionKind,
    pub input: MIRTemplateInput,
}