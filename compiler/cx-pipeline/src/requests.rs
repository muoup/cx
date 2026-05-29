use cx_ast::data::CXFunctionKind;
use cx_mir::mir::{data::MIRTemplateInput, r#type::MIRType};
use cx_pipeline_data::CompilationUnit;
use cx_typechecker::{
    environment::{MIRFunctionGenRequest, TypeEnvironment},
    realize_fn_implementation,
};
use cx_util::CXResult;


