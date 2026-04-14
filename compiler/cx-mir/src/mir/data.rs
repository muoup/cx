use std::hash::Hash;

use cx_ast::data::CXFunctionPrototype;
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::mir::expression::MIRFunctionContract;
use crate::mir::r#type::MIRType;

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRParameter {
    pub name: Option<CXIdent>,
    pub _type: MIRType,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct MIRFunctionPrototype {
    pub name: CXIdent,
    pub source_prototype: CXFunctionPrototype,
    pub return_type: MIRType,
    pub params: Vec<MIRParameter>,
    pub var_args: bool,
    pub contract: MIRFunctionContract,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct MIRTemplateInput {
    pub args: Vec<MIRType>,
}

#[derive(Debug, Clone, Readable, Writable)]
pub struct TemplateInstantiationInformation {
    pub base_name: CXIdent,
    pub template_input: MIRTemplateInput,
}