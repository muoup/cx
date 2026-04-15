use std::hash::{Hash, Hasher};

use cx_ast::data::CXFunctionPrototype;
use cx_util::identifier::CXIdent;
use speedy::{Readable, Writable};

use crate::mir::expression::MIRFunctionContract;
pub use crate::mir::r#type::{
    same_type, same_types, MIRFloatType, MIRIntegerType, MIRMoveAttributes, MIRType,
    MIRTypeContext, MIRTypeId, MIRTypeKind,
};

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
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

impl PartialEq for MIRFunctionPrototype {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && self.return_type == other.return_type
            && self.params == other.params
            && self.var_args == other.var_args
    }
}

impl Eq for MIRFunctionPrototype {}

impl Hash for MIRFunctionPrototype {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        self.return_type.hash(state);
        self.params.hash(state);
        self.var_args.hash(state);
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct MIRTemplateInput {
    pub args: Vec<MIRType>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, Readable, Writable)]
pub struct TemplateInfo {
    pub base_name: CXIdent,
    pub template_input: MIRTemplateInput,
}
