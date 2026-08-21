use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::thir::{expression::THIRExpression, r#type::THIRType};

#[derive(Debug, Clone)]
pub struct THIRGlobalVariable {
    pub name: CXIdent,
    pub _type: THIRType,
    pub initializer: Option<THIRExpression>,
 
    pub linkage: LinkageMode,
    pub is_mutable: bool,
}
