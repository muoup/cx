use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::thir::{expression::THIRExpression, r#type::THIRType};

#[derive(Debug, Clone)]
pub struct THIRGlobalVariable {
    name: CXIdent,
    ty: THIRType,
    initializer: Option<THIRExpression>,

    linkage: LinkageMode,
    is_mutable: bool,
}

impl THIRGlobalVariable {
    pub fn new(
        name: CXIdent,
        ty: THIRType,
        initializer: Option<THIRExpression>,
        linkage: LinkageMode,
        is_mutable: bool,
    ) -> Self {
        Self {
            name,
            ty,
            initializer,
            linkage,
            is_mutable,
        }
    }

    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn ty(&self) -> &THIRType {
        &self.ty
    }

    pub fn initializer(&self) -> Option<&THIRExpression> {
        self.initializer.as_ref()
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }

    pub fn is_mutable(&self) -> bool {
        self.is_mutable
    }
}
