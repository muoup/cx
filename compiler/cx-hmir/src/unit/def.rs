use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{dense_id, linkage::LinkageMode};

use crate::{
    HMIRTypeID,
    body::HMIRBody,
    expr::{meta::HMIRMetaID, obj::HMIRObjID},
    function::HMIRFunction,
    type_def::HMIRTypeDef,
};

dense_id!(HMIRDefID, "def");

#[derive(Debug, Clone)]
pub struct HMIRDef {
    name: QualifiedName,
    span: TokenRange,
    kind: HMIRDefKind,
}

#[derive(Debug, Clone)]
pub enum HMIRDefKind {
    Function(Box<HMIRFunction>),
    Global(Box<HMIRGlobal>),
    Type(Box<HMIRTypeID>),
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRDefRef {
    Local(HMIRDefID),
    External(QualifiedName),
}

impl HMIRDef {
    pub fn new(name: QualifiedName, span: TokenRange, kind: HMIRDefKind) -> Self {
        Self { name, span, kind }
    }

    pub fn name(&self) -> &QualifiedName {
        &self.name
    }

    pub fn span(&self) -> &TokenRange {
        &self.span
    }

    pub fn kind(&self) -> &HMIRDefKind {
        &self.kind
    }

    pub fn kind_mut(&mut self) -> &mut HMIRDefKind {
        &mut self.kind
    }
}
