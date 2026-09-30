use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;

use crate::{
    function::HMIRFunction, global::HMIRGlobal, ids::HMIRDefID, type_def::HMIRTypeDef,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRDefRef {
    Local(HMIRDefID),
    External(QualifiedName),
}

#[derive(Debug, Clone)]
pub enum HMIRDefKind {
    Function(Box<HMIRFunction>),
    Global(Box<HMIRGlobal>),
    Type(Box<HMIRTypeDef>),
}

#[derive(Debug, Clone)]
pub struct HMIRDef {
    name: QualifiedName,
    span: TokenRange,
    kind: HMIRDefKind,
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
