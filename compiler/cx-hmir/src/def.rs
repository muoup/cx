use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;
use cx_util::{dense_id, linkage::LinkageMode};

use crate::{
    body::HMIRBody,
    expr::{meta::HMIRMetaID, obj::HMIRObjID},
    function::HMIRFunction,
    type_def::HMIRTypeDef,
};

dense_id!(HMIRDefID, "def");

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

#[derive(Debug, Clone)]
pub struct HMIRGlobal {
    body: HMIRBody,
    ty: HMIRMetaID,
    initializer: Option<HMIRObjID>,
    mutable: bool,
    linkage: LinkageMode,
}

impl HMIRGlobal {
    pub fn new(
        body: HMIRBody,
        ty: HMIRMetaID,
        initializer: Option<HMIRObjID>,
        mutable: bool,
        linkage: LinkageMode,
    ) -> Self {
        Self {
            body,
            ty,
            initializer,
            mutable,
            linkage,
        }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn ty(&self) -> HMIRMetaID {
        self.ty
    }

    pub fn initializer(&self) -> Option<HMIRObjID> {
        self.initializer
    }

    pub fn is_mutable(&self) -> bool {
        self.mutable
    }

    pub fn linkage(&self) -> LinkageMode {
        self.linkage
    }
}
