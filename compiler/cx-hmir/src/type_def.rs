use cx_util::identifier::CXIdent;

use crate::{
    binding::HMIRMetaLocalID,
    body::HMIRBody,
    expr::meta::HMIRMetaID,
    ty::nominal::{HMIRAggregateKind, HMIRMoveSemantics},
};

#[derive(Debug, Clone)]
pub struct HMIRFieldDef {
    name: Option<CXIdent>,
    ty: HMIRMetaID,
    bit_width: Option<usize>,
}

#[derive(Debug, Clone)]
pub enum HMIRTypeDefKind {
    Alias(HMIRMetaID),
    Aggregate {
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
        fields: Vec<HMIRFieldDef>,
    },
}

#[derive(Debug, Clone)]
pub struct HMIRTypeDef {
    body: HMIRBody,
    params: Vec<HMIRMetaLocalID>,
    kind: HMIRTypeDefKind,
}

impl HMIRFieldDef {
    pub fn new(name: Option<CXIdent>, ty: HMIRMetaID, bit_width: Option<usize>) -> Self {
        Self {
            name,
            ty,
            bit_width,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRMetaID {
        self.ty
    }

    pub fn bit_width(&self) -> Option<usize> {
        self.bit_width
    }
}

impl HMIRTypeDef {
    pub fn new(body: HMIRBody, params: Vec<HMIRMetaLocalID>, kind: HMIRTypeDefKind) -> Self {
        Self { body, params, kind }
    }

    pub fn body(&self) -> &HMIRBody {
        &self.body
    }

    pub fn params(&self) -> &[HMIRMetaLocalID] {
        &self.params
    }

    pub fn kind(&self) -> &HMIRTypeDefKind {
        &self.kind
    }
}
