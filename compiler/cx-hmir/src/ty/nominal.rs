use cx_util::identifier::CXIdent;

use crate::{constant::HMIRConstant, def::HMIRDefRef, ids::HMIRTypeID};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRAggregateKind {
    Struct,
    Union,
    TaggedUnion,
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum HMIRMoveSemantics {
    #[default]
    POD,
    Nocopy,
    Nodrop,
}

#[derive(Debug, Clone)]
pub struct HMIRFieldDesc {
    name: Option<CXIdent>,
    ty: HMIRTypeID,
    bit_width: Option<usize>,
}

// Identity is (def, args); fields are filled in after interning so recursive types can refer to themselves.
#[derive(Debug, Clone)]
pub struct HMIRNominalDesc {
    def: HMIRDefRef,
    args: Vec<HMIRConstant>,
    kind: HMIRAggregateKind,
    semantics: HMIRMoveSemantics,
    fields: Vec<HMIRFieldDesc>,
}

impl HMIRFieldDesc {
    pub fn new(name: Option<CXIdent>, ty: HMIRTypeID, bit_width: Option<usize>) -> Self {
        Self {
            name,
            ty,
            bit_width,
        }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub fn bit_width(&self) -> Option<usize> {
        self.bit_width
    }
}

impl HMIRNominalDesc {
    pub fn new(
        def: HMIRDefRef,
        args: Vec<HMIRConstant>,
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
    ) -> Self {
        Self {
            def,
            args,
            kind,
            semantics,
            fields: Vec::new(),
        }
    }

    pub fn def(&self) -> &HMIRDefRef {
        &self.def
    }

    pub fn args(&self) -> &[HMIRConstant] {
        &self.args
    }

    pub fn kind(&self) -> HMIRAggregateKind {
        self.kind
    }

    pub fn semantics(&self) -> HMIRMoveSemantics {
        self.semantics
    }

    pub fn fields(&self) -> &[HMIRFieldDesc] {
        &self.fields
    }

    pub fn set_fields(&mut self, fields: Vec<HMIRFieldDesc>) {
        self.fields = fields;
    }
}
