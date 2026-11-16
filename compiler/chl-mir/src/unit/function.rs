use chl_util::{dense_id, identifier::CHLIdent, linkage::LinkageMode};

use crate::{ty::MIRTypeID, unit::body::MIRBody};

dense_id!(MIRFunctionID, "@f");

#[derive(Debug, Clone)]
pub struct MIRFunction {
    prototype: MIRFnPrototype,
    body: Option<MIRBody>,
}

impl MIRFunction {
    pub fn new(prototype: MIRFnPrototype, definition: Option<MIRBody>) -> Self {
        Self {
            prototype,
            body: definition,
        }
    }

    pub fn body(&self) -> Option<&MIRBody> {
        self.body.as_ref()
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRFnParam {
    name: Option<CHLIdent>,
    ty: MIRTypeID,
}

impl MIRFnParam {
    pub fn new(name: Option<CHLIdent>, ty: MIRTypeID) -> Self {
        Self { name, ty }
    }

    pub fn name(&self) -> Option<&CHLIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> MIRTypeID {
        self.ty
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MIRFnSignature {
    return_type: MIRTypeID,
    params: Vec<MIRFnParam>,
    variadic: bool,
    safe: bool,
}

impl MIRFnSignature {
    pub fn new(
        params: Vec<MIRFnParam>,
        return_type: MIRTypeID,
        variadic: bool,
        safe: bool,
    ) -> Self {
        Self {
            params,
            return_type,
            variadic,
            safe,
        }
    }

    pub fn return_type(&self) -> MIRTypeID {
        self.return_type
    }

    pub fn params(&self) -> &[MIRFnParam] {
        &self.params
    }

    pub fn variadic(&self) -> bool {
        self.variadic
    }

    pub fn safe(&self) -> bool {
        self.safe
    }
}

#[derive(Debug, Clone)]
pub struct MIRFnPrototype {
    pub signature: MIRFnSignature,
    pub linkage: LinkageMode,

    pub symbol_name: CHLIdent,
    pub debug_name: Option<CHLIdent>,
}

impl MIRFnPrototype {
    pub fn new(
        signature: MIRFnSignature,
        linkage: LinkageMode,
        symbol_name: CHLIdent,
        debug_name: Option<CHLIdent>,
    ) -> Self {
        Self {
            signature,
            linkage,
            symbol_name,
            debug_name,
        }
    }

    pub fn display_name(&self) -> &CHLIdent {
        self.debug_name.as_ref().unwrap_or(&self.symbol_name)
    }
}
