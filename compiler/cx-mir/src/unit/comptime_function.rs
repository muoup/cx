use cx_util::identifier::CXIdent;

use crate::{expr::body::MIRBody, ty::MIRTypeID, unit::function::MIRFunctionID};

pub struct MIRComptimeFunction {
    id: MIRFunctionID,
    prototype: MIRComptimeFnPrototype,
    body: MIRBody,
}

impl MIRComptimeFunction {
    pub fn new(id: MIRFunctionID, prototype: MIRComptimeFnPrototype, body: MIRBody) -> Self {
        MIRComptimeFunction {
            id,
            prototype,
            body,
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRComptimeFnPrototype {
        &self.prototype
    }

    pub fn body(&self) -> &MIRBody {
        &self.body
    }
}

pub struct MIRComptimeFnPrototype {
    name: CXIdent,
    signature: MIRComptimeFnSignature,
}

impl MIRComptimeFnPrototype {
    pub fn new(name: CXIdent, signature: MIRComptimeFnSignature) -> Self {
        Self { name, signature }
    }

    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn signature(&self) -> &MIRComptimeFnSignature {
        &self.signature
    }
}

pub struct MIRComptimeFnSignature {
    return_type: MIRTypeID,
    params: Vec<MIRComptimeFnParam>,
}

impl MIRComptimeFnSignature {
    pub fn new(return_type: MIRTypeID, params: Vec<MIRComptimeFnParam>) -> Self {
        Self { return_type, params }
    }

    pub fn return_type(&self) -> MIRTypeID {
        self.return_type
    }

    pub fn params(&self) -> &[MIRComptimeFnParam] {
        &self.params
    }
}

pub struct MIRComptimeFnParam {}