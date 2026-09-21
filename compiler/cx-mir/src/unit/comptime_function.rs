use cx_util::identifier::CXIdent;

use crate::{
    expr::comptime::MIRComptimeBody,
    ty::{MIRTypeID, comptime::MIRComptimeType},
    unit::function::MIRFunctionID,
};

#[derive(Debug, Clone)]
pub struct MIRComptimeFunction<'thir> {
    id: MIRFunctionID,
    prototype: MIRComptimeFnPrototype,
    body: Option<MIRComptimeBody<'thir>>,
}

impl<'thir> MIRComptimeFunction<'thir> {
    pub fn new(
        id: MIRFunctionID,
        prototype: MIRComptimeFnPrototype,
    ) -> Self {
        MIRComptimeFunction {
            id,
            prototype,
            body: None,
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn prototype(&self) -> &MIRComptimeFnPrototype {
        &self.prototype
    }

    pub fn body(&self) -> Option<&MIRComptimeBody<'thir>> {
        self.body.as_ref()
    }

    pub fn set_body(&mut self, body: MIRComptimeBody<'thir>) {
        self.body = Some(body);
    }
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub struct MIRComptimeContext {
    pub expected_return_type: Option<MIRTypeID>,
    pub expected_yield_type: Option<MIRTypeID>,
}

#[derive(Debug, Clone)]
pub struct MIRComptimeFnPrototype {
    name: CXIdent,
    signature: MIRComptimeFnSignature,
    context: MIRComptimeContext,
}

impl MIRComptimeFnPrototype {
    pub fn new(
        name: CXIdent,
        signature: MIRComptimeFnSignature,
        context: MIRComptimeContext,
    ) -> Self {
        Self {
            name,
            signature,
            context,
        }
    }

    pub fn name(&self) -> &CXIdent {
        &self.name
    }

    pub fn signature(&self) -> &MIRComptimeFnSignature {
        &self.signature
    }

    pub fn context(&self) -> MIRComptimeContext {
        self.context
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRComptimeFnSignature {
    return_type: MIRComptimeType,
    params: Vec<MIRComptimeFnParam>,
}

impl MIRComptimeFnSignature {
    pub fn new(return_type: MIRComptimeType, params: Vec<MIRComptimeFnParam>) -> Self {
        Self {
            return_type,
            params,
        }
    }

    pub fn return_type(&self) -> &MIRComptimeType {
        &self.return_type
    }

    pub fn params(&self) -> &[MIRComptimeFnParam] {
        &self.params
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRComptimeFnParam {
    pub name: Option<CXIdent>,
    pub ty: MIRComptimeType,
}
