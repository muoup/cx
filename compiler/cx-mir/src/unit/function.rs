use cx_util::{dense_id, identifier::CXIdent};

use crate::{expr::body::MIRBody, ty::MIRTypeID};

dense_id!(MIRFunctionID);

#[derive(Debug, Clone)]
pub struct MIRFunction {
    id: MIRFunctionID,
    prototype: MIRFnPrototype,
    body: Option<MIRBody>,
}

impl MIRFunction {
    pub fn new(id: MIRFunctionID, prototype: MIRFnPrototype, definition: Option<MIRBody>) -> Self {
        Self {
            id,
            prototype,
            body: definition,
        }
    }

    pub fn id(&self) -> MIRFunctionID {
        self.id
    }

    pub fn body(&self) -> Option<&MIRBody> {
        self.body.as_ref()
    }

    pub fn prototype(&self) -> &MIRFnPrototype {
        &self.prototype
    }

    pub fn define(&mut self, def: MIRBody) {
        assert!(
            self.body.is_none(),
            "Attempt to redefine function: {}",
            self.prototype().display_name()
        );

        self.body = Some(def);
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MIRFnParam {
    name: Option<CXIdent>,
    ty: MIRTypeID,
    nodrop: bool,
}

impl MIRFnParam {
    pub fn new(name: Option<CXIdent>, ty: MIRTypeID, nodrop: bool) -> Self {
        Self { name, ty, nodrop }
    }

    pub fn name(&self) -> Option<&CXIdent> {
        self.name.as_ref()
    }

    pub fn ty(&self) -> MIRTypeID {
        self.ty
    }

    pub fn nodrop(&self) -> bool {
        self.nodrop
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

    pub symbol_name: CXIdent,
    pub debug_name: Option<CXIdent>,
}

impl MIRFnPrototype {
    pub fn new(
        signature: MIRFnSignature,
        symbol_name: CXIdent,
        debug_name: Option<CXIdent>,
    ) -> Self {
        Self {
            signature,
            symbol_name,
            debug_name,
        }
    }

    pub fn display_name(&self) -> &CXIdent {
        self.debug_name.as_ref().unwrap_or(&self.symbol_name)
    }
}
