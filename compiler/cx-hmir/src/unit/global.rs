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