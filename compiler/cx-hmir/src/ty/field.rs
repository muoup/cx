#[derive(Debug, Clone)]
pub struct HMIRFieldDef {
    name: Option<CXIdent>,
    ty: HMIRMetaID,
    bit_width: Option<usize>,
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
