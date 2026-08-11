use cx_thir::thir::r#type::{THIRType, THIRTypeKind};

/// A semantic type carried into MIR from THIR.
///
/// MIR deliberately keeps the THIR type instead of introducing target layout or
/// calling-convention details. Those belong to later lowering stages.
#[derive(Debug, Clone)]
pub struct MIRType(pub THIRType);

impl MIRType {
    pub fn new(ty: THIRType) -> Self {
        Self(ty)
    }

    pub fn from_kind(kind: THIRTypeKind) -> Self {
        Self(kind.into())
    }

    pub fn as_thir(&self) -> &THIRType {
        &self.0
    }

    pub fn as_thir_mut(&mut self) -> &mut THIRType {
        &mut self.0
    }

    pub fn into_thir(self) -> THIRType {
        self.0
    }
}

impl Default for MIRType {
    fn default() -> Self {
        Self(THIRType::default())
    }
}

impl From<THIRType> for MIRType {
    fn from(value: THIRType) -> Self {
        Self(value)
    }
}

impl From<MIRType> for THIRType {
    fn from(value: MIRType) -> Self {
        value.0
    }
}
