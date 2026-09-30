use crate::ids::HMIRMetaID;

#[derive(Debug, Clone)]
pub enum HMIRTypeOp {
    Pointer(HMIRMetaID),
    Reference(HMIRMetaID),
    Array {
        element: HMIRMetaID,
        length: Option<HMIRMetaID>,
    },
    Function {
        params: Vec<HMIRMetaID>,
        ret: HMIRMetaID,
        variadic: bool,
    },
    Expr {
        params: Vec<HMIRMetaID>,
        result: HMIRMetaID,
    },

    SizeOf(HMIRMetaID),
    AlignOf(HMIRMetaID),
    IsInt(HMIRMetaID),
    IsFloat(HMIRMetaID),
    IsPointer(HMIRMetaID),
    IsSigned(HMIRMetaID),
    Equal(HMIRMetaID, HMIRMetaID),
}

impl HMIRTypeOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Pointer(_) => "type.pointer",
            Self::Reference(_) => "type.reference",
            Self::Array { .. } => "type.array",
            Self::Function { .. } => "type.function",
            Self::Expr { .. } => "type.expr",
            Self::SizeOf(_) => "type.size_of",
            Self::AlignOf(_) => "type.align_of",
            Self::IsInt(_) => "type.is_int",
            Self::IsFloat(_) => "type.is_float",
            Self::IsPointer(_) => "type.is_pointer",
            Self::IsSigned(_) => "type.is_signed",
            Self::Equal(..) => "type.equal",
        }
    }
}
