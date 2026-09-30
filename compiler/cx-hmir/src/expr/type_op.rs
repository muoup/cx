use crate::{
    expr::kind::HMIRExprID,
    ty::{
        field::HMIRFieldDef,
        nominal::{HMIRAggregateKind, HMIRMoveSemantics},
    },
};

#[derive(Debug, Clone)]
pub enum HMIRTypeOp {
    Pointer(HMIRExprID),
    Reference(HMIRExprID),
    Array {
        element: HMIRExprID,
        length: Option<HMIRExprID>,
    },
    Function {
        params: Vec<HMIRExprID>,
        ret: HMIRExprID,
        variadic: bool,
    },
    Expr {
        params: Vec<HMIRExprID>,
        result: HMIRExprID,
    },
    Aggregate {
        kind: HMIRAggregateKind,
        semantics: HMIRMoveSemantics,
        fields: Vec<HMIRFieldDef>,
    },

    SizeOf(HMIRExprID),
    AlignOf(HMIRExprID),
    IsInt(HMIRExprID),
    IsFloat(HMIRExprID),
    IsPointer(HMIRExprID),
    IsSigned(HMIRExprID),
    Equal(HMIRExprID, HMIRExprID),
}

impl HMIRTypeOp {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Pointer(_) => "type.pointer",
            Self::Reference(_) => "type.reference",
            Self::Array { .. } => "type.array",
            Self::Function { .. } => "type.function",
            Self::Expr { .. } => "type.expr",
            Self::Aggregate { .. } => "type.aggregate",
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
