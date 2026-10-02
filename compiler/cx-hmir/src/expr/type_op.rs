use cx_util::identifier::CXIdent;

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
    Const(HMIRExprID),
    PointerInner(HMIRExprID),
    ReferenceInner(HMIRExprID),
    Member {
        ty: HMIRExprID,
        name: CXIdent,
    },
    TypeOf(HMIRExprID),
    Decay(HMIRExprID),
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
        unsafe_move: bool,
        // A type whose move semantics the aggregate takes on top of its own
        traits_of: Option<HMIRExprID>,
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
            Self::Const(_) => "type.const",
            Self::PointerInner(_) => "type.pointer_inner",
            Self::ReferenceInner(_) => "type.reference_inner",
            Self::Member { .. } => "type.member",
            Self::TypeOf(_) => "type.type_of",
            Self::Decay(_) => "type.decay",
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
