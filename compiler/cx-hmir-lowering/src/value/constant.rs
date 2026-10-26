use std::iter;

use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_mir::MIRFunctionID;
use cx_util::unsafe_float::FloatWrapper;

use crate::value::{
    HMIRPlace, HMIRValue, HMIRValueKind,
    address::{Addressable, HMIRAddress},
    staged::QuoteRef,
};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum HMIRConstant {
    Unit,
    Int(i128),
    Float(FloatWrapper),
    Str(String),
    Null,
    Type(HMIRTypeID),
    Function(HMIRFunctionRef),
    Curried {
        callee: Box<HMIRValue>,
        args: Vec<HMIRValue>,
    },
    Quote(QuoteRef),
    Aggregate(Vec<HMIRConstant>),
    Union {
        member: usize,
        value: Box<HMIRConstant>,
    },
    TaggedUnion {
        variant: usize,
        payload: Box<HMIRConstant>,
    },
    Address(HMIRAddress),
    BlockAddress {
        function: MIRFunctionID,
        label: String,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum HMIRFunctionRef {
    Def(HMIRDefID),
    // The constructor of a variant of a concrete tagged union
    Constructor { ty: HMIRTypeID, variant: usize },
}

impl HMIRConstant {
    pub fn zero(env: &impl HMIRTypeContext, ty: HMIRTypeID) -> Option<Self> {
        Some(match env.resolve_kind(ty) {
            HMIRTypeKind::Int(_) => Self::Int(0),
            HMIRTypeKind::Float(_) => Self::Float(0.0.into()),
            HMIRTypeKind::PointerTo(_) => Self::Null,
            HMIRTypeKind::Void => Self::Unit,
            HMIRTypeKind::Array {
                element,
                length: Some(length),
            } => Self::Aggregate(vec![Self::zero(env, *element)?; *length as usize]),
            HMIRTypeKind::Struct { fields } => Self::Aggregate(
                fields
                    .iter()
                    .map(|field| Self::zero(env, *field.ty()))
                    .collect::<Option<_>>()?,
            ),
            HMIRTypeKind::Union { fields } => Self::Union {
                member: 0,
                value: Box::new(Self::zero(env, *fields.first()?.ty())?),
            },
            HMIRTypeKind::TaggedUnion { fields } => Self::TaggedUnion {
                variant: 0,
                payload: Box::new(Self::zero(env, *fields.first()?.ty())?),
            },
            _ => return None,
        })
    }

    pub fn is_zero(&self) -> bool {
        match self {
            Self::Unit | Self::Int(0) | Self::Null => true,
            Self::Float(value) => *value == 0.0.into(),
            Self::Aggregate(items) => items.iter().all(Self::is_zero),
            Self::Union { value, .. } => value.is_zero(),
            _ => false,
        }
    }

    pub fn holds_local_address(&self) -> bool {
        match self {
            Self::Address(address) => matches!(
                address.base(),
                Addressable::Place(HMIRPlace::Local(_) | HMIRPlace::Region(_))
            ),
            Self::Aggregate(items) => items.iter().any(Self::holds_local_address),
            Self::Union { value, .. } => value.holds_local_address(),
            Self::TaggedUnion { payload, .. } => payload.holds_local_address(),
            Self::Curried { callee, args } => iter::once(callee.as_ref()).chain(args).any(|value| {
                matches!(value.kind(), HMIRValueKind::Constant(constant) if constant.holds_local_address())
            }),
            _ => false,
        }
    }
}
