use crate::{arg::IntrinsicArg, mapper::IntrinsicMapper};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntUnaryOp {
    Neg,
    LNot,
    BNot,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IntBinaryOp {
    Add,
    Sub,
    UMul,
    SMul,
    UDiv,
    SDiv,
    UMod,
    SMod,

    Eq,
    Neq,
    ULt,
    SLt,
    ULe,
    SLe,
    UGt,
    SGt,
    UGe,
    SGe,

    LAnd,
    LOr,
    BAnd,
    BOr,
    BXor,

    LShift,
    ARShift,
    LRShift,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IntIntrinsic<V, T> {
    Unary {
        op: IntUnaryOp,
        value: V,
    },
    Binary {
        op: IntBinaryOp,
        lhs: V,
        rhs: V,
    },
    ToFloat {
        value: V,
        target: T,
        signed: bool,
    },
    Cast {
        value: V,
        target: T,
        sign_extend: bool,
    },
    ToPtr {
        value: V,
        sign_extend: bool,
    },
}

impl IntUnaryOp {
    pub fn path(self) -> &'static str {
        match self {
            Self::Neg => "int.neg",
            Self::LNot => "int.l_not",
            Self::BNot => "int.b_not",
        }
    }
}

impl IntBinaryOp {
    pub fn path(self) -> &'static str {
        match self {
            Self::Add => "int.add",
            Self::Sub => "int.sub",
            Self::UMul => "int.u_mul",
            Self::SMul => "int.s_mul",
            Self::UDiv => "int.u_div",
            Self::SDiv => "int.s_div",
            Self::UMod => "int.u_mod",
            Self::SMod => "int.s_mod",
            Self::Eq => "int.eq",
            Self::Neq => "int.neq",
            Self::ULt => "int.u_lt",
            Self::SLt => "int.s_lt",
            Self::ULe => "int.u_le",
            Self::SLe => "int.s_le",
            Self::UGt => "int.u_gt",
            Self::SGt => "int.s_gt",
            Self::UGe => "int.u_ge",
            Self::SGe => "int.s_ge",
            Self::LAnd => "int.l_and",
            Self::LOr => "int.l_or",
            Self::BAnd => "int.b_and",
            Self::BOr => "int.b_or",
            Self::BXor => "int.b_xor",
            Self::LShift => "int.l_shift",
            Self::ARShift => "int.ar_shift",
            Self::LRShift => "int.lr_shift",
        }
    }
}

impl<V, T> IntIntrinsic<V, T> {
    pub fn path(&self) -> &'static str {
        match self {
            Self::Unary { op, .. } => op.path(),
            Self::Binary { op, .. } => op.path(),
            Self::ToFloat { signed: true, .. } => "int.to_float.signed",
            Self::ToFloat { signed: false, .. } => "int.to_float.unsigned",
            Self::Cast { .. } => "int.int_cast",
            Self::ToPtr {
                sign_extend: true, ..
            } => "int.to_ptr.signed",
            Self::ToPtr {
                sign_extend: false,
                ..
            } => "int.to_ptr.unsigned",
        }
    }

    pub fn args(&self) -> Vec<IntrinsicArg<'_, V, T>> {
        match self {
            Self::Unary { value, .. } | Self::ToPtr { value, .. } => {
                vec![IntrinsicArg::Value(value)]
            }
            Self::Binary { lhs, rhs, .. } => {
                vec![IntrinsicArg::Value(lhs), IntrinsicArg::Value(rhs)]
            }
            Self::ToFloat { value, target, .. } => {
                vec![IntrinsicArg::Value(value), IntrinsicArg::Type(target)]
            }
            Self::Cast {
                value,
                target,
                sign_extend,
            } => vec![
                IntrinsicArg::Value(value),
                IntrinsicArg::Type(target),
                IntrinsicArg::Flag(*sign_extend),
            ],
        }
    }

    pub(crate) fn try_map<M: IntrinsicMapper<V, T>>(
        &self,
        mapper: &mut M,
    ) -> Result<IntIntrinsic<M::Value, M::Type>, M::Error> {
        Ok(match self {
            Self::Unary { op, value } => IntIntrinsic::Unary {
                op: *op,
                value: mapper.value(value)?,
            },
            Self::Binary { op, lhs, rhs } => IntIntrinsic::Binary {
                op: *op,
                lhs: mapper.value(lhs)?,
                rhs: mapper.value(rhs)?,
            },
            Self::ToFloat {
                value,
                target,
                signed,
            } => IntIntrinsic::ToFloat {
                value: mapper.value(value)?,
                target: mapper.ty(target)?,
                signed: *signed,
            },
            Self::Cast {
                value,
                target,
                sign_extend,
            } => IntIntrinsic::Cast {
                value: mapper.value(value)?,
                target: mapper.ty(target)?,
                sign_extend: *sign_extend,
            },
            Self::ToPtr { value, sign_extend } => IntIntrinsic::ToPtr {
                value: mapper.value(value)?,
                sign_extend: *sign_extend,
            },
        })
    }
}
