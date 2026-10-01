use cx_hmir::{HMIRConstant, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth, HMIRTypeDesc};
use cx_tokens::{
    TokenRange,
    token::{FloatSuffix, IntegerBase, IntegerLength, IntegerSuffix},
};
use cx_util::unsafe_float::FloatWrapper;

use crate::body::BodyLowering;

// C's literal typing: the first candidate that represents the magnitude, where
// non-decimal literals may also take the unsigned type of each width
pub(crate) fn lower_int_literal(
    cx: &mut BodyLowering<'_>,
    magnitude: u64,
    base: IntegerBase,
    suffix: IntegerSuffix,
    span: &TokenRange,
) -> HMIRExprID {
    let start = match suffix.length {
        IntegerLength::Default => HMIRIntWidth::I32,
        _ => HMIRIntWidth::I64,
    };
    let decimal = base == IntegerBase::Decimal;
    let (width, signed) = [HMIRIntWidth::I32, HMIRIntWidth::I64]
        .into_iter()
        .filter(|width| *width >= start)
        .flat_map(|width| [(width, true), (width, false)])
        .filter(|(width, signed)| match signed {
            true => !suffix.unsigned,
            false => suffix.unsigned || !decimal || *width == HMIRIntWidth::I64,
        })
        .find(|(width, signed)| {
            let bits = if *width == HMIRIntWidth::I32 { 32 } else { 64 } - *signed as u32;
            bits == 64 || magnitude < 1u64 << bits
        })
        .unwrap_or((HMIRIntWidth::I64, false));
    cx.int_constant(HMIRTypeDesc::Int { width, signed }, magnitude as i128, span)
}

pub(crate) fn lower_float_literal(
    cx: &mut BodyLowering<'_>,
    value: FloatWrapper,
    suffix: FloatSuffix,
    span: &TokenRange,
) -> HMIRExprID {
    let width = match suffix {
        FloatSuffix::Float => HMIRFloatWidth::F32,
        FloatSuffix::Default | FloatSuffix::LongDouble => HMIRFloatWidth::F64,
    };
    let ty = cx.intern(HMIRTypeDesc::Float { width });
    cx.push(
        HMIRExprKind::Constant(HMIRConstant::Float { value, ty }),
        span,
    )
}
