use cx_hmir::{
    HMIRConstant, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth, HMIRTypeDesc,
};
use cx_tokens::{
    TokenRange,
    token::{FloatSuffix, IntegerBase, IntegerLength, IntegerSuffix},
};
use cx_util::unsafe_float::FloatWrapper;

use crate::body::BodyLowering;

impl BodyLowering<'_> {
    // C's literal typing: the first candidate that represents the magnitude, where
    // non-decimal literals may also take the unsigned type of each width
    pub(super) fn int_literal(
        &mut self,
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
        let ty = self.intern(HMIRTypeDesc::Int { width, signed });
        self.push(
            HMIRExprKind::Constant(HMIRConstant::Int {
                value: magnitude as i128,
                ty,
            }),
            span,
        )
    }

    pub(super) fn float_literal(
        &mut self,
        value: FloatWrapper,
        suffix: FloatSuffix,
        span: &TokenRange,
    ) -> HMIRExprID {
        let width = match suffix {
            FloatSuffix::Float => HMIRFloatWidth::F32,
            FloatSuffix::Default | FloatSuffix::LongDouble => HMIRFloatWidth::F64,
        };
        let ty = self.intern(HMIRTypeDesc::Float { width });
        self.push(HMIRExprKind::Constant(HMIRConstant::Float { value, ty }), span)
    }
}
