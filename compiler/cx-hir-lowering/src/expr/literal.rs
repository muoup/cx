use cx_hmir::{
    HMIRConstant, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth, HMIRTypeDesc,
};
use cx_tokens::{
    TokenRange,
    token::{FloatSuffix, IntegerLength, IntegerSuffix},
};
use cx_util::unsafe_float::FloatWrapper;

use crate::body::BodyLowering;

impl BodyLowering<'_> {
    pub(super) fn int_literal(
        &mut self,
        magnitude: u64,
        suffix: IntegerSuffix,
        span: &TokenRange,
    ) -> HMIRExprID {
        let fits_word = if suffix.unsigned {
            magnitude <= u32::MAX as u64
        } else {
            magnitude <= i32::MAX as u64
        };
        let width = match suffix.length {
            IntegerLength::Default if fits_word => HMIRIntWidth::I32,
            _ => HMIRIntWidth::I64,
        };
        let ty = self.intern(HMIRTypeDesc::Int {
            width,
            signed: !suffix.unsigned,
        });
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
