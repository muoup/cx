use cx_hmir::{
    expr::literal::HMIRLiteral,
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics},
};
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    value::{HMIRValue, constant::HMIRConstant},
};

pub(crate) fn eval_literal(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    literal: &HMIRLiteral,
) -> HMIRValue {
    match literal {
        HMIRLiteral::Unit => unit(context, span),

        HMIRLiteral::Str(value) => {
            let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
                HMIRTypeKind::Str,
                HMIRTypeSemantics::default(),
                span.clone(),
                None,
            ));

            HMIRValue::constant(ty, HMIRConstant::Str(value.clone()))
        }

        HMIRLiteral::Int { value, ty } => HMIRValue::constant(
            *ty,
            HMIRConstant::Int {
                value: *value,
                ty: *ty,
            },
        ),
        HMIRLiteral::Float { value, ty } => HMIRValue::constant(
            *ty,
            HMIRConstant::Float {
                value: *value,
                ty: *ty,
            },
        ),
    }
}

pub(crate) fn unit(context: &mut LocalLoweringContext, span: &TokenRange) -> HMIRValue {
    let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Void,
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));

    HMIRValue::constant(ty, HMIRConstant::Unit)
}
