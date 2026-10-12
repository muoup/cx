use cx_hmir::{
    expr::constant::HMIRConstant,
    ty::{HMIRType, HMIRTypeKind, aggregate::HMIRTypeSemantics},
    value::{HMIRValue, HMIRValueKind},
};
use cx_tokens::TokenRange;

use crate::{env::function::LocalLoweringContext, eval::flow::EvalResult};

pub(crate) fn eval_constant(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    constant: &HMIRConstant,
) -> EvalResult<HMIRValue> {
    match constant {
        // Note: because lowering refers to runtime expressions, where comptime expressions are "evaluated", there
        // is not a need to handle HMIRConstant::Comptime here. A type reaching this path would indicate that we are
        // trying to use a type directly.
        //
        // In the future, we may want to introduce a runtime-friendly type info API, but for now, we will just error out.
        HMIRConstant::Type(_) => todo!("Error: unexpected type constant in runtime expression"),

        HMIRConstant::Unit => Ok(unit(context, span)),

        HMIRConstant::Str(_) => {
            let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
                HMIRTypeKind::Str,
                HMIRTypeSemantics::default(),
                span.clone(),
                None,
            ));

            Ok(HMIRValue::new(
                ty,
                HMIRValueKind::Constant(constant.clone()),
            ))
        }

        HMIRConstant::Null(ty)
        | HMIRConstant::Aggregate { ty, .. }
        | HMIRConstant::Int { ty, .. }
        | HMIRConstant::Float { ty, .. }
        | HMIRConstant::Address { ty, .. } => Ok(HMIRValue::new(
            *ty,
            HMIRValueKind::Constant(constant.clone()),
        )),

        HMIRConstant::Function { .. } => todo!("function constants"),
        HMIRConstant::Quote(_) => todo!("quote constants"),
        HMIRConstant::LabelAddress { .. } => todo!("label address constants"),
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
