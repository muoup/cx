use cx_hmir::{
    HMIRBody,
    expr::constant::HMIRConstant,
    ty::{HMIRTypeKind, context::HMIRTypeContext},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_mir::MIRConstant;
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, lowering::FnLoweringContext},
    lower::types::{lower_type, mir_float, mir_int},
};

pub(crate) fn lower_constant(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    constant: &HMIRConstant,
) -> CXResult<HMIRValue> {
    match constant {
        // Note: because lowering refers to runtime expressions, where comptime expressions are "evaluated", there
        // is not a need to handle HMIRConstant::Comptime here. A type reaching this path would indicate that we are
        // trying to use a type directly.
        //
        // In the future, we may want to introduce a runtime-friendly type info API, but for now, we will just error out.
        HMIRConstant::Type(_) => todo!("Error: unexpected type constant in runtime expression"),

        HMIRConstant::Unit => Ok(unit(context, span)),

        HMIRConstant::Str(_) => {
            let ty = context
                .env_mut()
                .resolver_mut()
                .realize_type(HMIRTypeKind::Str, span);

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

pub(crate) fn unit(context: &mut FnLoweringContext, span: &TokenRange) -> HMIRValue {
    let ty = context
        .env_mut()
        .resolver_mut()
        .realize_type(HMIRTypeKind::Void, span);

    HMIRValue::constant(ty, HMIRConstant::Unit)
}

pub(crate) fn mir_constant(
    env: &mut HMIREnvironment,
    constant: &HMIRConstant,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(match constant {
        HMIRConstant::Unit => MIRConstant::Unit,
        HMIRConstant::Int { value, ty } => match env.resolve_type_id(*ty).kind() {
            HMIRTypeKind::Int(int) => MIRConstant::Integer {
                ty: mir_int(int.width()),
                value: *value,
            },
            other => todo!("integer constant of type {other:?}"),
        },
        HMIRConstant::Float { value, ty } => match env.resolve_type_id(*ty).kind() {
            HMIRTypeKind::Float(float) => MIRConstant::Float {
                value: *value,
                ty: mir_float(float.width()),
            },
            other => todo!("float constant of type {other:?}"),
        },
        HMIRConstant::Null(ty) => MIRConstant::Nullptr {
            ty: lower_type(env, *ty, span)?,
        },
        HMIRConstant::Str(value) => MIRConstant::String(value.clone()),
        other => todo!("constant {other:?}"),
    })
}
