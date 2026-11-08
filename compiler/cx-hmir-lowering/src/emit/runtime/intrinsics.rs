use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_intrinsics::VAIntrinsic;
use cx_log::CXResult;
use cx_mir::{MIRTarget, MIRVAIntrinsic};
use cx_tokens::TokenRange;

use crate::{
    emit::runtime,
    env::function::LocalLoweringContext,
    lower::types::lower_type,
    value::{HMIRValIntrinsic, HMIRValue},
};

fn kind(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeKind {
    context.env().resolve_kind(ty).clone()
}

pub fn intrinsic(
    context: &mut LocalLoweringContext,
    intrinsic: &HMIRValIntrinsic,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match intrinsic {
        HMIRValIntrinsic::VA(var) => variadic(context, var, span),

        _ => todo!(),
    }
}

pub fn variadic(
    context: &mut LocalLoweringContext,
    op: &VAIntrinsic<HMIRValue, HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let (instruction, result) = match op {
        VAIntrinsic::Start { list, last } => {
            let list = runtime::value(context, list, span)?;
            let last = runtime::value(context, last, span)?;

            (MIRVAIntrinsic::VaStart { list, last }, HMIRValue::unit())
        }
        VAIntrinsic::End { list } => {
            let list = runtime::value(context, list, span)?;

            (MIRVAIntrinsic::VaEnd { list }, HMIRValue::unit())
        }
        VAIntrinsic::Arg { list, ty } => {
            let list = runtime::value(context, list, span)?;

            let out = runtime::register(context, *ty, span)?;
            let mir_out = context.runtime().register(out);

            let mir_ty = lower_type(context.env_mut(), *ty, span)?;

            (
                MIRVAIntrinsic::VaArg {
                    out: MIRTarget::Register(mir_out),
                    list,
                    ty: mir_ty,
                },
                HMIRValue::new(*ty, out),
            )
        }
    };

    context
        .runtime_mut()
        .emit_intrinsic(instruction, span.clone());
    Ok(result)
}
