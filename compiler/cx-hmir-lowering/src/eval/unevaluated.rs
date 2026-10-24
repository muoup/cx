use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::catalogue::typecheck;
use cx_mir::{MIRFnPrototype, MIRFnSignature, MIRType, MIRTypeKind};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext, RuntimeContext, runtime::Emission},
    log::hmir_error,
    value::{HMIRValueKind, constant::HMIRConstant},
};

use super::{EvalResult, coerce, comptime, eval, flow::Evaluation, types};

pub(crate) fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    operand: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    let span = body.expr(operand).span();
    let locals = match context.context() {
        FnContext::Runtime(runtime) => runtime.locals().collect::<Vec<_>>(),
        FnContext::Comptime(comptime) => comptime.locals().collect::<Vec<_>>(),
    }
    .into_iter()
    .map(|(id, value)| (*id, value.clone()))
    .collect::<Vec<_>>();
    let frozen = comptime::frozen_inputs(context);
    let void = context
        .env_mut()
        .module_mut()
        .intern_type(MIRType::new(MIRTypeKind::Void));
    let prototype = MIRFnPrototype::new(
        MIRFnSignature::new(Vec::new(), void, false, false),
        LinkageMode::Static,
        CXIdent::new("__cx_unevaluated"),
        None,
    );
    let mut runtime = RuntimeContext::new(prototype);
    runtime.set_emission(Emission::Unevaluated);
    let evaluation = context.evaluation();
    let mut child = context.child(None, FnContext::Runtime(runtime), Some(evaluation));
    for (local, value) in locals {
        let frozen_type = frozen.iter().find(|(id, value)| {
            *id == local && matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Type(_)))
        });
        let value = if let Some((_, value)) = frozen_type {
            value.clone()
        } else if matches!(value.kind(), HMIRValueKind::Constant(constant) if !constant.holds_local_address())
        {
            value
        } else {
            let ty = child
                .env()
                .reference_inner(value.ty())
                .unwrap_or(value.ty());
            emit::scratch(&mut child, ty, span)?.with_ty(value.ty())
        };
        emit::storage::bind_local(&mut child, local, value);
    }
    child.runtime_mut().set_captures(frozen);
    emit::open_scope(&mut child, span.clone());
    let result = eval(&mut child, body, operand)?;
    let span = span.clone();
    let result = result.and_then(&mut child, body, None, move |context, _, value, _| {
        let ty = context
            .env()
            .reference_inner(value.ty())
            .unwrap_or(value.ty());
        let ty = if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Type) {
            let value = coerce::std_rvalue(context, value, &span)?;
            match value.kind() {
                HMIRValueKind::Constant(HMIRConstant::Type(ty)) => *ty,
                _ => {
                    return Err(hmir_error(
                        &span,
                        &typecheck::EXPECTED_TYPE,
                        "an unknown type value".into(),
                    ));
                }
            }
        } else if let HMIRValueKind::Constant(HMIRConstant::Str(string)) = value.kind() {
            let element = types::int(context.env_mut(), HMIRIntWidth::I8, false, &span);
            context.env_mut().resolver_mut().realize_type(HMIRType::new(
                HMIRTypeKind::Array {
                    element,
                    length: Some((string.len() + 1) as u64),
                },
                Default::default(),
                span.clone(),
                None,
            ))
        } else {
            ty
        };
        context.prepare_type(ty, &span)?;
        Ok(Evaluation::Ready(ty))
    })?;
    Ok(match result {
        Evaluation::Incomplete(incomplete) => {
            Evaluation::Incomplete(incomplete.in_context(child.into_context()))
        }
        result => result,
    })
}
