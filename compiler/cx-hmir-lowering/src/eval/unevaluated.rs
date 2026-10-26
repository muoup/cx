use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRType, HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRFnPrototype, MIRFnSignature, MIRType, MIRTypeKind};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit,
    env::function::{
        FnContext, LocalLoweringContext, RuntimeContext,
        runtime::{Emission, RuntimeScope},
    },
    log::hmir_error,
    value::{
        Bindings, HMIRPlace, HMIRValue, HMIRValueKind,
        constant::HMIRConstant,
        staged::{Quote, QuoteRef},
    },
};

use super::{EvalResult, coerce, comptime, eval, flow::Evaluation, types};

pub(crate) fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    operand: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    let span = body.expr(operand).span();
    let mut child = detached(context, Emission::Unevaluated, false, span)?;
    let result = eval(&mut child, body, operand);
    let result = super::operand(&mut child, result, span)?;
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

pub(crate) fn detached<'parent, 'global, 'hmir>(
    context: &'parent mut LocalLoweringContext<'_, 'global, 'hmir>,
    emission: Emission,
    deferred: bool,
    span: &TokenRange,
) -> CXResult<LocalLoweringContext<'parent, 'global, 'hmir>> {
    let locals = context.visible_locals();
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
    runtime.set_emission(emission);
    let evaluation = context.evaluation();
    let mut child = context.child(None, FnContext::Runtime(runtime), Some(evaluation));
    let scope = child.runtime_mut().builder_mut().add_scope(span.clone());
    for (local, value) in locals {
        let known = frozen
            .iter()
            .find_map(|(id, known)| (*id == local).then_some(known));
        let value = match known {
            Some(known)
                if matches!(known.kind(), HMIRValueKind::Constant(HMIRConstant::Type(_)))
                    || known == &value =>
            {
                shadow(&mut child, known.clone(), span)?
            }
            _ => {
                let ty = child
                    .env()
                    .reference_inner(value.ty())
                    .unwrap_or(value.ty());
                let storage = emit::scratch(&mut child, ty, span)?;
                if let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = storage.kind() {
                    child.runtime_mut().slot_mut(*slot).set_known(known.cloned());
                }
                storage.with_ty(value.ty())
            }
        };
        emit::storage::bind_local(&mut child, local, value);
    }
    let scopes = child.runtime_mut().scopes_mut();
    let parent = scopes.top();
    scopes.push(RuntimeScope::new(Some(scope), deferred), Some(parent), Bindings::new());

    Ok(child)
}

fn shadow(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if let HMIRValueKind::Constant(HMIRConstant::Quote(node)) = value.kind() {
        let quote = node.get();
        let mut captures = Bindings::new();
        for (local, captured) in quote.captures() {
            captures.insert(*local, shadow(context, captured.clone(), span)?);
        }
        let code = Quote::new(
            quote.source(),
            quote.root(),
            quote.params().to_vec(),
            captures,
            quote.visible_through(),
        );
        return Ok(HMIRValue::constant(value.ty(), HMIRConstant::Quote(QuoteRef::new(code))));
    }
    let frozen = matches!(
        value.kind(),
        HMIRValueKind::Constant(constant) if !constant.holds_local_address()
    );
    if frozen {
        return Ok(value);
    }
    let ty = context.env().reference_inner(value.ty()).unwrap_or(value.ty());
    Ok(emit::scratch(context, ty, span)?.with_ty(value.ty()))
}
