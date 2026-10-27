use cx_hmir::{
    HMIRBody,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
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

use crate::eval::{EvalResult, coerce, comptime, eval, flow::{EvalExit, Evaluation, RuntimeStop, StopReason}, types};

// The type of an operand evaluated without emitting it; a diverging operand is typed unreachable
pub(crate) fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    operand: HMIRExprID,
) -> EvalResult<Evaluation<HMIRTypeID>> {
    let span = body.expr(operand).span();
    let mut child = detached(context, Emission::Unevaluated, false, span)?;
    let result = match eval(&mut child, body, operand) {
        Err(EvalExit::Stop(StopReason::Runtime(RuntimeStop::Diverged))) => {
            return Ok(Evaluation::Ready(types::realize(child.env_mut(), HMIRTypeKind::Unreachable, span)));
        }
        result => result?,
    };
    let span = span.clone();
    let result = result.map(&mut child, move |context, value| {
        let referent = context.env().referent(value.ty());
        let ty = match (context.env().resolve_kind(referent), value.kind()) {
            (HMIRTypeKind::Type, _) => coerce::std_rvalue(context, value, &span)?
                .as_type()
                .ok_or_else(|| hmir_error(&span, &typecheck::EXPECTED_TYPE, "an unknown type value".into()))?,
            (_, HMIRValueKind::Constant(HMIRConstant::Str(string))) => {
                let element = types::int(context.env_mut(), HMIRIntWidth::I8, false, &span);
                let length = Some((string.len() + 1) as u64);
                types::realize(context.env_mut(), HMIRTypeKind::Array { element, length }, &span)
            }
            _ => referent,
        };
        context.prepare_type(ty, &span)?;
        Ok(ty)
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
    let frozen = comptime::inputs(context);
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
            Some(known) if known.as_type().is_some() || known == &value =>
            {
                shadow(&mut child, known.clone(), span)?
            }
            _ => {
                let ty = child.env().referent(value.ty());
                let storage = emit::scratch(&mut child, ty, span)?;
                if let HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) = storage.kind() {
                    child.runtime_mut().slot_mut(*slot).set_known(known.cloned());
                }
                storage.with_ty(value.ty())
            }
        };
        emit::bind_local(&mut child, local, value);
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
    let ty = context.env().referent(value.ty());
    Ok(emit::scratch(context, ty, span)?.with_ty(value.ty()))
}
