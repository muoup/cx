use std::ptr;

use cx_hmir::{
    HMIRBody, HMIRDeclarationRole, HMIRDefID, HMIRDefKind, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    env::{
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        memo::{MemoContextID, MemoKey},
        resolver::HMIRResolver,
    },
    eval::{self, coerce, eval_type, flow, types},
    log::hmir_error,
    symbol::{
        composite::{alike, composite, signatures_alike},
        structure::{self, incompatible},
    },
    value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        address::HMIRAddress,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub(crate) fn definition(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    scope: MemoContextID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(id.brand())
        .expect("definition owner exists");
    let def = unit.resolve_def(id).unwrap();
    let selected = structure::select(def, context.visible_through())?;
    let declaration = &def.declarations()[selected];
    if declaration.tag().is_some() {
        let canonical = structure::select(def, None)?;
        let Some(HMIRDefKind::ComptimeGlobal(global)) = def.declarations()[canonical].kind() else {
            unreachable!("a tagged declaration has a type initializer");
        };
        let ty = context.deferred_type(global.initializer());
        if context.env().resolver().realized_type(ty).is_none() {
            let name = Some(def.name().to_string());
            context.env_mut().resolver_mut().forward_type(
                ty,
                HMIRTypeSemantics::default(),
                span,
                name,
            );
        }
        return eval::type_value(context, ty, span);
    }
    match declaration
        .kind()
        .expect("selected declaration has content")
    {
        HMIRDefKind::Function(function) => {
            if function.signature().stage() == HMIRFunctionStage::Comptime
                || structure::needs_specialization(function)
            {
                let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
                    HMIRTypeKind::FunctionItem(id),
                    HMIRTypeSemantics::default(),
                    span.clone(),
                    None,
                ));
                return Ok(HMIRValue::constant(
                    ty,
                    HMIRConstant::Function {
                        function: HMIRFunctionRef::Def(id),
                        args: Vec::new(),
                    },
                ));
            }
            let mut canonical = None;
            for (_, declaration) in structure::visible(def, context.visible_through()) {
                let Some(HMIRDefKind::Function(function)) = declaration.kind() else {
                    continue;
                };
                let signature = flow::top(types::eval_signature(
                    context,
                    function.body(),
                    function.signature(),
                ))?;
                if let Some(previous) = &canonical {
                    if !signatures_alike(context.env(), previous, &signature) {
                        return Err(incompatible(def, declaration));
                    }
                }
                if canonical.is_none() || ptr::eq(declaration, &def.declarations()[selected]) {
                    canonical = Some(signature);
                }
            }
            let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
                HMIRTypeKind::Function(canonical.unwrap()),
                HMIRTypeSemantics::default(),
                span.clone(),
                None,
            ));
            Ok(HMIRValue::constant(
                ty,
                HMIRConstant::Function {
                    function: HMIRFunctionRef::Def(id),
                    args: Vec::new(),
                },
            ))
        }
        HMIRDefKind::Global(_) => {
            let mut ty = None;
            for (_, declaration) in structure::visible(def, context.visible_through()) {
                let Some(HMIRDefKind::Global(global)) = declaration.kind() else {
                    continue;
                };
                let declared = eval_type(context, global.body(), global.ty())?;
                let known = declared.known().copied();
                let resolved = match known {
                    Some(known)
                        if types::is_complete(context.env(), known)
                            || global.initializer().is_none() =>
                    {
                        known
                    }
                    _ => {
                        let Some(initializer) = global.initializer() else {
                            return Err(match declared {
                                flow::Evaluation::Incomplete(incomplete) => incomplete.error(),
                                _ => unreachable!(),
                            });
                        };
                        let value = initializer_value(context, global.body(), initializer, known)?;
                        flow::top(declared.complete(context, global.body(), Some(value.ty())))?
                    }
                };
                ty = Some(match ty {
                    None => resolved,
                    Some(previous) => composite(context.env(), previous, resolved)
                        .ok_or_else(|| incompatible(def, declaration))?,
                });
            }
            let mut ty = ty.expect("global has a type expression");
            if context.visible_through().is_none()
                && types::unsized_array(context.env(), ty)
                && structure::reconcile_linkage(def, None)? != LinkageMode::Extern
            {
                if let Some(HMIRDefKind::Global(global)) = declaration.kind() {
                    if global.initializer().is_none() {
                        ty = types::array(context.env_mut(), ty, 1, span);
                    }
                }
            }
            if !context.complete_object(ty)
                && !(declaration.role() == HMIRDeclarationRole::Tentative
                    && types::unsized_array(context.env(), ty))
                && structure::reconcile_linkage(def, context.visible_through())?
                    != LinkageMode::Extern
            {
                return Err(hmir_error(
                    span,
                    &typecheck::INCOMPLETE_TYPE,
                    format!("global of type '{}'", types::name(context.env(), ty)),
                ));
            }
            let reference = types::reference(context.env_mut(), ty, span);
            Ok(HMIRValue::constant(
                reference,
                HMIRConstant::Address(HMIRAddress::place(HMIRPlace::Global(id))),
            ))
        }
        HMIRDefKind::ComptimeGlobal(global) => {
            let declared =
                flow::top(eval_type(context, global.body(), global.ty())?.description())?;
            let type_definition =
                matches!(context.env().resolve_kind(declared), HMIRTypeKind::Type);
            let value = if type_definition {
                let memo = context
                    .env_mut()
                    .resolver_mut()
                    .reserve(MemoKey::Definition(id, scope));
                let alias = HMIRResolver::type_id(memo);
                let target = context.deferred_type(global.initializer());
                context
                    .env_mut()
                    .resolver_mut()
                    .alias_type(alias, target, span);
                let identity = eval::type_value(context, alias, span)?;
                context.env_mut().resolver_mut().publish(memo, identity);
                let ty = flow::top(
                    eval_type(context, global.body(), global.initializer())?.description(),
                )?;
                eval::type_value(context, ty, span)?
            } else {
                initializer_value(context, global.body(), global.initializer(), Some(declared))?
            };
            for (_, other) in structure::visible(def, context.visible_through()) {
                if ptr::eq(other, declaration) || other.kind().is_none() {
                    continue;
                }
                let Some(HMIRDefKind::ComptimeGlobal(other_global)) = other.kind() else {
                    unreachable!();
                };
                let other_ty = flow::top(
                    eval_type(context, other_global.body(), other_global.initializer())?
                        .description(),
                )?;
                let HMIRValueKind::Constant(HMIRConstant::Type(ty)) = value.kind() else {
                    return Err(incompatible(def, other));
                };
                if !alike(context.env(), *ty, other_ty, true) {
                    return Err(incompatible(def, other));
                }
            }
            Ok(value)
        }
    }
}

pub(crate) fn initializer_value(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    expected: Option<HMIRTypeID>,
) -> CXResult<HMIRValue> {
    let evaluation = context.evaluation();
    let mut child = context
        .child(
            None,
            FnContext::Comptime(ComptimeContext::new()),
            Some(evaluation),
        )
        .at(body.visible_through());
    let scope = child.memo_context(body.visible_through(), expected, true);
    let memo = child
        .env_mut()
        .resolver_mut()
        .reserve(MemoKey::Expression(expr, scope));
    let value = flow::top(child.evaluate(memo, body.expr(expr).span()))?;
    flow::top(eval::comptime::export(
        &mut child,
        value,
        body.expr(expr).span(),
    ))
}

pub(crate) fn initialize(context: &mut LocalLoweringContext, id: HMIRDefID) -> CXResult<HMIRValue> {
    let unit = context
        .env_mut()
        .symbols_mut()
        .owner(id.brand())
        .expect("definition owner exists");
    let def = unit.resolve_def(id).unwrap();
    let selected = structure::select(def, None)?;
    let declaration = &def.declarations()[selected];
    let Some(HMIRDefKind::Global(global)) = declaration.kind() else {
        unreachable!("only storage globals have initial values");
    };
    let source = id.brand();
    let mut child = context
        .child(None, FnContext::Comptime(ComptimeContext::new()), None)
        .sourced(source)
        .at(global.body().visible_through());
    let declared = eval_type(&mut child, global.body(), global.ty())?;
    let value = match global.initializer() {
        Some(expr) => {
            initializer_value(&mut child, global.body(), expr, declared.known().copied())?
        }
        None => {
            let ty = flow::top(declared.description())?;
            if global.linkage() == LinkageMode::Extern {
                return Err(hmir_error(
                    declaration.span(),
                    &mir::COMPTIME_UNAVAILABLE,
                    format!("initial value of '{}'", def.name()),
                ));
            }
            let ty = if types::unsized_array(child.env(), ty) {
                types::array(child.env_mut(), ty, 1, declaration.span())
            } else {
                ty
            };
            return zero(&mut child, ty, declaration.span())
                .map(|value| HMIRValue::constant(ty, value));
        }
    };
    let ty = flow::top(declared.complete(&mut child, global.body(), Some(value.ty())))?;
    if !types::is_complete(child.env(), ty) {
        return Err(hmir_error(
            declaration.span(),
            &typecheck::INCOMPLETE_TYPE,
            format!("global of type '{}'", types::name(child.env(), ty)),
        ));
    }
    coerce::implicit(&mut child, value, ty, declaration.span())
}

pub(crate) fn zero(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    HMIRConstant::zero(context.env(), ty).ok_or_else(|| {
        hmir_error(
            span,
            &typecheck::INCOMPLETE_TYPE,
            format!("global of type '{}'", types::name(context.env(), ty)),
        )
    })
}
