use cx_hir::ast::types::HIRTagKind;
use cx_hmir::{
    HMIRComptimeGlobal, HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind,
    HMIRFunction, HMIRGlobal,
    expr::HMIRExprKind,
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
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
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        resolver::{SymbolState, demand},
    },
    eval::{self, coerce, eval_type, flow, name::type_value, types},
    log::hmir_error,
    symbol,
    value::{
        HMIRValue,
        immediate::{Addressable, HMIRAddress, HMIRImmediate},
    },
};

pub(crate) fn value(context: &mut LocalLoweringContext, id: HMIRDefID) -> CXResult<HMIRValue> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let declaration = symbol::canonical(def);
    let span = declaration.span();
    match declaration.kind().expect("a canonical declaration has content") {
        HMIRDefKind::ComptimeGlobal(global)
            if let Some(tag) = declaration.tag()
                && let Some(declared) = aggregate(global) =>
        {
            tagged(context, id, declaration, tag, declared)
        }
        HMIRDefKind::Function(function) => function_value(context, id, function, span),
        HMIRDefKind::Global(_) => global_value(context, id, def, declaration),
        HMIRDefKind::ComptimeGlobal(global) => comptime_value(context, global, span),
    }
}

// The body of the aggregate 'id' declares, completing the type reserved for it
pub(crate) fn body(context: &mut LocalLoweringContext, id: HMIRDefID, ty: HMIRTypeID) -> CXResult<()> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let Some(HMIRDefKind::ComptimeGlobal(global)) = symbol::canonical(def).kind() else {
        unreachable!("an aggregate is declared by a type")
    };
    let declared = aggregate(global).expect("a pending type declares an aggregate");
    types::construct_type(context, global.body(), declared, Some(ty))?;
    Ok(())
}

fn aggregate(global: &HMIRComptimeGlobal) -> Option<&HMIRExprType> {
    let HMIRExprKind::Type(declared) = global.body().expr(global.initializer()).kind() else {
        return None;
    };
    matches!(
        declared.kind(),
        HMIRExprTypeKind::Struct { .. }
            | HMIRExprTypeKind::Union { .. }
            | HMIRExprTypeKind::TaggedUnion { .. }
            | HMIRExprTypeKind::Undefined(HMIRUndefined::Tagged(_))
    )
    .then_some(declared)
}

// A tagged aggregate is named before its body is evaluated, so that the body may refer to it
fn tagged(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    declaration: &HMIRDeclaration,
    tag: HIRTagKind,
    declared: &HMIRExprType,
) -> CXResult<HMIRValue> {
    let placeholder = HMIRType::new(
        HMIRTypeKind::Undefined(HMIRUndefined::Tagged(tag)),
        HMIRTypeSemantics::default(),
        declaration.span().clone(),
        declared.debug_name().map(str::to_owned),
    );
    let resolver = context.env_mut().resolver_mut();
    let ty = resolver.reserve_type(placeholder);
    if declaration.role() == HMIRDeclarationRole::Definition {
        resolver.set_pending(ty, Some(id));
    }
    type_value(context, ty, declaration.span())
}

fn function_value(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    function: &HMIRFunction,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let signature = flow::top(types::eval_signature(context, function))?;
    let ty = types::realize(context.env_mut(), HMIRTypeKind::Function(signature), span);
    let address = HMIRAddress::new(Addressable::Def(id), 0);
    Ok(HMIRValue::new(ty, HMIRImmediate::Address(address)))
}

fn global_value(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    def: &HMIRDef,
    declaration: &HMIRDeclaration,
) -> CXResult<HMIRValue> {
    let span = declaration.span();
    let mut merged = None;
    for other in def.declarations() {
        let Some(HMIRDefKind::Global(global)) = other.kind() else {
            continue;
        };
        let ty = global_type(context, id, global, span)?;
        merged = Some(match merged {
            None => ty,
            Some(previous) => composite(context.env(), previous, ty)
                .ok_or_else(|| symbol::incompatible(def, other))?,
        });
    }
    let mut ty = merged.expect("a global has a type");
    let linkage = symbol::linkage(context.env_mut(), id);
    let unbounded = matches!(
        context.env().resolve_kind(ty),
        HMIRTypeKind::Undefined(HMIRUndefined::Array { .. })
    );
    let Some(HMIRDefKind::Global(canonical)) = declaration.kind() else {
        unreachable!("a global is declared as one")
    };
    if unbounded && linkage != LinkageMode::Extern && canonical.initializer().is_none() {
        ty = types::array(context.env_mut(), ty, 1, span);
    }
    let tentative = declaration.role() == HMIRDeclarationRole::Tentative && unbounded;
    if !demand::complete(context.env_mut(), ty)? && !tentative && linkage != LinkageMode::Extern {
        let subject = format!("global of type '{}'", types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject));
    }
    Ok(address(context, id, ty, span))
}

// The type one declaration gives a global, completed from its initializer where the declared type is not
fn global_type(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    global: &HMIRGlobal,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let declared = flow::boundary(eval_type(context, global.body(), global.ty()))?;
    let known = declared.known().copied();
    if let Some(known) = known
        && (demand::complete(context.env_mut(), known)?
            || global.initializer().is_none() && declared.standing().is_some())
    {
        return Ok(known);
    }
    let Some(initializer) = global.initializer() else {
        return Err(match declared {
            flow::Evaluation::Incomplete(incomplete) => incomplete.error(),
            flow::Evaluation::Ready(_) => unreachable!("a ready type without an initializer stands"),
        });
    };
    if let Some(known) = known {
        let provisional = address(context, id, known, span);
        context
            .env_mut()
            .resolver_mut()
            .set_symbol(id, SymbolState::Evaluating(Some(provisional)));
    }
    let value = eval::eval_comptime(context, global.body(), initializer, known)?;
    flow::boundary(declared.complete(context, global.body(), Some(value.ty())))
}

fn address(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRValue {
    let reference = types::reference(context.env_mut(), ty, None, span);
    HMIRValue::new(
        reference,
        HMIRImmediate::Address(HMIRAddress::place(HMIRReference::Global(id))),
    )
}

// A type definition names the type it evaluates to; any other comptime global is its frozen initializer
fn comptime_value(
    context: &mut LocalLoweringContext,
    global: &HMIRComptimeGlobal,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let declared = flow::top(
        flow::boundary(eval_type(context, global.body(), global.ty()))?.description(),
    )?;
    if !matches!(context.env().resolve_kind(declared), HMIRTypeKind::Type) {
        return eval::eval_comptime(context, global.body(), global.initializer(), Some(declared));
    }
    let ty = flow::top(
        flow::boundary(eval_type(context, global.body(), global.initializer()))?.description(),
    )?;
    type_value(context, ty, span)
}

// Two declarations of a global agree on its type, though one may bound an array the other leaves unbounded
fn composite(env: &HMIREnvironment, a: HMIRTypeID, b: HMIRTypeID) -> Option<HMIRTypeID> {
    if types::alike(env, a, b, true) {
        return Some(a);
    }
    if env.semantics(a) != env.semantics(b) {
        return None;
    }
    let array = |ty| match *env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. } => Some((element, true)),
        HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => Some((element, false)),
        _ => None,
    };
    let ((left, left_bound), (right, right_bound)) = (array(a)?, array(b)?);
    match (left_bound, right_bound) {
        (false, true) if types::alike(env, left, right, true) => Some(b),
        (true, false) if types::alike(env, left, right, true) => Some(a),
        _ => None,
    }
}

// The value a storage global starts with: its initializer, or else zero
pub(crate) fn initialize(context: &mut LocalLoweringContext, id: HMIRDefID) -> CXResult<HMIRValue> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let declaration = symbol::canonical(def);
    let Some(HMIRDefKind::Global(global)) = declaration.kind() else {
        unreachable!("only storage globals have initial values");
    };
    let span = declaration.span();
    let mut child = context
        .child(None, FnContext::Comptime(ComptimeContext::new()), None)
        .sourced(id.brand());
    let declared = flow::boundary(eval_type(&mut child, global.body(), global.ty()))?;
    let Some(expr) = global.initializer() else {
        if global.linkage() == LinkageMode::Extern {
            let subject = format!("initial value of '{}'", def.name());
            return Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, subject));
        }
        let ty = flow::top(declared.description())?;
        let ty = match child.env().resolve_kind(ty) {
            HMIRTypeKind::Undefined(HMIRUndefined::Array { .. }) => {
                types::array(child.env_mut(), ty, 1, span)
            }
            _ => ty,
        };
        return zero(&mut child, ty, span).map(|value| HMIRValue::new(ty, value));
    };
    let value = eval::eval_comptime(&mut child, global.body(), expr, declared.known().copied())?;
    let ty = flow::boundary(declared.complete(&mut child, global.body(), Some(value.ty())))?;
    if !types::is_complete(child.env(), ty) {
        let subject = format!("global of type '{}'", types::name(child.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject));
    }
    coerce::implicit::standard(&mut child, value, ty, span)
}

pub(crate) fn zero(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRImmediate> {
    types::zero(context.env(), ty).ok_or_else(|| {
        let subject = format!("global of type '{}'", types::name(context.env(), ty));
        hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject)
    })
}
