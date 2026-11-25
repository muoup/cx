use std::{ops::Deref, rc::Rc};

use chl_hir::ast::types::HIRTagKind;
use chl_hmir::{
    HMIRComptimeGlobal, HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind,
    HMIRFnSignature, HMIRFunction, HMIRGlobal,
    ty::{
        HMIRExprType, HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
};
use chl_log::{
    CHLResult,
    catalogue::{mir, typecheck},
    error::CHLError,
};
use chl_tokens::TokenRange;
use chl_util::linkage::LinkageMode;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        resolver::{SymbolState, demand},
        symbols::HMIRUnitHandle,
    },
    eval::{self, comptime, deduce, eval_type, flow, name::type_value, types},
    log::hmir_error,
    lower::declaration,
    value::{
        HMIRValue,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmValue,
    },
};

// The value 'def' names, evaluated the first time it is asked for
pub(crate) fn value(env: &mut HMIREnvironment, def: HMIRDefID) -> CHLResult<HMIRValue> {
    match env.resolver().symbol(def) {
        SymbolState::Ready(value) | SymbolState::Evaluating(Some(value)) => return Ok(value.clone()),
        SymbolState::Evaluating(None) => return Err(cycle(env, def)),
        SymbolState::Declared => {}
    }
    env.resolver_mut()
        .set_symbol(def, SymbolState::Evaluating(None));
    let result = evaluating(env, def, |context| evaluate(context, def));
    let state = match &result {
        Ok(value) => SymbolState::Ready(value.clone()),
        Err(_) => SymbolState::Declared,
    };
    env.resolver_mut().set_symbol(def, state);
    let value = result?;
    demand::drain(env)?;
    Ok(value)
}

// Evaluates the body of the type 'def' reserved as 'ty'
pub(crate) fn complete(env: &mut HMIREnvironment, def: HMIRDefID, ty: HMIRTypeID) -> CHLResult<()> {
    env.resolver_mut().set_pending(ty, None);
    evaluating(env, def, |context| body(context, def, ty))?;
    demand::drain(env)
}

pub(crate) fn signature(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
) -> CHLResult<HMIRFnSignature<HMIRTypeID>> {
    let value = value(env, def)?;
    let HMIRTypeKind::Function { signature, .. } = env.resolve_kind(env.referent(value.ty())) else {
        unreachable!("a function binding has a function type")
    };
    Ok(signature.clone())
}

pub(crate) fn value_type(env: &mut HMIREnvironment, def: HMIRDefID) -> CHLResult<HMIRTypeID> {
    let value = value(env, def)?;
    Ok(env.referent(value.ty()))
}

pub(crate) enum Function<'hmir> {
    Source(HMIRUnitHandle<'hmir>, HMIRDefID),
    Elaborated(Rc<HMIRFunction>),
}

impl Deref for Function<'_> {
    type Target = HMIRFunction;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Source(unit, def) => {
                declaration::function(unit.resolve_def(*def).expect("function exists"))
            }
            Self::Elaborated(function) => function,
        }
    }
}

pub(crate) fn function<'hmir>(
    env: &mut HMIREnvironment<'_, 'hmir>,
    def: HMIRDefID,
) -> CHLResult<Function<'hmir>> {
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("function owner exists");
    let source = Function::Source(unit, def);
    let expanded = match env.resolver().elaborated_functions.get(&def) {
        Some(expanded) => expanded.clone(),
        None => {
            let expanded = deduce::signature::elaborate(env, def, &source)?.map(Rc::new);
            env.resolver_mut()
                .elaborated_functions
                .insert(def, expanded.clone());
            expanded
        }
    };
    Ok(expanded.map_or(source, Function::Elaborated))
}

fn evaluating<T>(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> CHLResult<T>,
) -> CHLResult<T> {
    env.resolver_mut().enter(def);
    let result = {
        let mut context =
            LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
                .sourced(def.brand());
        evaluate(&mut context)
    };
    env.resolver_mut().leave();
    result
}

pub(crate) fn name(env: &HMIREnvironment, def: HMIRDefID) -> String {
    env.symbols().resolve_definition(def).name().to_string()
}

fn cycle(env: &HMIREnvironment, def: HMIRDefID) -> CHLError {
    let active = env.resolver().active();
    let start = active.iter().position(|active| *active == def).unwrap_or(0);
    let path = active[start..]
        .iter()
        .chain(std::iter::once(&def))
        .map(|id| name(env, *id))
        .collect::<Vec<_>>()
        .join(" -> ");
    let span = env.symbols().resolve_definition(def).declarations()[0].span();
    hmir_error(span, &mir::DEPENDENCY_CYCLE, path)
}

fn evaluate(context: &mut LocalLoweringContext, id: HMIRDefID) -> CHLResult<HMIRValue> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let declaration = declaration::canonical(def);
    let span = declaration.span();
    match declaration.kind().expect("a canonical declaration has content") {
        HMIRDefKind::ComptimeGlobal(global)
            if let Some(tag) = declaration.tag()
                && let Some(declared) = types::aggregate(global.body(), global.initializer()) =>
        {
            tagged(context, id, declaration, tag, declared.ty)
        }
        HMIRDefKind::Function(_) => {
            let function = function(context.env_mut(), id)?;
            function_value(context, id, &function, span)
        }
        HMIRDefKind::Global(_) => global_value(context, id, def, declaration),
        HMIRDefKind::ComptimeGlobal(global) => comptime_value(context, id, global, span),
    }
}

// The body of the aggregate 'id' declares, completing the type reserved for it
fn body(context: &mut LocalLoweringContext, id: HMIRDefID, ty: HMIRTypeID) -> CHLResult<()> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let Some(HMIRDefKind::ComptimeGlobal(global)) = declaration::canonical(def).kind() else {
        unreachable!("an aggregate is declared by a type")
    };
    let declared = types::aggregate(global.body(), global.initializer())
        .expect("a pending type declares an aggregate");
    types::define_aggregate(context, global.body(), declared, Some(ty))?;
    Ok(())
}

// A tagged aggregate is named before its body is evaluated, so that the body may refer to it
fn tagged(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    declaration: &HMIRDeclaration,
    tag: HIRTagKind,
    declared: &HMIRExprType,
) -> CHLResult<HMIRValue> {
    let placeholder = HMIRType::new(
        HMIRTypeKind::Undefined(HMIRUndefined::Forward(Some(tag))),
        HMIRTypeSemantics::default(),
        declaration.span().clone(),
        declared.debug_name().map(str::to_owned),
    );
    let name = declaration::type_name(context.unit(id.brand()).resolve_def(id).unwrap());
    let resolver = context.env_mut().resolver_mut();
    let ty = resolver.reserve_nominal(placeholder);
    resolver.name_type(ty, name);
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
) -> CHLResult<HMIRValue> {
    let signature = flow::top(types::eval_signature(context, function))?;
    let ty = types::callable(context.env_mut(), signature, span);
    let address = HMIRAddress::new(Addressable::Def(id), 0);
    Ok(HMIRValue::new(ty, HMIRImmValue::Address(address)))
}

fn global_value(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    def: &HMIRDef,
    declaration: &HMIRDeclaration,
) -> CHLResult<HMIRValue> {
    let span = declaration.span();
    let HMIRDefKind::Global(canonical) = declaration.kind().unwrap() else {
        unreachable!("a global is declared as one")
    };
    let replacement = if declaration.overwrites() {
        Some(global_type(context, id, canonical, span)?)
    } else {
        None
    };
    let mut merged = replacement;
    let mut original = None;
    for other in def.declarations() {
        if replacement.is_some() && std::ptr::eq(other, declaration) {
            continue;
        }
        let Some(HMIRDefKind::Global(global)) = other.kind() else {
            continue;
        };
        let ty = global_type(context, id, global, other.span())?;
        if let Some(replacement) = replacement {
            let env = context.env();
            let matches = if other.point() < declaration.point() {
                declaration::integer_types_alike(env, replacement, ty, true)
                    && types::alike(env, *original.get_or_insert(ty), ty, true)
            } else {
                types::alike(env, replacement, ty, true)
            };
            if !matches {
                return Err(declaration::incompatible(def, other));
            }
            continue;
        }
        merged = Some(match merged {
            None => ty,
            Some(previous) => composite(context.env(), previous, ty)
                .ok_or_else(|| declaration::incompatible(def, other))?,
        });
    }
    let mut ty = merged.expect("a global has a type");
    let linkage = declaration::linkage(def);
    let unbounded = matches!(
        context.env().resolve_kind(ty),
        HMIRTypeKind::Undefined(HMIRUndefined::Array { .. })
    );
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
) -> CHLResult<HMIRTypeID> {
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
    let inferred = match known {
        Some(_) => eval::eval_comptime(context, global.body(), initializer, known)?.ty(),
        None => flow::top(flow::boundary(crate::eval::deduce::typed(context, span, true, |child| {
            eval::eval(child, global.body(), initializer)
        }))?.description())?,
    };
    flow::boundary(declared.complete(context, global.body(), Some(inferred)))
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
        HMIRImmValue::Address(HMIRAddress::new(Addressable::Def(id), 0)),
    )
}

// A type definition names the type it evaluates to; any other comptime global is its frozen initializer
fn comptime_value(
    context: &mut LocalLoweringContext,
    id: HMIRDefID,
    global: &HMIRComptimeGlobal,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let declared = flow::top(
        flow::boundary(eval_type(context, global.body(), global.ty()))?.description(),
    )?;
    if !matches!(context.env().resolve_kind(declared), HMIRTypeKind::Type) {
        let value = eval::eval_comptime(context, global.body(), global.initializer(), Some(declared))?;
        return comptime::independent(context, value, "a comptime global initializer", span).map(Into::into);
    }
    let source = context.env().symbols().resolve_definition(id);
    let name = source.name().name.to_string();
    let symbol_name = declaration::type_name(source);
    let publish = |env: &mut HMIREnvironment, value| {
        env.resolver_mut()
            .set_symbol(id, SymbolState::Evaluating(Some(value)))
    };
    let body = global.body();
    types::alias::define(context, name, span, publish, |context, reserved| {
        match types::aggregate(body, global.initializer()) {
            Some(declared) => {
                context.env_mut().resolver_mut().name_type(reserved, symbol_name);
                types::define_aggregate(context, body, declared, Some(reserved))
            }
            None => flow::top(
                flow::boundary(eval_type(context, body, global.initializer()))?.description(),
            ),
        }
    })
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
pub(crate) fn initialize(context: &mut LocalLoweringContext, id: HMIRDefID) -> CHLResult<HMIRValue> {
    let unit = context.unit(id.brand());
    let def = unit.resolve_def(id).unwrap();
    let declaration = declaration::canonical(def);
    let Some(HMIRDefKind::Global(global)) = declaration.kind() else {
        unreachable!("only storage globals have initial values");
    };
    let span = declaration.span();
    let mut child = context
        .child(None, FnContext::Comptime(ComptimeContext::new()), None)
        .sourced(id.brand());
    let ty = value_type(child.env_mut(), id)?;
    match global.initializer() {
        Some(expr) => eval::eval_comptime(&mut child, global.body(), expr, Some(ty)),
        None if global.linkage() == LinkageMode::Extern => {
            let subject = format!("initial value of '{}'", def.name());
            Err(hmir_error(span, &mir::COMPTIME_UNAVAILABLE, subject))
        }
        None => zero(&mut child, ty, span).map(|value| HMIRValue::new(ty, value)),
    }
}

pub(crate) fn zero(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRImmValue> {
    types::zero(context.env(), ty).ok_or_else(|| {
        let subject = format!("global of type '{}'", types::name(context.env(), ty));
        hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject)
    })
}
