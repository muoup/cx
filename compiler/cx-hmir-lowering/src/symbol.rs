pub(crate) mod evaluate;

use std::cmp::Reverse;

use cx_hmir::{
    HMIRDeclaration, HMIRDef, HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunction,
    HMIRFunctionStage,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
    error::CXError,
};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        resolver::SymbolState,
    },
    log::hmir_error,
    value::HMIRValue,
};

// The value 'def' names, evaluated the first time it is asked for
pub(crate) fn value(env: &mut HMIREnvironment, def: HMIRDefID) -> CXResult<HMIRValue> {
    match env.resolver().symbol(def) {
        SymbolState::Ready(value) | SymbolState::Evaluating(Some(value)) => return Ok(value),
        SymbolState::Evaluating(None) => return Err(cycle(env, def)),
        SymbolState::Declared => {}
    }
    env.resolver_mut()
        .set_symbol(def, SymbolState::Evaluating(None));
    let result = evaluating(env, def, |context| evaluate::value(context, def));
    let state = match &result {
        Ok(value) => SymbolState::Ready(value.clone()),
        Err(_) => SymbolState::Declared,
    };
    env.resolver_mut().set_symbol(def, state);
    result
}

// Evaluates the body of the type 'def' reserved as 'ty'
pub(crate) fn complete(env: &mut HMIREnvironment, def: HMIRDefID, ty: HMIRTypeID) -> CXResult<()> {
    env.resolver_mut().set_pending(ty, None);
    evaluating(env, def, |context| evaluate::body(context, def, ty))
}

fn evaluating<T>(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    evaluate: impl FnOnce(&mut LocalLoweringContext) -> CXResult<T>,
) -> CXResult<T> {
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

fn cycle(env: &HMIREnvironment, def: HMIRDefID) -> CXError {
    let active = env.resolver().active();
    let start = active.iter().position(|active| *active == def).unwrap_or(0);
    let path = active[start..]
        .iter()
        .chain(std::iter::once(&def))
        .map(|id| env.symbols().resolve_definition(*id).name().to_string())
        .collect::<Vec<_>>()
        .join(" -> ")Tilde Codegen Backend;
    let span = env.symbols().resolve_definition(def).declarations()[0].span();
    hmir_error(span, &mir::DEPENDENCY_CYCLE, path)
}

// The declaration that stands for 'def': its definition, or else its first declaration
pub(crate) fn canonical(def: &HMIRDef) -> &HMIRDeclaration {
    def.declarations()
        .iter()
        .enumerate()
        .filter(|(_, declaration)| declaration.kind().is_some())
        .max_by_key(|(index, declaration)| (declaration.role(), Reverse(*index)))
        .map(|(_, declaration)| declaration)
        .expect("a symbol has declaration content")
}

pub(crate) fn function(def: &HMIRDef) -> &HMIRFunction {
    match canonical(def).kind() {
        Some(HMIRDefKind::Function(function)) => function,
        _ => unreachable!("'{}' does not name a function", def.name()),
    }
}

pub(crate) fn needs_specialization(function: &HMIRFunction) -> bool {
    function.signature().stage() == HMIRFunctionStage::Runtime
        && function
            .signature()
            .params()
            .iter()
            .any(|param| param.comptime())
}

pub(crate) fn signature(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
    let value = value(env, def)?;
    let HMIRTypeKind::Function(signature) = env.resolve_kind(value.ty()) else {
        unreachable!("a function binding has a function type")
    };
    Ok(signature.clone())
}

pub(crate) fn value_type(env: &mut HMIREnvironment, def: HMIRDefID) -> CXResult<HMIRTypeID> {
    let value = value(env, def)?;
    Ok(env.referent(value.ty()))
}

// The linkage of each declaration of a function or storage global, in order
pub(crate) fn linkages(def: &HMIRDef) -> impl Iterator<Item = (&HMIRDeclaration, LinkageMode)> {
    def.declarations()
        .iter()
        .filter_map(|declaration| match declaration.kind()? {
            HMIRDefKind::Function(function) => Some((declaration, function.prototype().linkage())),
            HMIRDefKind::Global(global) => Some((declaration, global.linkage())),
            _ => None,
        })
}

// A first internal declaration makes the symbol internal; otherwise the last one that is not 'extern' decides
pub(crate) fn linkage(env: &mut HMIREnvironment, def: HMIRDefID) -> LinkageMode {
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("definition owner exists");
    let mut linkages = linkages(unit.resolve_def(def).unwrap()).map(|(_, linkage)| linkage);
    let Some(first) = linkages.next() else {
        return LinkageMode::Standard;
    };
    if first == LinkageMode::Static {
        return first;
    }
    linkages
        .filter(|linkage| *linkage != LinkageMode::Extern)
        .last()
        .unwrap_or(first)
}

pub(crate) fn link_name(env: &mut HMIREnvironment, def: HMIRDefID) -> Option<CXIdent> {
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("definition owner exists");
    match canonical(unit.resolve_def(def).unwrap()).kind() {
        Some(HMIRDefKind::Function(function)) => Some(function.prototype().symbol_name().clone()),
        Some(HMIRDefKind::Global(global)) => Some(global.symbol_name().clone()),
        _ => None,
    }
}

pub(crate) fn incompatible(def: &HMIRDef, declaration: &HMIRDeclaration) -> CXError {
    hmir_error(
        declaration.span(),
        &typecheck::INCOMPATIBLE_DECLARATION,
        def.name().to_string(),
    )
}
