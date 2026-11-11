use std::{cmp::Reverse, mem::discriminant};

use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefKind, HMIRFunction, HMIRFunctionStage,
    expr::HMIRExprKind, ty::HMIRExprTypeKind, unit::def::HMIRDeclarationPoint,
};
use cx_log::{CXResult, catalogue::typecheck, error::CXError};
use cx_util::linkage::LinkageMode;

use crate::log::hmir_error;

pub(crate) fn visible(
    def: &HMIRDef,
    point: Option<HMIRDeclarationPoint>,
) -> impl Iterator<Item = (usize, &HMIRDeclaration)> {
    def.declarations()
        .iter()
        .enumerate()
        .filter(move |(_, declaration)| {
            point.is_none_or(|point| {
                declaration.point().brand() != point.brand()
                    || declaration.point().index() <= point.index()
            })
        })
}

pub(crate) fn is_type(declaration: &HMIRDeclaration) -> bool {
    let Some(HMIRDefKind::ComptimeGlobal(global)) = declaration.kind() else {
        return false;
    };
    let HMIRExprKind::Type(ty) = global.body().expr(global.ty()).kind() else {
        return false;
    };
    matches!(ty.kind(), HMIRExprTypeKind::Type)
}

// Only a C-style function may be declared more than once. Any other function has one declaration, as a call to
// it binds its arguments from what that declaration says of the parameters
fn is_redeclarable(function: &HMIRFunction) -> bool {
    let signature = function.signature();
    let generator = matches!(
        function.body().expr(*signature.return_type()).kind(),
        HMIRExprKind::Type(ty) if matches!(ty.kind(), HMIRExprTypeKind::Type)
    );
    signature.stage() == HMIRFunctionStage::Runtime
        && !generator
        && signature.params().iter().all(|param| !param.comptime())
        && function
            .prototype()
            .params()
            .iter()
            .all(|param| param.default().is_none())
}

// The one function that 'def' means wherever it is named; its declarations have been checked against each other
pub(crate) fn function(def: &HMIRDef) -> CXResult<&HMIRFunction> {
    match def.declarations()[select(def, None)?].kind() {
        Some(HMIRDefKind::Function(function)) => Ok(function),
        _ => unreachable!("'{}' does not name a function", def.name()),
    }
}

// A function means the same throughout its unit, so every declaration of one is visible from any point
pub(crate) fn select(def: &HMIRDef, point: Option<HMIRDeclarationPoint>) -> CXResult<usize> {
    let functions = def
        .declarations()
        .iter()
        .filter_map(|declaration| match declaration.kind() {
            Some(HMIRDefKind::Function(function)) => Some((declaration, function)),
            _ => None,
        });
    let point = point.filter(|_| functions.clone().next().is_none());
    if let Some((duplicate, _)) = functions.clone().nth(1)
        && functions
            .clone()
            .any(|(_, function)| !is_redeclarable(function))
    {
        let duplicate_of = (
            format!("declaration of '{}'", def.name()),
            "this unit".into(),
        );
        return Err(hmir_error(
            duplicate.span(),
            &typecheck::DUPLICATE_ITEM,
            duplicate_of,
        ));
    }
    let declarations = visible(def, point).collect::<Vec<_>>();
    let Some((_, first)) = declarations.first() else {
        return Err(hmir_error(
            def.declarations()[0].span(),
            &typecheck::UNKNOWN_SYMBOL,
            def.name().to_string(),
        ));
    };
    let kind = declarations
        .iter()
        .find_map(|(_, declaration)| declaration.kind());
    for (_, declaration) in &declarations {
        if declaration.tag() != first.tag() {
            return Err(hmir_error(
                declaration.span(),
                &typecheck::INCOMPATIBLE_TAG,
                def.name().to_string(),
            ));
        }
        if let (Some(kind), Some(other)) = (kind, declaration.kind())
            && discriminant(kind) != discriminant(other)
        {
            return Err(incompatible(def, declaration));
        }
    }
    let mut definitions = declarations
        .iter()
        .filter(|(_, declaration)| declaration.role() == HMIRDeclarationRole::Definition);
    if let Some((_, _)) = definitions.next()
        && let Some((_, duplicate)) = definitions.next()
        && (first.tag().is_some() || !is_type(first))
    {
        return Err(hmir_error(
            duplicate.span(),
            &typecheck::DUPLICATE_ITEM,
            (
                format!("definition of '{}'", def.name()),
                "this unit".into(),
            ),
        ));
    }
    let selected = declarations
        .iter()
        .filter(|(_, declaration)| declaration.kind().is_some())
        .max_by_key(|(index, declaration)| (declaration.role(), Reverse(*index)))
        .map(|(index, _)| *index)
        .expect("a symbol has declaration content");
    let chosen = &def.declarations()[selected];
    for (_, declaration) in &declarations {
        match (chosen.kind(), declaration.kind()) {
            (Some(HMIRDefKind::Function(a)), Some(HMIRDefKind::Function(b))) => {
                let (a_sig, b_sig) = (a.signature(), b.signature());
                if a_sig.stage() != b_sig.stage()
                    || a_sig.is_variadic() != b_sig.is_variadic()
                    || a_sig.params().len() != b_sig.params().len()
                    || a_sig
                        .params()
                        .iter()
                        .zip(b_sig.params())
                        .any(|(a, b)| a.comptime() != b.comptime())
                    || a.prototype().symbol_name() != b.prototype().symbol_name()
                {
                    return Err(incompatible(def, declaration));
                }
            }
            (Some(HMIRDefKind::Global(a)), Some(HMIRDefKind::Global(b)))
                if a.symbol_name() != b.symbol_name() =>
            {
                return Err(incompatible(def, declaration));
            }
            (Some(HMIRDefKind::ComptimeGlobal(_)), Some(HMIRDefKind::ComptimeGlobal(_)))
                if is_type(chosen) != is_type(declaration) =>
            {
                return Err(incompatible(def, declaration));
            }
            _ => {}
        }
    }
    reconcile_linkage(def, point)?;
    Ok(selected)
}

pub(crate) fn needs_specialization(function: &HMIRFunction) -> bool {
    function.signature().stage() == HMIRFunctionStage::Runtime
        && function
            .signature()
            .params()
            .iter()
            .any(|param| param.comptime())
}

pub(crate) fn reconcile_linkage(
    def: &HMIRDef,
    point: Option<HMIRDeclarationPoint>,
) -> CXResult<LinkageMode> {
    let mut linkages =
        visible(def, point).filter_map(|(_, declaration)| match declaration.kind()? {
            HMIRDefKind::Function(function) => {
                Some((declaration, function.prototype().linkage(), true))
            }
            HMIRDefKind::Global(global) => Some((declaration, global.linkage(), false)),
            _ => None,
        });
    let Some((_, first, inherits)) = linkages.next() else {
        return Ok(LinkageMode::Standard);
    };
    let internal = first == LinkageMode::Static;
    let mut selected = first;
    for (declaration, linkage, _) in linkages {
        if linkage == LinkageMode::Static && !internal
            || linkage == LinkageMode::Standard && internal && !inherits
        {
            return Err(incompatible(def, declaration));
        }
        if linkage != LinkageMode::Extern {
            selected = linkage;
        }
    }
    Ok(if internal {
        LinkageMode::Static
    } else {
        selected
    })
}

pub(crate) fn incompatible(def: &HMIRDef, declaration: &HMIRDeclaration) -> CXError {
    hmir_error(
        declaration.span(),
        &typecheck::INCOMPATIBLE_DECLARATION,
        def.name().to_string(),
    )
}
