use std::{cmp::Reverse, mem::discriminant, ptr};

use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefID, HMIRDefKind, HMIRFunction,
    HMIRFunctionStage, HMIRUnit, expr::HMIRExprKind, ty::HMIRExprTypeKind,
};
use cx_log::{CXResult, catalogue::typecheck, error::CXError};
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
        generation::Generation,
        resolver::demand,
    },
    eval::{eval_type, flow, symbol, types},
    log::hmir_error,
    value::{self, HMIRValue},
};

// Checks the declarations of each symbol of 'unit' against each other, evaluates those that must be, and requests
// the definitions the unit emits
pub(crate) fn resolve(env: &mut HMIREnvironment, unit: &HMIRUnit) -> CXResult<()> {
    let mut defs = unit.defs().collect::<Vec<_>>();
    defs.sort_by_key(|(id, _)| **id);
    for (id, def) in defs {
        declarations(def)?;
        agreement(def)?;
        linkage_agreement(def)?;
        evaluate(env, *id, def)?;
        request(env, *id, def);
    }
    Ok(())
}

fn declarations(def: &HMIRDef) -> CXResult<()> {
    let functions = def.declarations().iter().filter_map(|declaration| match declaration.kind() {
        Some(HMIRDefKind::Function(function)) => Some((declaration, function)),
        _ => None,
    });
    if let Some((duplicate, _)) = functions.clone().nth(1)
        && functions.clone().any(|(_, function)| !is_redeclarable(function))
    {
        let subject = (format!("declaration of '{}'", def.name()), "this unit".into());
        return Err(hmir_error(duplicate.span(), &typecheck::DUPLICATE_ITEM, subject));
    }
    let first = &def.declarations()[0];
    let kind = def.declarations().iter().find_map(HMIRDeclaration::kind);
    for declaration in def.declarations() {
        if declaration.tag() != first.tag() {
            let name = def.name().to_string();
            return Err(hmir_error(declaration.span(), &typecheck::INCOMPATIBLE_TAG, name));
        }
        if let (Some(kind), Some(other)) = (kind, declaration.kind())
            && discriminant(kind) != discriminant(other)
        {
            return Err(incompatible(def, declaration));
        }
    }
    let mut definitions = def
        .declarations()
        .iter()
        .filter(|declaration| declaration.role() == HMIRDeclarationRole::Definition);
    if definitions.next().is_some()
        && let Some(duplicate) = definitions.next()
        && (first.tag().is_some() || !is_type(first))
    {
        let subject = (format!("definition of '{}'", def.name()), "this unit".into());
        return Err(hmir_error(duplicate.span(), &typecheck::DUPLICATE_ITEM, subject));
    }
    Ok(())
}

// What every declaration of a symbol says of it apart from its types
fn agreement(def: &HMIRDef) -> CXResult<()> {
    let chosen = canonical(def);
    for declaration in def.declarations() {
        let agrees = match (chosen.kind(), declaration.kind()) {
            (Some(HMIRDefKind::Function(a)), Some(HMIRDefKind::Function(b))) => {
                let (left, right) = (a.signature(), b.signature());
                left.stage() == right.stage()
                    && left.is_variadic() == right.is_variadic()
                    && left.params().len() == right.params().len()
                    && left.params().iter().zip(right.params()).all(|(a, b)| a.comptime() == b.comptime())
                    && a.prototype().symbol_name() == b.prototype().symbol_name()
            }
            (Some(HMIRDefKind::Global(a)), Some(HMIRDefKind::Global(b))) => {
                a.symbol_name() == b.symbol_name()
            }
            (Some(HMIRDefKind::ComptimeGlobal(_)), Some(HMIRDefKind::ComptimeGlobal(_))) => {
                is_type(chosen) == is_type(declaration)
            }
            _ => true,
        };
        if !agrees {
            return Err(incompatible(def, declaration));
        }
    }
    if let Some(HMIRDefKind::Function(function)) = chosen.kind()
        && function.def().is_some()
        && function.signature().is_variadic()
        && function.signature().contract().is_safe()
    {
        let context = ("Varargs".into(), "a safe function".into());
        return Err(hmir_error(chosen.span(), &typecheck::INVALID_CONTEXT, context));
    }
    Ok(())
}

// An internal symbol is declared internal first; a function redeclared without 'static' stays internal
fn linkage_agreement(def: &HMIRDef) -> CXResult<()> {
    let mut linkages = linkages(def);
    let Some((first, mode)) = linkages.next() else {
        return Ok(());
    };
    let internal = mode == LinkageMode::Static;
    let inherits = matches!(first.kind(), Some(HMIRDefKind::Function(_)));
    for (declaration, mode) in linkages {
        if mode == LinkageMode::Static && !internal
            || mode == LinkageMode::Standard && internal && !inherits
        {
            return Err(incompatible(def, declaration));
        }
    }
    Ok(())
}

// A definition is evaluated whether or not it is named, as are the symbols whose declarations must agree on types
fn evaluate(env: &mut HMIREnvironment, id: HMIRDefID, def: &HMIRDef) -> CXResult<()> {
    let chosen = canonical(def);
    let redeclared = def.declarations().len() > 1;
    match chosen.kind() {
        Some(HMIRDefKind::ComptimeGlobal(_)) if chosen.tag().is_some() => {
            let value = symbol::value(env, id)?;
            let ty = value::as_type(&value).expect("a tagged declaration names a type");
            demand::force(env, ty)
        }
        Some(HMIRDefKind::ComptimeGlobal(_)) if redeclared => {
            let value = symbol::value(env, id)?;
            comptime_agreement(env, id, def, &value)
        }
        Some(HMIRDefKind::ComptimeGlobal(_)) if chosen.role() == HMIRDeclarationRole::Definition => {
            symbol::value(env, id).map(drop)
        }
        Some(HMIRDefKind::Global(_)) if redeclared => symbol::value(env, id).map(drop),
        Some(HMIRDefKind::Function(_)) if redeclared => signature_agreement(env, id, def),
        _ => Ok(()),
    }
}

fn signature_agreement(env: &mut HMIREnvironment, id: HMIRDefID, def: &HMIRDef) -> CXResult<()> {
    let canonical = symbol::signature(env, id)?;
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(id.brand());
    for declaration in def.declarations() {
        let Some(HMIRDefKind::Function(function)) = declaration.kind() else {
            continue;
        };
        let signature = flow::top(types::eval_signature(&mut context, function))?;
        if !types::signatures_alike(context.env(), &canonical, &signature) {
            return Err(incompatible(def, declaration));
        }
    }
    Ok(())
}

fn comptime_agreement(
    env: &mut HMIREnvironment,
    id: HMIRDefID,
    def: &HMIRDef,
    value: &HMIRValue,
) -> CXResult<()> {
    let chosen = canonical(def);
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(id.brand());
    for declaration in def.declarations() {
        if ptr::eq(declaration, chosen) {
            continue;
        }
        let Some(HMIRDefKind::ComptimeGlobal(global)) = declaration.kind() else {
            continue;
        };
        let declared = flow::boundary(eval_type(&mut context, global.body(), global.initializer()))?;
        let declared = flow::top(declared.description())?;
        match value::as_type(value) {
            Some(ty) if types::alike(context.env(), ty, declared, true) => {}
            _ => return Err(incompatible(def, declaration)),
        }
    }
    Ok(())
}

fn request(env: &mut HMIREnvironment, id: HMIRDefID, def: &HMIRDef) {
    let linkage = linkage(def);
    let root = linkage != LinkageMode::Static
        && match canonical(def).kind() {
            Some(HMIRDefKind::Function(function)) => {
                function.signature().stage() == HMIRFunctionStage::Runtime
                    && !needs_specialization(function)
                    && function.def().is_some()
            }
            Some(HMIRDefKind::Global(global)) => {
                global.initializer().is_some() || linkage != LinkageMode::Extern
            }
            _ => false,
        };
    if root {
        env.generation_mut().request(Generation::Definition(id), None);
    }
}

// Requests the definition of a symbol that is referenced; one of the local unit or a static one is only generated
// where it is referenced, while the others are requested as roots of their own unit
pub(crate) fn referenced(env: &mut HMIREnvironment, id: HMIRDefID) {
    let linkage = linkage(env.symbols().resolve_definition(id));
    if id.brand() == env.symbols().local_unit().id() || linkage == LinkageMode::Static {
        env.generation_mut().request(Generation::Definition(id), None);
    }
}

fn is_type(declaration: &HMIRDeclaration) -> bool {
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
        && function.prototype().params().iter().all(|param| param.default().is_none())
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

pub(crate) fn is_function(def: &HMIRDef) -> bool {
    matches!(canonical(def).kind(), Some(HMIRDefKind::Function(_)))
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

// The linkage of each declaration of a function or storage global, in order
fn linkages(def: &HMIRDef) -> impl Iterator<Item = (&HMIRDeclaration, LinkageMode)> {
    def.declarations()
        .iter()
        .filter_map(|declaration| match declaration.kind()? {
            HMIRDefKind::Function(function) => Some((declaration, function.prototype().linkage())),
            HMIRDefKind::Global(global) => Some((declaration, global.linkage())),
            _ => None,
        })
}

// A first internal declaration makes the symbol internal; otherwise the last one that is not 'extern' decides
pub(crate) fn linkage(def: &HMIRDef) -> LinkageMode {
    let mut linkages = linkages(def).map(|(_, linkage)| linkage);
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

pub(crate) fn link_name(def: &HMIRDef) -> Option<CXIdent> {
    match canonical(def).kind() {
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
