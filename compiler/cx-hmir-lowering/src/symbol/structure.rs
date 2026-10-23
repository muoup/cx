use std::mem::discriminant;

use cx_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefKind, HMIRFunction, HMIRFunctionStage,
    HMIRGlobal,
};
use cx_log::{CXResult, catalogue::typecheck, error::CXError};
use cx_util::linkage::LinkageMode;

use crate::log::hmir_error;

pub(crate) fn check(def: &HMIRDef) -> CXResult<Shape> {
    let declarations = def.declarations();
    let tag = declarations[0].tag();
    if let Some(conflict) = declarations.iter().find(|other| other.tag() != tag) {
        let name = def.name().to_string();
        return Err(hmir_error(
            conflict.span(),
            &typecheck::INCOMPATIBLE_TAG,
            name,
        ));
    }

    let mut lowered = declarations
        .iter()
        .enumerate()
        .filter_map(|(index, declaration)| Some((index, declaration, declaration.kind()?)));
    let (first, _, kind) = lowered
        .next()
        .expect("a symbol has a declaration that lowers");
    if let Some((_, conflict, _)) =
        lowered.find(|(_, _, other)| discriminant(*other) != discriminant(kind))
    {
        return Err(incompatible(def, conflict));
    }

    match kind {
        HMIRDefKind::Function(_) => function(def),
        HMIRDefKind::Global(_) => global(def),
        HMIRDefKind::TypeDef(_) if tag.is_none() => Ok(Shape::Alias),
        HMIRDefKind::TypeDef(_) => Ok(Shape::Type(definition(def)?.unwrap_or(first))),
        HMIRDefKind::ComptimeGlobal(_) => Ok(Shape::Constant(definition(def)?.unwrap_or(first))),
        HMIRDefKind::Type(_) => Ok(Shape::Type(first)),
    }
}

pub(crate) fn is_staged(function: &HMIRFunction) -> bool {
    function.signature().stage() == HMIRFunctionStage::Comptime
        || function
            .signature()
            .params()
            .iter()
            .any(|param| param.comptime())
}

fn function(def: &HMIRDef) -> CXResult<Shape> {
    let definition = definition(def)?;
    let declaration = definition.unwrap_or(0);
    let functions = def
        .declarations()
        .iter()
        .map(|declaration| match declaration.kind() {
            Some(HMIRDefKind::Function(function)) => (declaration, &**function),
            _ => unreachable!("every declaration of a function declares a function"),
        })
        .collect::<Vec<_>>();

    let selected = functions[declaration].1;
    let expected = selected.signature();
    for (declaration, function) in &functions {
        let signature = function.signature();
        let alike = signature.stage() == expected.stage()
            && signature.is_variadic() == expected.is_variadic()
            && signature.params().len() == expected.params().len()
            && signature
                .params()
                .iter()
                .zip(expected.params())
                .all(|(param, expected)| param.comptime() == expected.comptime())
            && function.prototype().symbol_name() == selected.prototype().symbol_name();
        if !alike {
            return Err(incompatible(def, declaration));
        }
    }

    let linkages = functions
        .iter()
        .map(|(declaration, function)| (*declaration, function.prototype().linkage()));
    let linkage = linkage(def, linkages, selected.prototype().linkage(), true)?;

    Ok(Shape::Function {
        declaration,
        linkage,
        defined: definition.is_some(),
        staged: is_staged(selected),
    })
}

fn global(def: &HMIRDef) -> CXResult<Shape> {
    let definition = definition(def)?;
    let declarations = def.declarations();
    let tentative = declarations
        .iter()
        .position(|declaration| declaration.role() == HMIRDeclarationRole::Tentative);
    let declaration = definition.or(tentative).unwrap_or(0);
    let globals = declarations
        .iter()
        .map(|declaration| match declaration.kind() {
            Some(HMIRDefKind::Global(global)) => (declaration, &**global),
            _ => unreachable!("every declaration of a global declares a global"),
        })
        .collect::<Vec<_>>();

    let selected: &HMIRGlobal = globals[declaration].1;
    if let Some((conflict, _)) = globals
        .iter()
        .find(|(_, global)| global.symbol_name() != selected.symbol_name())
    {
        return Err(incompatible(def, conflict));
    }

    let linkages = globals
        .iter()
        .map(|(declaration, global)| (*declaration, global.linkage()));
    let linkage = linkage(def, linkages, selected.linkage(), false)?;

    Ok(Shape::Global {
        declaration,
        linkage,
        initialized: selected.initializer().is_some(),
    })
}

fn definition(def: &HMIRDef) -> CXResult<Option<usize>> {
    let mut definitions = def
        .declarations()
        .iter()
        .enumerate()
        .filter(|(_, declaration)| declaration.role() == HMIRDeclarationRole::Definition);
    let first = definitions.next().map(|(index, _)| index);
    match definitions.next() {
        Some((_, duplicate)) => {
            let subject = (
                format!("definition of '{}'", def.name()),
                "this unit".into(),
            );
            Err(hmir_error(
                duplicate.span(),
                &typecheck::DUPLICATE_ITEM,
                subject,
            ))
        }
        None => Ok(first),
    }
}

fn linkage<'d>(
    def: &HMIRDef,
    mut linkages: impl Iterator<Item = (&'d HMIRDeclaration, LinkageMode)>,
    selected: LinkageMode,
    inherits: bool,
) -> CXResult<LinkageMode> {
    let (_, first) = linkages.next().expect("a symbol has a declaration");
    let internal = first == LinkageMode::Static;
    let conflict = linkages.find(|(_, linkage)| match linkage {
        LinkageMode::Static => !internal,
        LinkageMode::Standard => internal && !inherits,
        LinkageMode::Extern => false,
    });
    match conflict {
        Some((declaration, _)) => Err(incompatible(def, declaration)),
        None if internal => Ok(LinkageMode::Static),
        None => Ok(selected),
    }
}

pub(crate) fn incompatible(def: &HMIRDef, declaration: &HMIRDeclaration) -> CXError {
    let name = def.name().to_string();
    hmir_error(
        declaration.span(),
        &typecheck::INCOMPATIBLE_DECLARATION,
        name,
    )
}
