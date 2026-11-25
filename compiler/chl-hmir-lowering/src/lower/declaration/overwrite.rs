use chl_hir::ast::modifiers::HIRSymbolNameScheme;
use chl_hmir::{
    HMIRDeclaration, HMIRDeclarationRole, HMIRDef, HMIRDefKind,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_util::linkage::LinkageMode;

use crate::{env::HMIREnvironment, eval::types, log::hmir_error};

pub(super) fn validate(def: &HMIRDef) -> CHLResult<()> {
    let mut marked = def
        .declarations()
        .iter()
        .filter(|declaration| declaration.overwrites());
    let Some(replacement) = marked.next() else {
        return Ok(());
    };
    let fail = |declaration: &HMIRDeclaration, context: &str| {
        hmir_error(
            declaration.span(),
            &typecheck::INVALID_CONTEXT,
            ("@overwrite".into(), context.into()),
        )
    };
    if let Some(duplicate) = marked.next() {
        return Err(fail(duplicate, "a second overwrite of the same symbol"));
    }
    if !def
        .declarations()
        .iter()
        .any(|declaration| declaration.point() < replacement.point())
    {
        return Err(fail(
            replacement,
            "a declaration without an earlier C predeclaration",
        ));
    }
    if !def.declarations().iter().all(|declaration| {
        declaration.role() == HMIRDeclarationRole::Declaration
            && declaration.naming() == Some(HIRSymbolNameScheme::Unmangled)
            && match declaration.kind() {
                Some(HMIRDefKind::Function(function)) => {
                    super::is_redeclarable(function)
                        && function.prototype().linkage() == LinkageMode::Extern
                }
                Some(HMIRDefKind::Global(global)) => global.is_explicit_extern(),
                _ => false,
            }
    }) {
        return Err(fail(
            replacement,
            "a declaration group containing something other than C predeclarations",
        ));
    }
    Ok(())
}

pub(crate) fn integer_types_alike(
    env: &HMIREnvironment,
    original: HMIRTypeID,
    replacement: HMIRTypeID,
    qualified: bool,
) -> bool {
    if types::alike(env, original, replacement, qualified) {
        return true;
    }
    let (mut original_sem, mut replacement_sem) =
        (env.semantics(original), env.semantics(replacement));
    if !qualified {
        original_sem.constant = false;
        replacement_sem.constant = false;
    }
    original_sem == replacement_sem
        && matches!(
            (env.resolve_kind(original), env.resolve_kind(replacement)),
            (HMIRTypeKind::Int(a), HMIRTypeKind::Int(b)) if a == b
        )
}
