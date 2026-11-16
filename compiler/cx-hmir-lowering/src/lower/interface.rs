use cx_hir::ast::{
    modifiers::{HIRSymbolNameScheme, VisibilityMode},
    types::HIRTagKind,
};
use cx_hmir::{HMIRDefKind, HMIRFunctionStage, HMIRUnit};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::unit::interface::MIRCType;
use cx_util::linkage::LinkageMode;

use crate::{
    env::HMIREnvironment,
    eval::{symbol, types::comptime_only},
    log::hmir_error,
    lower::{declaration, global, types},
    value,
};

pub(crate) fn collect(env: &mut HMIREnvironment, unit: &HMIRUnit) -> CXResult<()> {
    let mut defs = unit.defs().collect::<Vec<_>>();
    defs.sort_by_key(|(id, _)| **id);
    for (id, def) in defs {
        if !def.declarations().iter().any(|declaration| {
            declaration.visibility() == VisibilityMode::Public
                && declaration.naming() == Some(HIRSymbolNameScheme::Unmangled)
        }) {
            continue;
        }
        let declared = declaration::canonical(def);
        let span = declared.span();
        match declared.kind() {
            Some(HMIRDefKind::Function(function)) if function.def().is_some() => {
                if declaration::linkage(def) == LinkageMode::Static {
                    continue;
                }
                if function.signature().stage() != HMIRFunctionStage::Runtime
                    || declaration::needs_specialization(function)
                {
                    return Err(hmir_error(
                        span,
                        &typecheck::INVALID_CONTEXT,
                        (
                            "a compile-time or generic function".into(),
                            "a C interface".into(),
                        ),
                    ));
                }
                let prototype = types::lower_prototype(env, *id, span)?;
                let function = env.module_mut().declare_function(prototype);
                env.module_mut().c_interface.functions.push(function);
            }
            Some(HMIRDefKind::Global(_)) if declaration::linkage(def) == LinkageMode::Standard => {
                let ty = symbol::value_type(env, *id)?;
                let global = global::declare(env, *id, ty, span)?;
                env.module_mut().c_interface.globals.push(global);
            }
            Some(HMIRDefKind::ComptimeGlobal(global)) if global.declares_type() => {
                let value = symbol::value(env, *id)?;
                let ty = value::as_type(&value).expect("a type declaration evaluates to a type");
                if comptime_only(env, ty) {
                    return Err(hmir_error(
                        span,
                        &typecheck::INVALID_CONTEXT,
                        ("a compile-time type".into(), "a C interface".into()),
                    ));
                }
                let ty = types::lower_type(env, ty, span)?;
                env.module_mut().c_interface.types.push(MIRCType {
                    name: declaration::type_name(def),
                    ty,
                    alias: declared.tag().is_none() || declared.tag() == Some(HIRTagKind::Enum),
                });
            }
            Some(HMIRDefKind::ComptimeGlobal(_)) => {
                let constant = symbol::value(env, *id)?;
                let Some(constant) = value::as_int(&constant) else {
                    return Err(hmir_error(
                        span,
                        &typecheck::INVALID_CONTEXT,
                        (
                            "a non-integer compile-time value".into(),
                            "a C interface".into(),
                        ),
                    ));
                };
                env.module_mut()
                    .c_interface
                    .constants
                    .push((declaration::type_name(def), constant));
            }
            _ => {}
        }
    }
    Ok(())
}
