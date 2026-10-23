use cx_hmir::{HMIRDefID, HMIRDefKind, ty::context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRGlobalVariable};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    emit::runtime,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{eval_comptime, types},
    log::hmir_error,
    symbol::CompletedGlobal,
    value::HMIRValueKind,
};

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    global: &CompletedGlobal,
    span: &TokenRange,
) -> CXResult<MIRGlobalID> {
    let mir_ty = types::lower_type(env, global.ty, span)?;
    let mutable = !env.semantics(global.ty).constant;
    let name = global.symbol_name.as_str();
    if let Some(id) = env.module().declared_global(name) {
        let existing = env.module().global(id);
        if existing.ty() != mir_ty || existing.is_mutable() != mutable {
            return Err(hmir_error(
                span,
                &mir::INCOMPATIBLE_GLOBAL,
                name.to_string(),
            ));
        }
        return Ok(id);
    }

    Ok(env.module_mut().declare_global(MIRGlobalVariable::new(
        global.symbol_name.clone(),
        global.linkage,
        mir_ty,
        MIRGlobalState::External,
        mutable,
    )))
}

pub(crate) fn lower(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    global: &CompletedGlobal,
) -> CXResult<()> {
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("symbol owner is available");
    let HMIRDefKind::Global(source) = unit.resolve_def(def).unwrap().declarations()
        [global.declaration]
        .kind()
        .unwrap()
    else {
        unreachable!("a global symbol selects a global declaration")
    };
    let span = source.body().expr(source.ty()).span();
    let id = declare(env, global, span)?;
    if source.initializer().is_none() && global.linkage == LinkageMode::Extern {
        return Ok(());
    }
    if !matches!(env.module().global(id).state(), MIRGlobalState::External) {
        let subject = (
            format!("global '{}'", global.symbol_name),
            "this unit".into(),
        );
        return Err(hmir_error(span, &mir::DUPLICATE_ENTITY, subject));
    }

    let state = match source.initializer() {
        Some(initializer) => {
            let mut context =
                LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
                    .sourced(def.brand());
            let span = source.body().expr(initializer).span();
            let value = eval_comptime(&mut context, source.body(), initializer, Some(global.ty))?;
            let HMIRValueKind::Constant(value) = value.kind() else {
                unreachable!("an exported comptime value is a constant")
            };
            MIRGlobalState::Initialized(runtime::constant(context.env_mut(), value, span)?)
        }
        None => MIRGlobalState::ZeroInitialized,
    };
    env.module_mut().define_global(id, state);

    Ok(())
}
