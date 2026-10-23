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
    eval::types,
    log::hmir_error,
    symbol,
    value::HMIRValueKind,
};

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    ty: cx_hmir::ty::HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalID> {
    let symbol_name = symbol::link_name(env, def, None)?.expect("a global has a symbol name");
    let linkage = symbol::linkage(env, def, None)?;
    let mir_ty = types::lower_type(env, ty, span)?;
    let mutable = !env.semantics(ty).constant;
    let name = symbol_name.as_str();
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
        symbol_name,
        linkage,
        mir_ty,
        MIRGlobalState::External,
        mutable,
    )))
}

pub(crate) fn lower(env: &mut HMIREnvironment, def: HMIRDefID) -> CXResult<()> {
    let ty = symbol::value_type(env, def, None)?;
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("global owner exists");
    let source_def = unit.resolve_def(def).unwrap();
    let selected = symbol::select(source_def, None)?;
    let Some(HMIRDefKind::Global(source)) = source_def.declarations()[selected].kind() else {
        unreachable!("global declaration selected");
    };
    let linkage = symbol::linkage(env, def, None)?;
    let span = source.body().expr(source.ty()).span().clone();
    let id = declare(env, def, ty, &span)?;
    if source.initializer().is_none() && linkage == LinkageMode::Extern {
        return Ok(());
    }
    if !matches!(env.module().global(id).state(), MIRGlobalState::External) {
        let subject = (
            format!("global '{}'", source.symbol_name()),
            "this unit".into(),
        );
        return Err(hmir_error(&span, &mir::DUPLICATE_ENTITY, subject));
    }

    let state = match source.initializer() {
        Some(initializer) => {
            let mut context =
                LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
                    .sourced(def.brand());
            let value = symbol::initialize(&mut context, def)?;
            let HMIRValueKind::Constant(value) = value.kind() else {
                unreachable!("an exported comptime value is a constant")
            };
            let span = source.body().expr(initializer).span();
            MIRGlobalState::Initialized(runtime::constant(context.env_mut(), value, span)?)
        }
        None => MIRGlobalState::ZeroInitialized,
    };
    env.module_mut().define_global(id, state);

    Ok(())
}
