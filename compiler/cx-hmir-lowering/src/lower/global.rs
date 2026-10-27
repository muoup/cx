use cx_hmir::{
    HMIRDefID, HMIRDefKind,
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue};
use cx_mir::{MIRGlobalID, MIRGlobalRef, MIRGlobalState, MIRGlobalVariable};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    lower::{mir, types::lower_type},
    log::hmir_error,
    symbol,
    value::HMIRValueKind,
};

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalID> {
    let symbol_name = symbol::link_name(env, def, None)?.expect("a global has a symbol name");
    let linkage = symbol::linkage(env, def, None)?;
    let mir_ty = lower_type(env, ty, span)?;
    let mutable = !env.semantics(ty).constant;
    let name = symbol_name.as_str();
    if let Some(id) = env.module().declared_global(name) {
        let existing = env.module().global(id);
        if existing.ty() != mir_ty || existing.is_mutable() != mutable {
            return Err(hmir_error(
                span,
                &catalogue::mir::INCOMPATIBLE_GLOBAL,
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
        return Err(hmir_error(&span, &catalogue::mir::DUPLICATE_ENTITY, subject));
    }

    let state = match source.initializer() {
        Some(initializer) => {
            let mut context =
                LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
                    .sourced(def.brand());
            let value = symbol::initialize(&mut context, def)?;
            let HMIRValueKind::Constant(constant) = value.kind() else {
                unreachable!("an exported comptime value is a constant")
            };
            let span = source.body().expr(initializer).span();
            let constant = mir::constant(context.env_mut(), constant, value.ty(), span)?;
            MIRGlobalState::Initialized(constant)
        }
        None => MIRGlobalState::ZeroInitialized,
    };
    env.module_mut().define_global(id, state);

    Ok(())
}

pub fn address(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    offset: i64,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let object = symbol::value_type(env, def, None)?;
    let global = declare(env, def, object, span)?;
    Ok(MIRGlobalRef {
        global,
        offset,
        ty: lower_type(env, ty, span)?,
    })
}
