use cx_hmir::{
    HMIRGlobal,
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRGlobalVariable};
use cx_util::linkage::LinkageMode;

use crate::{
    emit::runtime,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{eval_comptime, eval_type, flow, types},
    log::hmir_error,
    value::HMIRValueKind,
};

pub fn declare(
    global: &HMIRGlobal,
    env: &mut HMIREnvironment,
) -> CXResult<(HMIRTypeID, MIRGlobalID)> {
    let mut context =
        LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()));
    let span = global.body().expr(global.ty()).span();
    let ty = flow::top(eval_type(&mut context, global.body(), global.ty()))?;
    let external = global.initializer().is_none() && global.linkage() == LinkageMode::Extern;
    if !external && !types::is_complete(context.env(), ty) {
        let subject = format!("global of type '{}'", types::name(context.env(), ty));
        return Err(hmir_error(span, &typecheck::INCOMPLETE_TYPE, subject));
    }
    let mir_ty = types::lower_type(context.env_mut(), ty, span)?;
    let mutable = !context.env().semantics(ty).constant;
    if let Some(id) = context
        .env()
        .module()
        .declared_global(global.symbol_name().as_str())
    {
        let existing = context.env().module().global(id);
        if existing.ty() != mir_ty || existing.is_mutable() != mutable {
            return Err(hmir_error(
                span,
                &mir::INCOMPATIBLE_GLOBAL,
                global.symbol_name().to_string(),
            ));
        }
        return Ok((ty, id));
    }
    let id = context
        .env_mut()
        .module_mut()
        .declare_global(MIRGlobalVariable::new(
            global.symbol_name().clone(),
            global.linkage(),
            mir_ty,
            MIRGlobalState::External,
            mutable,
        ));

    Ok((ty, id))
}

pub fn lower(global: &HMIRGlobal, env: &mut HMIREnvironment) -> CXResult<MIRGlobalID> {
    let (ty, id) = declare(global, env)?;
    let mut context =
        LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()));
    if !matches!(
        context.env().module().global(id).state(),
        MIRGlobalState::External
    ) {
        if global.initializer().is_none() {
            return Ok(id);
        }
        if matches!(
            context.env().module().global(id).state(),
            MIRGlobalState::Initialized(_)
        ) {
            let span = global.body().expr(global.initializer().unwrap()).span();
            let subject = (
                format!("global '{}'", global.symbol_name()),
                "this unit".into(),
            );
            return Err(hmir_error(span, &mir::DUPLICATE_ENTITY, subject));
        }
    }
    let state = match (global.initializer(), global.linkage()) {
        (Some(initializer), _) => {
            let span = global.body().expr(initializer).span();
            let value = eval_comptime(&mut context, global.body(), initializer, Some(ty))?;
            let HMIRValueKind::Constant(value) = value.kind() else {
                unreachable!("an exported comptime value is a constant")
            };
            MIRGlobalState::Initialized(runtime::constant(context.env_mut(), value, span)?)
        }
        (None, LinkageMode::Extern) => MIRGlobalState::External,
        (None, _) => MIRGlobalState::ZeroInitialized,
    };
    context.env_mut().module_mut().define_global(id, state);

    Ok(id)
}
