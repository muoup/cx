use cx_hmir::{
    HMIRDefID, HMIRDefKind,
    ty::{HMIRTypeID, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue};
use cx_mir::{
    MIRGlobalID, MIRGlobalRef, MIRGlobalState, MIRGlobalVariable, ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use crate::{
    emit::runtime,
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::symbol,
    log::hmir_error,
    lower::{declaration, types::lower_type},
    value::HMIRValueKind,
};

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalID> {
    declaration::referenced(env, def);
    let source = env.symbols().resolve_definition(def);
    let symbol_name = declaration::link_name(source).expect("a global has a symbol name");
    let linkage = declaration::linkage(source);
    let mir_ty = lower_type(env, ty, span)?;
    let mutable = !env.semantics(ty).constant;
    let name = symbol_name.as_str();
    if let Some(id) = env.module().declared_global(name) {
        let existing = env.module().global(id);
        if !env.module().same_type(existing.ty(), mir_ty) || existing.is_mutable() != mutable {
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
    let ty = symbol::value_type(env, def)?;
    let unit = env
        .symbols_mut()
        .owner(def.brand())
        .expect("global owner exists");
    let declared = unit.resolve_def(def).unwrap();
    let Some(HMIRDefKind::Global(source)) = declaration::canonical(declared).kind() else {
        unreachable!("a global is declared as one");
    };
    let linkage = declaration::linkage(declared);
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
        return Err(hmir_error(
            &span,
            &catalogue::mir::DUPLICATE_ENTITY,
            subject,
        ));
    }

    let state = match source.initializer() {
        Some(initializer) => {
            let mut context =
                LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
                    .sourced(def.brand());
            let value = symbol::initialize(&mut context, def)?;
            let HMIRValueKind::Immediate(constant) = value.kind() else {
                unreachable!("an exported comptime value is a constant")
            };
            let span = source.body().expr(initializer).span();
            let constant = runtime::constant(&mut context, constant, value.ty(), span)?;
            MIRGlobalState::Initialized(constant)
        }
        None => MIRGlobalState::ZeroInitialized,
    };
    env.module_mut().define_global(id, state);

    Ok(())
}

pub fn reference(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    offset: i64,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let object = symbol::value_type(env, def)?;
    let global = declare(env, def, object, span)?;
    Ok(MIRGlobalRef {
        global,
        offset,
        ty: lower_type(env, ty, span)?,
    })
}
