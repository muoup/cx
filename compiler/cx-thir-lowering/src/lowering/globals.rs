use crate::builder::MIRBuilder;
use crate::lowering::{lower_expression, types::lower_type};
use cx_log::CXResult;
use cx_mir::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunctionID, MIRFunctionMode, MIRGlobalID,
    MIRGlobalKind, MIRGlobalState, MIRInstrKind,
};
use cx_mir_comptime::InterpretedFunction;
use cx_thir::thir::{expression::THIRExpression, global::THIRGlobalVariable};
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

pub struct MIRGlobalInitRequest {
    global_id: MIRGlobalID,
    init_id: MIRFunctionID,
    initializer: THIRExpression,
}

pub(crate) fn lower_unit_globals(builder: &mut MIRBuilder<'_>) -> CXResult<()> {
    for global in builder.module().globals_in_order() {
        let MIRGlobalKind::Variable { state, .. } = &global.kind else {
            continue;
        };
        let MIRGlobalState::Initializer(function_id) = state else {
            continue;
        };
        let Some(function) = builder.module().function(*function_id) else {
            continue;
        };
        let Some(entry) = InterpretedFunction::new(function) else {
            continue;
        };

        let constant = engine.run(entry, &[])?;
        evaluated.push((global.id, constant));
    }

    Ok(())
}

pub(crate) fn predeclare_global(
    builder: &mut MIRBuilder<'_>,
    global: &THIRGlobalVariable,
) -> CXResult<MIRGlobalID> {
    let ty = lower_type(builder, &global._type)?;

    builder.module_mut().declare_global(
        global.linkage == LinkageMode::Extern,
        global.name.clone(),
        global.linkage,
        MIRGlobalKind::Variable {
            ty,
            state: if global.linkage == LinkageMode::Extern {
                MIRGlobalState::External
            } else {
                MIRGlobalState::ZeroInitialized
            },
            is_mutable: global.is_mutable,
        },
        global
            .initializer
            .as_ref()
            .map(|initializer| &initializer.token_range)
            .unwrap_or(&cx_tokens::TokenRange::internal()),
    )
}

pub(crate) fn lower_global(
    builder: &mut MIRBuilder<'_>,
    id: MIRGlobalID,
    global: &THIRGlobalVariable,
) -> CXResult<Option<MIRGlobalInitRequest>> {
    let Some(init) = global.initializer.as_ref() else {
        return Ok(None);
    };

    let global_type = lower_type(builder, &global._type)?;

    let signature = MIRFnSignature::new(
        CXIdent::from(format!("__comptime_{}_init", global.name.as_str())),
        Some(global.name.clone()),
        Vec::<MIRFnParam>::new(),
        global_type,
        MIRFunctionMode::Comptime,
        false,
        true,
    );
    let init_id = builder
        .module_mut()
        .declare_function(MIRFnPrototype::new(signature, LinkageMode::Static));
    builder
        .module_mut()
        .begin_global_initializer(id, init_id, &init.token_range)?;

    Ok(Some(MIRGlobalInitRequest {
        global_id: id,
        init_id,
        initializer: init.clone(),
    }))
}

pub(crate) fn fulfill_init_request(
    builder: &mut MIRBuilder<'_>,
    request: MIRGlobalInitRequest,
) -> CXResult<()> {
    builder.start_function(request.init_id);

    let value = lower_expression(builder, &request.initializer)?;
    if !builder.fun_mut().current_block_terminated() {
        builder.emit(MIRInstrKind::Return { value: Some(value) });
    }

    builder.finish_function();
    builder.module_mut().set_global_state(
        request.global_id,
        MIRGlobalState::Initializer(request.init_id),
    );

    Ok(())
}
