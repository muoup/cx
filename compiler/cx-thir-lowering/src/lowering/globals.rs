use crate::builder::MIRBuilder;
use crate::lowering::{lower_expression, types::lower_type};
use cx_log::CXResult;
use cx_mir::{
    MIRComptimeFnSignature, MIRConstant, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunctionID,
    MIRFunctionMode, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRInstrKind,
};
use cx_mir_comptime::evaluate_comptime_function;
use cx_thir::thir::{expression::THIRExpression, global::THIRGlobalVariable};
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

pub struct MIRGlobalInitRequest {
    global_id: MIRGlobalID,
    init_id: MIRFunctionID,
    initializer: THIRExpression,
}

pub(crate) fn predeclare_global(
    builder: &mut MIRBuilder<'_>,
    global: &THIRGlobalVariable,
) -> CXResult<MIRGlobalID> {
    let id = builder.module_mut().allocate_global_id();
    let ty = lower_type(builder, &global._type)?;

    builder.module_mut().declare_global(MIRGlobalVariable {
        id,
        name: global.name.clone(),
        _type: ty,
        kind: match global.kind {
            THIRGlobalKind::External => MIRGlobalKind::External,
            THIRGlobalKind::ZeroInitialized => MIRGlobalKind::ZeroInitialized,
            THIRGlobalKind::Initialized => MIRGlobalKind::Initialized,
        },
        token_range: global.token_range.clone(),
    })
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

    let signature = MIRComptimeFnSignature::new(MIRComptimeType::Standard(global_type), vec![]);
    let prototype = MIRComptimeFnPrototype::new(
        signature,
        global.name.clone()
    );

    let init_id = builder
        .module_mut()
        .declare_comptime_function(prototype);
    builder
        .module_mut()
        .begin_global_initializer(id, &init.token_range)?;

    Ok(Some(MIRGlobalInitRequest {
        global_id: id,
        init_id,
        initializer: init.clone(),
    }))
}

pub(crate) fn fulfill_init_request(
    builder: &mut MIRBuilder<'_>,
    request: &MIRGlobalInitRequest,
) -> CXResult<()> {
    builder.start_function(request.init_id);

    let value = lower_expression(builder, &request.initializer)?;
    if !builder.fun_mut().current_block_terminated() {
        builder.emit(MIRInstrKind::Return { value: Some(value) });
    }

    builder.finish_function()?;

    Ok(())
}

pub(crate) fn execute_request(
    builder: &mut MIRBuilder<'_>,
    request: &MIRGlobalInitRequest,
) -> CXResult<()> {
    let Some(func) = builder.module().function(request.init_id) else {
        unreachable!("Function for global init request not found");
    };

    let result = evaluate_comptime_function(builder, func, &[])?
        .constant()
        .ok_or_else(|| todo!("Expected a constant result from global initializer"))?;

    let intern_constant = builder.module_mut().intern_constant(result);

    builder
        .module_mut()
        .set_global_state(request.global_id, MIRGlobalState::Initialized(intern_constant));

    Ok(())
}
