use crate::builder::MIRBuilder;
use crate::lowering::{lower_expression, types::lower_type};
use cx_log::CXResult;
use cx_mir::{
    MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRFunctionID, MIRFunctionMode, MIRGlobalID,
    MIRGlobalKind, MIRGlobalState, MIRInstrKind,
};
use cx_thir::thir::{
    expression::THIRExpression,
    global::THIRGlobalVariable,
};
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
    let ty = lower_type(builder, &global._type)?;

    Ok(builder.module_mut().declare_global(
        global.linkage == LinkageMode::Extern,
        global.name.clone(),
        global.linkage,
        MIRGlobalKind::Variable {
            ty,
            state: MIRGlobalState::ZeroInitialized,
            is_mutable: global.is_mutable,
        },
    ))
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
    if !builder.current_block_terminated() {
        builder.emit(MIRInstrKind::Return {
            value: Some(value),
        });
    }

    builder.finish_function();
    builder.module_mut().set_global_state(
        request.global_id,
        MIRGlobalState::Initializer(request.init_id),
    );

    Ok(())
}
