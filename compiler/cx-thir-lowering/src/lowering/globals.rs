use crate::builder::MIRBuilder;
use cx_log::CXResult;
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRInstrKind};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_util::linkage::LinkageMode;

pub(crate) fn lower_global_initializer(
    builder: &mut MIRBuilder<'_>,
    function: cx_mir::MIRFunctionID,
    global: &THIRGlobalVariable,
) -> CXResult<()> {
    let initializer = global
        .initializer
        .as_ref()
        .expect("global initialization request has no initializer");
    builder.start_global_initializer(function, initializer);
    let value = super::lower_expression(builder, initializer)?;
    builder.emit(MIRInstrKind::Return { value: Some(value) });
    builder.finish_function();
    Ok(())
}

pub(crate) fn lower_global(
    builder: &mut MIRBuilder<'_>,
    id: MIRGlobalID,
    global: &THIRGlobalVariable,
) {
    let state = if global.linkage == LinkageMode::Extern {
        MIRGlobalState::External
    } else {
        MIRGlobalState::ZeroInitialized
    };

    builder.set_global_state(id, state);
}
