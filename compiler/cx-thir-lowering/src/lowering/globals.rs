use cx_log::CXResult;
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRGlobalVariable};
use cx_thir::thir::{expression::THIRExpression, global::THIRGlobalVariable};
use cx_util::linkage::LinkageMode;

use crate::{
    builder::MIRBuilder,
    lowering::{comptime, types::lower_type},
};

pub(crate) struct MIRGlobalInitRequest<'thir> {
    pub global_id: MIRGlobalID,
    pub initializer: &'thir THIRExpression,
}

pub(crate) fn predeclare_global(
    builder: &mut MIRBuilder<'_>,
    global: &THIRGlobalVariable,
) -> CXResult<MIRGlobalID> {
    let ty = lower_type(builder, &global._type)?;
    let id = builder.module_mut().reserve_global(global.name.as_str());
    builder.module_mut().define_global(
        id,
        MIRGlobalVariable::new(
            global.name.clone(),
            global.linkage,
            ty,
            if global.linkage == LinkageMode::Extern || global.initializer.is_some() {
                MIRGlobalState::External
            } else {
                MIRGlobalState::ZeroInitialized
            },
            global.is_mutable,
        ),
    );
    Ok(id)
}

pub(crate) fn lower_global<'thir>(
    id: MIRGlobalID,
    global: &'thir THIRGlobalVariable,
) -> Option<MIRGlobalInitRequest<'thir>> {
    global
        .initializer
        .as_ref()
        .map(|initializer| MIRGlobalInitRequest {
            global_id: id,
            initializer,
        })
}

pub(crate) fn execute_request(
    builder: &mut MIRBuilder<'_>,
    request: &MIRGlobalInitRequest<'_>,
) -> CXResult<()> {
    let value = comptime::evaluate(builder, request.initializer)?;
    builder
        .module_mut()
        .set_global_state(request.global_id, MIRGlobalState::Initialized(value));
    Ok(())
}
