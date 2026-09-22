use cx_log::CXResult;
use cx_mir::{MIRGlobalID, MIRGlobalState, MIRGlobalVariable};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_util::linkage::LinkageMode;

use crate::{builder::MIRBuilder, lowering::{comptime, types::lower_type}};

pub(crate) fn predeclare_global(
    builder: &mut MIRBuilder<'_>,
    global: &THIRGlobalVariable,
) -> CXResult<MIRGlobalID> {
    let ty = lower_type(builder, &global._type)?;
    builder.module_mut().declare_global(MIRGlobalVariable::new(
        global.name.clone(),
        global.linkage,
        ty,
        if global.linkage == LinkageMode::Extern {
            MIRGlobalState::External
        } else {
            MIRGlobalState::ZeroInitialized
        },
        global.is_mutable,
    ))
}

pub(crate) fn lower_global(
    builder: &mut MIRBuilder<'_>,
    id: MIRGlobalID,
    global: &THIRGlobalVariable,
) -> CXResult<()> {
    if let Some(initializer) = &global.initializer {
        let value = comptime::evaluate(builder, initializer)?;
        builder.module_mut().set_global_state(
            id,
            MIRGlobalState::Initialized(value),
        );
    }
    Ok(())
}
