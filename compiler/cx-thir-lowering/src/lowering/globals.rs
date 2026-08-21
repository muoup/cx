use crate::builder::MIRBuilder;
use cx_hir::ast::modifiers::HIR_CONST;
use cx_log::CXResult;
use cx_mir::{
    MIRFnPrototype, MIRFunctionID, MIRGlobalID, MIRGlobalKind, MIRGlobalState, MIRInstrKind,
};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_util::linkage::LinkageMode;

pub struct MIRGlobalInitRequest {
    global_id: MIRGlobalID,
    init_id: MIRFunctionID,
}

pub(crate) fn predeclare_global(
    builder: &mut MIRBuilder<'_>,
    global: &THIRGlobalVariable,
) -> CXResult<MIRGlobalID> {
    let ty = lower_type(builder.types_mut(), &global._type)?;

    Ok(builder.module_mut().declare_global(
        pre_used,
        global.name.clone(),
        global.linkage,
        MIRGlobalKind::Variable {
            ty,
            state: match global.initializer {
                Some(_) => MIRGlobalState::Extern,
                None => MIRGlobalState::ZeroInitialized,
            },
            is_mutable: global._type.get_specifier(HIR_CONST),
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

    let global_type = lower_type(builder.types_mut(), &global._type)?;

    let initializer_name = CXIdent::from(format!("__comptime_{}_init", global.name.as_str()));
    let func = MIRFnPrototype {
        linkage: LinkageMode::Static,
        signature: MIRFnSignature {
            parameters: vec![],
            return_type: global_type,
        },
    };

    let init_id = builder.start_new_function(func);
    Ok(Some(MIRGlobalInitRequest {
        global_id: id,
        init_id,
    }))
}

pub(crate) fn fulfill_init_request(
    builder: &mut MIRBuilder<'_>,
    request: MIRGlobalInitRequest,
) -> CXResult<()> {
    todo!()
}
