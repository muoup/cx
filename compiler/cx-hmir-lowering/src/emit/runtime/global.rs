use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::MIRGlobalRef;
use cx_tokens::TokenRange;

use crate::{env::HMIREnvironment, eval::types, log::hmir_error, symbol};

pub fn address(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    path: &[usize],
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRGlobalRef> {
    let mut object = symbol::value_type(env, def, None)?;
    let global = crate::lower::global::declare(env, def, object, span)?;
    let mut offset = 0usize;
    for index in path {
        let (next, displacement) = match env.resolve_kind(object).clone() {
            HMIRTypeKind::Array { element, .. } => {
                let element_mir = types::lower_type(env, element, span)?;
                let size =
                    cx_mir::ty::layout::calculate_type_layout(env.module(), element_mir).size();
                (
                    element,
                    size.checked_mul(*index)
                        .ok_or_else(|| hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()))?,
                )
            }
            HMIRTypeKind::Struct { fields }
            | HMIRTypeKind::Union { fields }
            | HMIRTypeKind::TaggedUnion { fields } => {
                let field = fields.get(*index).ok_or_else(|| {
                    hmir_error(
                        span,
                        &mir::INDEX_BOUNDS,
                        ("global subobject".into(), index.to_string()),
                    )
                })?;
                let parent = types::lower_type(env, object, span)?;
                let layout =
                    cx_mir::ty::layout::calculate_field_layout(env.module(), parent, *index)
                        .expect("global field has a layout");
                (*field.ty(), layout.offset())
            }
            _ => {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "forming this global subobject address".into(),
                ));
            }
        };
        offset = offset
            .checked_add(displacement)
            .ok_or_else(|| hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()))?;
        object = next;
    }
    let offset =
        i64::try_from(offset).map_err(|_| hmir_error(span, &mir::COMPTIME_POINTER_OVERFLOW, ()))?;
    Ok(MIRGlobalRef {
        global,
        offset,
        ty: types::lower_type(env, ty, span)?,
    })
}
