use cx_log::CXResult;
use cx_mir::{MIRAssignTarget, MIRInstrKind, MIRValue};
use cx_thir::thir::data::THIRType;
use cx_thir::type_context::THIRTypeContext;

use crate::builder::MIRBuilder;
use crate::lowering::types::lower_type;

pub(super) fn assign_operand_to_place(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
    name: Option<cx_util::identifier::CXIdent>,
) -> CXResult<cx_mir::MIRPlace> {
    let type_id = lower_type(builder, ty)?;
    let place = builder.create(type_id, name, ty.is_nodrop());
    builder.emit(MIRInstrKind::Assign {
        target: MIRAssignTarget::Place(place),
        value,
        ty: type_id,
    });
    Ok(place)
}

pub(super) fn ensure_place(
    builder: &mut MIRBuilder<'_>,
    value: MIRValue,
    ty: &THIRType,
) -> CXResult<cx_mir::MIRPlace> {
    match value {
        MIRValue::PlaceRef(place) => Ok(place),
        value if ty.is_memory_reference() => {
            let inner_type = ty
                .mem_ref_inner()
                .expect("memory reference is missing its pointee type");
            let pointee = builder.registry().resolve_type_id(inner_type).clone();
            let pointee_type = lower_type(builder, &pointee)?;
            let out = builder.fun_mut().new_place(pointee_type, None, false);
            builder.emit(MIRInstrKind::Dereference {
                out,
                pointer: value,
                pointee_type,
            });
            Ok(out)
        }
        value => assign_operand_to_place(builder, value, ty, None),
    }
}
