use cx_log::CXResult;
use cx_mir::{MIRInstructionKind, MIRValue};
use cx_thir::thir::data::THIRType;
use cx_util::identifier::CXIdent;

use crate::builder::MIRBuilder;
use crate::lowering::types::lower_type;

pub(crate) fn allocate_variable(
    builder: &mut MIRBuilder<'_>,
    name: Option<CXIdent>,
    ty: &THIRType,
    value: Option<MIRValue>,
) -> CXResult<cx_mir::MIRPlaceID> {
    let type_id = lower_type(builder, ty)?;
    let place = builder.new_place(type_id, name, ty.is_nodrop());

    if let Some(value) = value {
        builder.emit(MIRInstructionKind::Store {
            target: place,
            ty: type_id,
            value,
        });
    }

    Ok(place)
}
