use cx_data_ast::parse::value_type::{get_intrinsic_type, CXTypeUnion, CXValType};
use cx_data_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};
use cx_util::log_error;

pub(crate) fn implicit_cast(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXValType,
    to_type: &CXValType
) -> Option<ValueID> {
    if to_type.intrin_eq(from_type, &builder.type_map) {
        return Some(value);
    }

    match (get_intrinsic_type(&builder.type_map, from_type).cloned()?,
           get_intrinsic_type(&builder.type_map, to_type).cloned()?) {
        (CXTypeUnion::Integer { bytes: lb, .. }, CXTypeUnion::Integer { bytes: rb, .. })
            if lb > rb => {

            builder.add_instruction(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        }

        (CXTypeUnion::Integer { .. }, CXTypeUnion::Integer { signed: true, .. }) => {
            builder.add_instruction(
                VirtualInstruction::SExtend {
                    value,
                },
                to_type.clone()
            )
        },

        (CXTypeUnion::Integer { .. }, CXTypeUnion::Integer { signed: false, .. }) => {
            builder.add_instruction(
                VirtualInstruction::ZExtend {
                    value,
                },
                to_type.clone()
            )
        },

        (CXTypeUnion::PointerTo(ptr1), CXTypeUnion::PointerTo(ptr2)) => {
            if ptr1.is_intrinsic(&CXTypeUnion::Unit, &builder.type_map) ||
               ptr2.is_intrinsic(&CXTypeUnion::Unit, &builder.type_map) {
                return Some(value);
            }

            log_error!("TYPE ERROR: Implicit cast from {from_type} to {to_type} is not allowed");
        }

        _ => todo!("Implicit cast from {from_type} to {to_type}")
    }
}
