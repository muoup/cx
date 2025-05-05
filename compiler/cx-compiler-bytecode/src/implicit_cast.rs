use cx_data_ast::parse::value_type::{get_intrinsic_type, CXValType};
use cx_data_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};

pub(crate) fn implicit_cast(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXValType,
    to_type: &CXValType
) -> Option<ValueID> {
    match (get_intrinsic_type(&builder.type_map, from_type).cloned()?,
           get_intrinsic_type(&builder.type_map, to_type).cloned()?) {
        (x, y) if x == y => {
            Some(value)
        }

        (CXValType::Integer { bytes: lb, .. }, CXValType::Integer { bytes: rb, .. })
        if lb > rb => {

            builder.add_instruction(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        }

        (CXValType::Integer { .. }, CXValType::Integer { signed: true, .. }) => {
            builder.add_instruction(
                VirtualInstruction::SExtend {
                    value,
                },
                to_type.clone()
            )
        },

        (CXValType::Integer { .. }, CXValType::Integer { signed: false, .. }) => {
            builder.add_instruction(
                VirtualInstruction::ZExtend {
                    value,
                },
                to_type.clone()
            )
        },

        _ => todo!("Implicit cast from {from_type} to {to_type}")
    }
}
