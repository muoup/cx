use cx_bytecode_data::{
    types::{BCType, BCTypeKind},
    BCCoercionType, BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::expression::{MIRCoercion, MIRRegister, MIRValue};
use cx_util::CXResult;

use crate::{builder::BCBuilder, mir_lowering::instructions::lower_value};

pub fn lower_coercion(
    builder: &mut BCBuilder,
    result: MIRRegister,
    value: &MIRValue,
    coercion_type: MIRCoercion,
) -> CXResult<BCValue> {
    let bc_value = lower_value(builder, value)?;

    match coercion_type {
        MIRCoercion::ReinterpretBits => {
            builder.insert_symbol(result, bc_value.clone());
            
            Ok(bc_value)
        },

        MIRCoercion::Integral { sextend, to_type } => {
            let bc_itype = builder.convert_integer_type(&to_type);
            let from_type = builder.convert_cx_type(&value.get_type());
            
            let from_bytes = from_type.size();
            let to_bytes = to_type.bytes();
            
            if from_bytes > to_bytes {
                builder.add_instruction_translated(
                    BCInstructionKind::Coercion {
                        coercion_type: BCCoercionType::Trunc,
                        value: bc_value,
                    },
                    BCType::from(BCTypeKind::Integer(bc_itype)),
                    Some(result),
                )
            } else if sextend { 
                builder.add_instruction_translated(
                    BCInstructionKind::Coercion {
                        coercion_type: BCCoercionType::SExtend,
                        value: bc_value,
                    },
                    BCType::from(BCTypeKind::Integer(bc_itype)),
                    Some(result),
                )
            } else {
                builder.add_instruction_translated(
                    BCInstructionKind::Coercion {
                        coercion_type: BCCoercionType::ZExtend,
                        value: bc_value,
                    },
                    BCType::from(BCTypeKind::Integer(bc_itype)),
                    Some(result),
                )
            }
        }

        MIRCoercion::BoolToInt { to_type } => {
            let bc_itype = builder.convert_integer_type(&to_type);

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::BoolExtend,
                    value: bc_value,
                },
                BCType::from(BCTypeKind::Integer(bc_itype)),
                Some(result),
            )
        }

        MIRCoercion::FloatCast { to_type } => {
            let bc_itype = builder.convert_float_type(&to_type);
            
            let BCTypeKind::Float(from) = builder.convert_cx_type(&value.get_type()).kind else {
                unreachable!()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::FloatCast { from },
                    value: bc_value,
                },
                BCType::from(BCTypeKind::Float(bc_itype)),
                Some(result),
            )
        }

        MIRCoercion::IntToFloat { to_type, sextend } => {
            let to_ftype = builder.convert_float_type(&to_type);
            let to_type = BCType::from(BCTypeKind::Float(to_ftype));

            let BCTypeKind::Integer(from_itype) = builder.convert_cx_type(&value.get_type()).kind else {
                unreachable!()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::IntToFloat {
                        from: from_itype,
                        sextend,
                    },
                    value: bc_value,
                },
                to_type,
                Some(result),
            )
        }

        MIRCoercion::PtrToInt { to_type } => {
            let bc_itype = builder.convert_integer_type(&to_type);
            let to_type = BCType::from(BCTypeKind::Integer(bc_itype));

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::PtrToInt,
                    value: bc_value,
                },
                to_type,
                Some(result),
            )
        }

        MIRCoercion::IntToPtr { sextend } => {
            let bc_type = builder.convert_cx_type(&value.get_type());
            let BCTypeKind::Integer(bc_itype) = &bc_type.kind else {
                unreachable!()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::IntToPtr {
                        from: *bc_itype,
                        sextend,
                    },
                    value: bc_value,
                },
                BCType::default_pointer(),
                Some(result),
            )
        }

        MIRCoercion::IntToBool => {
            let bool_type = BCType::from(BCTypeKind::Bool);

            let from_type = builder.convert_cx_type(&value.get_type());
            let BCTypeKind::Integer(from_itype) = &from_type.kind else {
                unreachable!()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::IntToBool { from: *from_itype },
                    value: bc_value,
                },
                bool_type,
                Some(result),
            )
        }

        MIRCoercion::FloatToInt { to_type, sextend } => {
            let from_type = builder.convert_cx_type(&value.get_type());

            let to_itype = builder.convert_integer_type(&to_type);
            let int_type = BCType::from(BCTypeKind::Integer(to_itype));

            let BCTypeKind::Float(from_ftype) = &from_type.kind else {
                unreachable!()
            };

            builder.add_instruction_translated(
                BCInstructionKind::Coercion {
                    coercion_type: BCCoercionType::FloatToInt {
                        from: from_ftype.clone(),
                        sextend: sextend,
                    },
                    value: bc_value,
                },
                int_type,
                Some(result),
            )
        }
    }
}
