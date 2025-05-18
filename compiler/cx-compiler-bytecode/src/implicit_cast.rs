use std::clone;
use cx_data_ast::parse::ast::{CXBinOp, CXCastType};
use cx_data_ast::parse::value_type::{get_intrinsic_type, CXTypeUnion, CXValType};
use cx_data_bytecode::builder::VirtualInstruction::IntegerBinOp;
use cx_data_bytecode::builder::{BytecodeBuilder, ValueID, VirtualInstruction};

pub(crate) fn implicit_cast(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXValType,
    to_type: &CXValType,
    cast_type: &CXCastType
) -> Option<ValueID> {
    match cast_type {
        CXCastType::BitCast => {
            builder.add_instruction(
                VirtualInstruction::BitCast {
                    value,
                },
                to_type.clone()
            )
        },
        
        CXCastType::IntToScaledPtrDiff => {
            let CXTypeUnion::PointerTo(inner) =
                to_type.intrinsic_type(&builder.type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid pointer type")
                };
            let elem_size = inner.size(&builder.type_map)?;
            
            let CXTypeUnion::Integer { bytes, signed } =
                from_type.intrinsic_type(&builder.type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid integer type")
                };
            
            let val = if bytes < 8 {
                if signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        CXTypeUnion::Integer { bytes: 8, signed: false }.to_val_type()
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        CXTypeUnion::Integer { bytes: 8, signed: false }.to_val_type()
                    )?
                }
            } else {
                value
            };
            
            let constant = builder.add_instruction(
                VirtualInstruction::Immediate {
                    value: elem_size as i32
                },
                CXTypeUnion::Integer { bytes: 8, signed: false }.to_val_type()
            )?;
            
            builder.add_instruction(
                IntegerBinOp {
                    op: CXBinOp::Multiply,
                    left: val,
                    right: constant
                },
                to_type.clone()
            )
        },

        CXCastType::IntegralTrunc => {
            builder.add_instruction(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::IntegralCast => {
            let (_type, signed) = match get_intrinsic_type(&builder.type_map, to_type)? {
                CXTypeUnion::Integer { signed, .. } => (to_type.clone(), *signed),
                CXTypeUnion::PointerTo(_)
                    => (CXTypeUnion::Integer { signed: false, bytes: 8 }.to_val_type(), false),

                _ => panic!("INTERNAL PANIC: Invalid integral cast type")
            };

            if signed {
                builder.add_instruction(
                    VirtualInstruction::SExtend {
                        value,
                    },
                    _type
                )
            } else {
                builder.add_instruction(
                    VirtualInstruction::ZExtend {
                        value,
                    },
                    _type
                )
            }
        },

        CXCastType::FunctionToPointerDecay => {
            builder.add_instruction(
                VirtualInstruction::GetFunctionAddr {
                    func_name: value,
                },
                to_type.clone()
            )
        },

        CXCastType::FloatCast => {
            builder.add_instruction(
                VirtualInstruction::FloatCast {
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::IntToFloat => {
            builder.add_instruction(
                VirtualInstruction::IntToFloat {
                    from: from_type.clone(),
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::FloatToInt => {
            builder.add_instruction(
                VirtualInstruction::FloatToInt {
                    from: from_type.clone(),
                    value,
                },
                to_type.clone()
            )
        },

        _ => todo!("implicit_cast({cast_type:?})")
    }
}