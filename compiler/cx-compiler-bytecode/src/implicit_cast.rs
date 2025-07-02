use crate::builder::BytecodeBuilder;
use cx_data_ast::parse::ast::CXCastType;
use cx_data_ast::parse::value_type::{get_intrinsic_type, CXType, CXTypeKind};
use cx_data_bytecode::VirtualInstruction::IntToPtrDiff;
use cx_data_bytecode::{BCPtrBinOp, ValueID, VirtualInstruction};

pub(crate) fn implicit_cast(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXType,
    to_type: &CXType,
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
        
        CXCastType::PtrToInt => {
            let CXTypeKind::PointerTo { .. } =
                from_type.intrinsic_type(&builder.cx_type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid pointer type")
                };
            
            builder.add_instruction(
                VirtualInstruction::PtrToInt {
                    value
                },
                to_type.clone()
            )
        },
        
        CXCastType::IntToPtr => {
            let CXTypeKind::Integer { bytes, signed } =
                from_type.intrinsic_type(&builder.cx_type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid integer type")
                };
            
            let val = if bytes < 8 {
                if signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        CXTypeKind::Integer { bytes: 8, signed: true }.to_val_type()
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        CXTypeKind::Integer { bytes: 8, signed: false }.to_val_type()
                    )?
                }
            } else {
                value
            };
            
            builder.add_instruction(
                VirtualInstruction::BitCast {
                    value: val
                },
                to_type.clone()
            )
        },

        CXCastType::IntToPtrDiff => {
            let CXTypeKind::PointerTo { inner, .. } =
                to_type.intrinsic_type(&builder.cx_type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid pointer type")
                };
            
            let CXTypeKind::Integer { bytes, signed } =
                from_type.intrinsic_type(&builder.cx_type_map)?.clone() else {
                    panic!("INTERNAL PANIC: Invalid integer type")
                };
            
            let val = if bytes < 8 {
                if signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        CXTypeKind::Integer { bytes: 8, signed: true }.to_val_type()
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        CXTypeKind::Integer { bytes: 8, signed: false }.to_val_type()
                    )?
                }
            } else {
                value
            };
            
            let bc_type = builder.convert_cx_type(inner.as_ref())?;
            
            builder.add_instruction(
                IntToPtrDiff {
                    value: val,
                    ptr_type: bc_type,
                },
                to_type.clone()
            )
        }

        CXCastType::IntegralTrunc => {
            builder.add_instruction(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::IntegralCast => {
            match get_intrinsic_type(&builder.cx_type_map, from_type)? {
                CXTypeKind::Integer { signed: true, .. } =>
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        to_type.clone()
                    ),
                CXTypeKind::Integer { signed: false, .. } =>
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        to_type.clone()
                    ),
                CXTypeKind::Bool =>
                    builder.add_instruction(
                        VirtualInstruction::BoolExtend {
                            value,
                        },
                        to_type.clone()
                    ),

                _ => panic!("INTERNAL PANIC: Invalid integral cast type: {to_type:?}")
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
                    from: builder.convert_fixed_cx_type(from_type)?,
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::FloatToInt => {
            builder.add_instruction(
                VirtualInstruction::FloatToInt {
                    from: builder.convert_fixed_cx_type(from_type)?,
                    value,
                },
                to_type.clone()
            )
        },
        
        CXCastType::AddPointerTag => {
            builder.add_instruction(
                VirtualInstruction::AddPointerTag { value },
                to_type.clone()
            )
        },
        
        CXCastType::RemovePointerTag => {
            builder.add_instruction(
                VirtualInstruction::ClearPointerTag { value },
                to_type.clone()
            )
        },
        
        _ => todo!("implicit_cast({cast_type:?})")
    }
}