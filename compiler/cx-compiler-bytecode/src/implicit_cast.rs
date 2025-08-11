use crate::builder::BytecodeBuilder;
use cx_data_ast::parse::ast::CXCastType;
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::VirtualInstruction::IntToPtrDiff;
use cx_data_bytecode::{ValueID, VirtualInstruction};
use cx_data_bytecode::types::BCTypeKind;

pub(crate) fn implicit_cast(
    builder: &mut BytecodeBuilder,
    value: ValueID,
    from_type: &CXType,
    to_type: &CXType,
    cast_type: &CXCastType
) -> Option<ValueID> {
    match cast_type {
        CXCastType::BitCast => {
            builder.add_instruction_cxty(
                VirtualInstruction::BitCast {
                    value,
                },
                to_type.clone()
            )
        },
        
        CXCastType::PtrToInt => {
            let CXTypeKind::PointerTo { .. } = from_type.kind else {
                panic!("INTERNAL PANIC: Invalid pointer type")
            };

            builder.add_instruction_cxty(
                VirtualInstruction::PtrToInt {
                    value
                },
                to_type.clone()
            )
        },
        
        CXCastType::IntToPtr => {
            let CXTypeKind::Integer { bytes, signed } = &from_type.kind else {
                panic!("INTERNAL PANIC: Invalid integer type")
            };
            
            let val = if *bytes < 8 {
                if *signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        BCTypeKind::Signed { bytes: 8 }.into()
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        BCTypeKind::Unsigned { bytes: 8 }.into()
                    )?
                }
            } else {
                value
            };
            
            builder.add_instruction_cxty(
                VirtualInstruction::BitCast {
                    value: val
                },
                to_type.clone()
            )
        },

        CXCastType::IntToPtrDiff => {
            let CXTypeKind::PointerTo { inner_type: inner, .. } = &to_type.kind else {
                panic!("INTERNAL PANIC: Invalid pointer type")
            };
            
            let CXTypeKind::Integer { bytes, signed } = &from_type.kind else {
                panic!("INTERNAL PANIC: Invalid integer type")
            };
            
            let val = if *bytes < 8 {
                if *signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        BCTypeKind::Signed { bytes: 8 }.into()
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        BCTypeKind::Unsigned { bytes: 8 }.into()
                    )?
                }
            } else {
                value
            };
            
            let bc_type = builder.convert_cx_type(inner.as_ref())?;
            
            builder.add_instruction_cxty(
                IntToPtrDiff {
                    value: val,
                    ptr_type: bc_type,
                },
                to_type.clone()
            )
        }

        CXCastType::IntegralTrunc => {
            builder.add_instruction_cxty(
                VirtualInstruction::Trunc {
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::IntegralCast => {
            match &from_type.kind {
                CXTypeKind::Integer { signed: true, .. } =>
                    builder.add_instruction_cxty(
                        VirtualInstruction::SExtend {
                            value,
                        },
                        to_type.clone()
                    ),
                CXTypeKind::Integer { signed: false, .. } =>
                    builder.add_instruction_cxty(
                        VirtualInstruction::ZExtend {
                            value,
                        },
                        to_type.clone()
                    ),
                CXTypeKind::Bool =>
                    builder.add_instruction_cxty(
                        VirtualInstruction::BoolExtend {
                            value,
                        },
                        to_type.clone()
                    ),

                _ => panic!("INTERNAL PANIC: Invalid integral cast type: {to_type:?}")
            }
        },

        CXCastType::FunctionToPointerDecay => {
            builder.add_instruction_cxty(
                VirtualInstruction::GetFunctionAddr {
                    func: value,
                },
                to_type.clone()
            )
        },

        CXCastType::FloatCast => {
            builder.add_instruction_cxty(
                VirtualInstruction::FloatCast {
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::IntToFloat => {
            builder.add_instruction_cxty(
                VirtualInstruction::IntToFloat {
                    from: builder.convert_fixed_cx_type(from_type)?,
                    value,
                },
                to_type.clone()
            )
        },

        CXCastType::FloatToInt => {
            builder.add_instruction_cxty(
                VirtualInstruction::FloatToInt {
                    from: builder.convert_fixed_cx_type(from_type)?,
                    value,
                },
                to_type.clone()
            )
        },
        
        CXCastType::FauxLoad => {
            Some(value)
        },
        
        _ => todo!("implicit_cast({cast_type:?})")
    }
}