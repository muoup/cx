use crate::builder::MIRBuilder;
use cx_parsing_data::parse::ast::CXCastType;
use cx_mir_data::types::MIRTypeKind;
use cx_mir_data::VirtualInstruction::IntToPtrDiff;
use cx_mir_data::{MIRValue, VirtualInstruction};
use cx_typechecker_data::cx_types::{CXType, CXTypeKind};

pub(crate) fn implicit_cast(
    builder: &mut MIRBuilder,
    value: MIRValue,
    from_type: &CXType,
    to_type: &CXType,
    cast_type: &CXCastType,
) -> Option<MIRValue> {
    match cast_type {
        CXCastType::NOOP => Some(value),

        CXCastType::BitCast => {
            builder.add_instruction_cxty(VirtualInstruction::BitCast { value }, to_type.clone())
        }

        CXCastType::PtrToInt => {
            let CXTypeKind::PointerTo { .. } = from_type.kind else {
                panic!("INTERNAL PANIC: Invalid pointer type")
            };

            builder.add_instruction_cxty(VirtualInstruction::PtrToInt { value }, to_type.clone())
        }

        CXCastType::IntToPtr => {
            let CXTypeKind::Integer { bytes, signed } = &from_type.kind else {
                panic!("INTERNAL PANIC: Invalid integer type")
            };

            let val = if *bytes < 8 {
                if *signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend { value },
                        MIRTypeKind::Signed { bytes: 8 }.into(),
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend { value },
                        MIRTypeKind::Unsigned { bytes: 8 }.into(),
                    )?
                }
            } else {
                value
            };

            builder
                .add_instruction_cxty(VirtualInstruction::BitCast { value: val }, to_type.clone())
        }

        CXCastType::IntToPtrDiff => {
            let CXTypeKind::PointerTo {
                inner_type: inner, ..
            } = &to_type.kind
            else {
                panic!("INTERNAL PANIC: Invalid pointer type")
            };

            let CXTypeKind::Integer { bytes, signed } = &from_type.kind else {
                panic!("INTERNAL PANIC: Invalid integer type")
            };

            let val = if *bytes < 8 {
                if *signed {
                    builder.add_instruction(
                        VirtualInstruction::SExtend { value },
                        MIRTypeKind::Signed { bytes: 8 }.into(),
                    )?
                } else {
                    builder.add_instruction(
                        VirtualInstruction::ZExtend { value },
                        MIRTypeKind::Unsigned { bytes: 8 }.into(),
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
                to_type.clone(),
            )
        }

        CXCastType::IntegralTrunc => {
            builder.add_instruction_cxty(VirtualInstruction::Trunc { value }, to_type.clone())
        }

        CXCastType::IntegralCast => match &from_type.kind {
            CXTypeKind::Integer { signed: true, .. } => {
                builder.add_instruction_cxty(VirtualInstruction::SExtend { value }, to_type.clone())
            }
            CXTypeKind::Integer { signed: false, .. } => {
                builder.add_instruction_cxty(VirtualInstruction::ZExtend { value }, to_type.clone())
            }
            CXTypeKind::Bool => builder
                .add_instruction_cxty(VirtualInstruction::BoolExtend { value }, to_type.clone()),

            _ => panic!("INTERNAL PANIC: Invalid integral cast type: {to_type:?}"),
        },

        CXCastType::FunctionToPointerDecay => {
            let MIRValue::FunctionRef(func) = &value else {
                panic!("INTERNAL PANIC: Invalid function to pointer decay value")
            };

            builder.add_instruction_cxty(
                VirtualInstruction::GetFunctionAddr {
                    func: func.as_string(),
                },
                to_type.clone(),
            )
        }

        CXCastType::FloatCast => {
            builder.add_instruction_cxty(VirtualInstruction::FloatCast { value }, to_type.clone())
        }

        CXCastType::IntToFloat => builder.add_instruction_cxty(
            VirtualInstruction::IntToFloat {
                from: builder.convert_fixed_cx_type(from_type)?,
                value,
            },
            to_type.clone(),
        ),

        CXCastType::FloatToInt => builder.add_instruction_cxty(
            VirtualInstruction::FloatToInt {
                from: builder.convert_fixed_cx_type(from_type)?,
                value,
            },
            to_type.clone(),
        ),

        CXCastType::Load => {
            let new_type = builder.convert_cx_type(to_type)?;
            builder.load_value(value, new_type)
        }

        CXCastType::Reinterpret => Some(value),
    }
}
