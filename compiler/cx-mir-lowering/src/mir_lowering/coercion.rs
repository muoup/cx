//! Type conversion and coercion lowering

use cx_lmir::{
    types::{LMIRFloatType, LMIRIntegerType, LMIRTypeKind},
    LMIRCoercionType, LMIRInstructionKind, LMIRValue,
};
use cx_mir::mir::{
    expression::{MIRCoercion, MIRExpression},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::LMIRBuilder;
use super::expressions::lower_expression;

/// Lower a type conversion expression
pub fn lower_type_conversion(
    builder: &mut LMIRBuilder,
    operand: &MIRExpression,
    conversion: MIRCoercion,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let coercion_type = convert_coercion(&conversion, &bc_operand)?;

    builder.add_new_instruction(
        LMIRInstructionKind::Coercion {
            coercion_type,
            value: bc_operand,
        },
        bc_result_type,
        true,
    )
}

fn convert_coercion(
    coercion: &MIRCoercion,
    operand: &LMIRValue,
) -> CXResult<LMIRCoercionType> {
    match coercion {
        MIRCoercion::ReinterpretBits => Ok(LMIRCoercionType::BitCast),
        MIRCoercion::IntToBool => {
            Ok(LMIRCoercionType::Trunc)
        }
        MIRCoercion::Integral { sextend, .. } => {
            if *sextend {
                Ok(LMIRCoercionType::SExtend)
            } else {
                Ok(LMIRCoercionType::ZExtend)
            }
        }
        MIRCoercion::FloatCast { .. } => {
            let from_type = match operand {
                LMIRValue::FloatImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        LMIRFloatType::F64
                    }
                }
                _ => LMIRFloatType::F64,
            };
            Ok(LMIRCoercionType::FloatCast { from: from_type })
        }
        MIRCoercion::IntToFloat { sextend, .. } => {
            let from_type = match operand {
                LMIRValue::IntImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        LMIRIntegerType::I64
                    }
                }
                _ => LMIRIntegerType::I64,
            };
            Ok(LMIRCoercionType::IntToFloat {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::PtrToInt { .. } => Ok(LMIRCoercionType::PtrToInt),
        MIRCoercion::IntToPtr { sextend } => {
            let from_type = match operand {
                LMIRValue::IntImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        LMIRIntegerType::I64
                    }
                }
                _ => LMIRIntegerType::I64,
            };
            Ok(LMIRCoercionType::IntToPtr {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::FloatToInt { sextend, .. } => {
            let from_type = match operand {
                LMIRValue::FloatImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        LMIRFloatType::F64
                    }
                }
                _ => LMIRFloatType::F64,
            };
            Ok(LMIRCoercionType::FloatToInt {
                from: from_type,
                sextend: *sextend,
            })
        }
    }
}
