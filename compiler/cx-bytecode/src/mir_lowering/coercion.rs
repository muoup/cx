//! Type conversion and coercion lowering

use cx_bytecode_data::{
    types::{BCFloatType, BCIntegerType, BCTypeKind},
    BCCoercionType, BCInstructionKind, BCValue,
};
use cx_typechecker_data::mir::{
    expression::{MIRCoercion, MIRExpression},
    types::MIRType,
};
use cx_util::CXResult;

use crate::builder::BCBuilder;
use super::expressions::lower_expression;

/// Lower a type conversion expression
pub fn lower_type_conversion(
    builder: &mut BCBuilder,
    operand: &MIRExpression,
    conversion: MIRCoercion,
    result_type: &MIRType,
) -> CXResult<BCValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);

    let coercion_type = convert_coercion(&conversion, &bc_operand)?;

    builder.add_new_instruction(
        BCInstructionKind::Coercion {
            coercion_type,
            value: bc_operand,
        },
        bc_result_type,
        true,
    )
}

fn convert_coercion(
    coercion: &MIRCoercion,
    operand: &BCValue,
) -> CXResult<BCCoercionType> {
    match coercion {
        MIRCoercion::ReinterpretBits => Ok(BCCoercionType::BitCast),
        MIRCoercion::IntToBool => {
            Ok(BCCoercionType::Trunc)
        }
        MIRCoercion::Integral { sextend, .. } => {
            if *sextend {
                Ok(BCCoercionType::SExtend)
            } else {
                Ok(BCCoercionType::ZExtend)
            }
        }
        MIRCoercion::FloatCast { .. } => {
            let from_type = match operand {
                BCValue::FloatImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        BCFloatType::F64
                    }
                }
                _ => BCFloatType::F64,
            };
            Ok(BCCoercionType::FloatCast { from: from_type })
        }
        MIRCoercion::IntToFloat { sextend, .. } => {
            let from_type = match operand {
                BCValue::IntImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        BCIntegerType::I64
                    }
                }
                _ => BCIntegerType::I64,
            };
            Ok(BCCoercionType::IntToFloat {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::PtrToInt { .. } => Ok(BCCoercionType::PtrToInt),
        MIRCoercion::IntToPtr { sextend } => {
            let from_type = match operand {
                BCValue::IntImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Integer(it) = _type.kind {
                        it
                    } else {
                        BCIntegerType::I64
                    }
                }
                _ => BCIntegerType::I64,
            };
            Ok(BCCoercionType::IntToPtr {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::FloatToInt { sextend, .. } => {
            let from_type = match operand {
                BCValue::FloatImmediate { _type, .. } => *_type,
                BCValue::Register { _type, .. } => {
                    if let BCTypeKind::Float(ft) = _type.kind {
                        ft
                    } else {
                        BCFloatType::F64
                    }
                }
                _ => BCFloatType::F64,
            };
            Ok(BCCoercionType::FloatToInt {
                from: from_type,
                sextend: *sextend,
            })
        }
    }
}
