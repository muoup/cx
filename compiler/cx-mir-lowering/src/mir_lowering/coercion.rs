//! Type conversion and coercion lowering

use cx_lmir::{
    LMIRCoercionType, LMIRInstructionKind, LMIRValue, types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind}
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
    coercion: MIRCoercion,
    result_type: &MIRType,
) -> CXResult<LMIRValue> {
    let bc_operand = lower_expression(builder, operand)?;
    let bc_result_type = builder.convert_cx_type(result_type);
    
    let std_coercion = |builder: &mut LMIRBuilder, bc_operand: LMIRValue, coercion_type: LMIRCoercionType| {
        builder.add_new_instruction(
            LMIRInstructionKind::Coercion {
                value: bc_operand,
                coercion_type,
            },
            bc_result_type.clone(),
            true
        )
    };

    match &coercion {
        MIRCoercion::ReinterpretBits => std_coercion(builder, bc_operand, LMIRCoercionType::BitCast),
        MIRCoercion::IntToBool => {
            std_coercion(builder, bc_operand, LMIRCoercionType::Trunc)
        }
        MIRCoercion::Integral { sextend, from_type, to_type } => {
            if from_type.bytes() > to_type.bytes() {
                std_coercion(builder, bc_operand, LMIRCoercionType::Trunc)
            } else if *sextend {
                std_coercion(builder, bc_operand, LMIRCoercionType::SExtend)
            } else {
                std_coercion(builder, bc_operand, LMIRCoercionType::ZExtend)
            }
        }
        MIRCoercion::FloatCast { .. } => {
            let from_type = match &bc_operand {
                LMIRValue::FloatImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Float(ft) = &_type.kind {
                        *ft
                    } else {
                        LMIRFloatType::F64
                    }
                }
                _ => LMIRFloatType::F64,
            };
            std_coercion(builder, bc_operand, LMIRCoercionType::FloatCast { from: from_type })
        }
        MIRCoercion::IntToFloat { sextend, .. } => {
            let from_type = match &bc_operand {
                LMIRValue::IntImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Integer(it) = &_type.kind {
                        *it
                    } else {
                        LMIRIntegerType::I64
                    }
                }
                _ => LMIRIntegerType::I64,
            };
            std_coercion(builder, bc_operand, LMIRCoercionType::IntToFloat {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::PtrToInt { .. } => std_coercion(builder, bc_operand, LMIRCoercionType::PtrToInt),
        MIRCoercion::IntToPtr { sextend } => {
            let from_type = match &bc_operand {
                LMIRValue::IntImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Integer(it) = &_type.kind {
                        *it
                    } else {
                        LMIRIntegerType::I64
                    }
                }
                _ => LMIRIntegerType::I64,
            };
            std_coercion(builder, bc_operand, LMIRCoercionType::IntToPtr {
                from: from_type,
                sextend: *sextend,
            })
        }
        MIRCoercion::FloatToInt { sextend, .. } => {
            let from_type = match &bc_operand {
                LMIRValue::FloatImmediate { _type, .. } => *_type,
                LMIRValue::Register { _type, .. } => {
                    if let LMIRTypeKind::Float(ft) = &_type.kind {
                        *ft
                    } else {
                        LMIRFloatType::F64
                    }
                }
                _ => LMIRFloatType::F64,
            };
            std_coercion(builder, bc_operand, LMIRCoercionType::FloatToInt {
                from: from_type,
                sextend: *sextend,
            })
        },
        MIRCoercion::CStrToStr => Ok(bc_operand),
        MIRCoercion::GetFnPtr => {
            let LMIRValue::FunctionRef(func) = &bc_operand else {
                unreachable!("GetFnPtr coercion applied to non-function value");
            };
            
            builder.add_new_instruction(
                LMIRInstructionKind::GetFunctionAddr { 
                    func: func.to_string()
                },
                LMIRType::default_pointer(),
                true
            )
        },
    }
}