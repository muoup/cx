use cx_parsing_data::ast::CXExpr;
use cx_typechecker_data::mir::{
    expression::{MIRCoercion, MIRInstruction, MIRValue},
    types::{CXIntegerType, MIRType, MIRTypeKind, same_type},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment, log_typecheck_error, type_checking::binary_ops::handle_assignment,
};

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
) -> CXResult<MIRValue> {
    let value_type = value.get_type();
    let mem_ref_inner = value_type.mem_ref_inner();

    if let Some(mem_ref_inner) = mem_ref_inner {
        implicit_cast(env, expr, value, mem_ref_inner)
    } else {
        Ok(value)
    }
    
    // match &inner_type.kind {
    //     // There are certain types where a memory reference to them is essentially equivalent to
    //     // their plain type. If we for instance have an array type, in memory, that is a pointer to
    //     // the base of the array, and if we have a memory reference to that, it is still just a pointer to the
    //     // base of the array. And for most semantic purposes, if we actually want to use either a &int[4] or a int[4],
    //     // it makes sense to treat them both as an int*
    //     CXTypeKind::Array {
    //         inner_type: element_type,
    //         ..
    //     } => {
    //         let pointer_to = element_type.clone().pointer_to();
    //         let new_register = env.builder.new_register();

    //         env.builder.add_instruction(MIRInstruction::ArrayGet {
    //             result: new_register.clone(),
    //             source: value,
    //             index: MIRValue::IntLiteral {
    //                 value: 0,
    //                 signed: false,
    //                 _type: CXIntegerType::I64,
    //             },
    //             array_type: inner_type.clone(),
    //             element_type: *element_type.clone(),
    //         });

    //         Ok(MIRValue::Register {
    //             register: new_register,
    //             _type: pointer_to,
    //         })
    //     }

    //     CXTypeKind::Function { prototype, .. } => Ok(MIRValue::FunctionReference {
    //         prototype: *prototype.clone(),
    //         implicit_variables: vec![],
    //     }),

    //     _ => {
    //         if let Some(inner) = mem_ref_inner {
    //             implicit_cast(env, expr, value, &inner.clone())
    //         } else {
    //             Ok(value)
    //         }
    //     }
    // }
}

pub(crate) fn coerce_condition(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
) -> CXResult<MIRValue> {
    let value = coerce_value(env, expr, value)?;

    if value.get_type().is_integer() {
        return Ok(value);
    }

    implicit_cast(
        env,
        expr,
        value,
        &MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        }
        .into(),
    )
}

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
    to_type: &MIRType,
) -> CXResult<MIRValue> {
    if let Ok(val) = implicit_cast(env, expr, value.clone(), to_type) {
        return Ok(val);
    }

    let from_type = value.get_type();
    let coerce = |coercion_type: MIRCoercion| {
        let result = env.builder.new_register();
        env.builder.add_instruction(MIRInstruction::Coercion {
            result: result.clone(),
            operand: value,
            cast_type: coercion_type,
        });

        Ok(MIRValue::Register {
            register: result,
            _type: to_type.clone(),
        })
    };

    match (&from_type.kind, &to_type.kind) {
        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. })
            if (*_type == CXIntegerType::I64) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::IntToPtr { sextend: *signed })
        }

        _ => {
            log_typecheck_error!(
                env,
                expr,
                "No explicit cast from {} to {}",
                from_type,
                to_type
            )
        }
    }
}

pub fn implicit_cast(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
    to_type: &MIRType,
) -> CXResult<MIRValue> {
    let from_type = value.get_type();

    if same_type(&from_type, to_type) {
        return Ok(value);
    }

    let mut coerce = |coercion_type: MIRCoercion| {
        let result = env.builder.new_register();
        env.builder.add_instruction(MIRInstruction::Coercion {
            result: result.clone(),
            operand: value.clone(),
            cast_type: coercion_type,
        });

        Ok(MIRValue::Register {
            register: result,
            _type: to_type.clone(),
        })
    };

    match (&from_type.kind, &to_type.kind) {
        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (
            MIRTypeKind::Integer { _type: t1, .. },
            MIRTypeKind::Integer {
                _type: t2,
                signed: s2,
                ..
            },
        ) => {
            if t1.bytes() != t2.bytes() {
                coerce(MIRCoercion::Integral {
                    to_type: *t2,
                    sextend: *s2,
                })
            } else {
                coerce(MIRCoercion::ReinterpretBits)
            }
        }

        (MIRTypeKind::Integer { .. }, MIRTypeKind::Bool) => coerce(MIRCoercion::IntToBool),
        (MIRTypeKind::Bool, MIRTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::BoolToInt { to_type: *_type })
        }
        (MIRTypeKind::Float { .. }, MIRTypeKind::Float { _type: to_type }) => {
            coerce(MIRCoercion::FloatCast { to_type: *to_type })
        }
        (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::Float { _type, .. }) => {
            coerce(MIRCoercion::IntToFloat {
                to_type: *_type,
                sextend: *signed,
            })
        }
        (MIRTypeKind::Float { .. }, MIRTypeKind::Integer { _type, signed, .. }) => {
            coerce(MIRCoercion::FloatToInt {
                to_type: *_type,
                sextend: *signed,
            })
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::MemoryReference(inner1),
            MIRTypeKind::PointerTo {
                inner_type: inner2, ..
            },
        ) if same_type(inner1.as_ref(), inner2.as_ref()) => coerce(MIRCoercion::ReinterpretBits),

        (
            MIRTypeKind::MemoryReference(inner1),
            MIRTypeKind::PointerTo { inner_type: inner2, .. }
        ) if inner1.is_array() => {
            let inner1_inner = inner1.array_inner().unwrap();
            
            if same_type(inner1_inner, inner2.as_ref()) {
                coerce(MIRCoercion::ReinterpretBits)
            } else {
                log_typecheck_error!(
                    env,
                    expr,
                    "No implicit cast from {} to {}",
                    from_type,
                    to_type
                )
            }
        },
        
        (MIRTypeKind::MemoryReference(inner), _) => {
            if !env.is_copyable(inner) {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Cannot implicitly copy value of type {} to type {}",
                    from_type,
                    to_type
                );
            }

            let result = env.builder.new_register();
            let result_value = MIRValue::Register {
                register: result.clone(),
                _type: *inner.clone(),
            };

            if inner.is_memory_resident() {
                env.builder
                    .add_instruction(MIRInstruction::CreateStackRegion {
                        result: result.clone(),
                        _type: *inner.clone(),
                    });
                handle_assignment(env, &result_value, &value, inner)?;
            } else {
                env.builder.add_instruction(MIRInstruction::MemoryRead {
                    result,
                    source: value,
                    _type: *inner.clone(),
                })
            }

            implicit_cast(env, expr, result_value, to_type)
        }

        (_, MIRTypeKind::MemoryReference(inner))
        | (
            _,
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) && from_type.is_memory_resident() => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::Array {
                inner_type: _type, ..
            },
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(_type, inner) => {
            let result = env.builder.new_register();
            env.builder.add_instruction(MIRInstruction::ArrayGet {
                result: result.clone(),
                source: value,
                index: MIRValue::IntLiteral {
                    value: 0,
                    signed: false,
                    _type: CXIntegerType::I64,
                },
                array_type: from_type.clone(),
                element_type: *inner.clone(),
            });

            Ok(MIRValue::Register {
                register: result,
                _type: to_type.clone(),
            })
        }

        (
            MIRTypeKind::Function { .. },
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) => Ok(value),

        _ => {
            log_typecheck_error!(
                env,
                expr,
                "No implicit cast from {} to {}",
                from_type,
                to_type
            )
        }
    }
}
