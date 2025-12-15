use cx_parsing_data::ast::CXExpr;
use cx_typechecker_data::mir::{
    expression::{MIRCoercion, MIRInstruction, MIRValue},
    types::{CXIntegerType, CXType, CXTypeKind, same_type},
};
use cx_util::CXResult;

use crate::{environment::TypeEnvironment, log_typecheck_error};

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
) -> CXResult<MIRValue> {
    if let Some(inner) = value.get_type().mem_ref_inner() {
        match &inner.kind {
            CXTypeKind::Array { inner_type, .. } => {
                let pointer_to = inner_type.clone().pointer_to();
                let new_register = env.builder.new_register();

                env.builder.add_instruction(MIRInstruction::Coercion {
                    result: new_register.clone(),
                    operand: value,
                    cast_type: MIRCoercion::ReinterpretBits,
                });

                Ok(MIRValue::Register {
                    register: new_register,
                    _type: pointer_to,
                })
            }

            CXTypeKind::Function { prototype, .. } => Ok(MIRValue::FunctionReference {
                prototype: *prototype.clone(),
                implicit_variables: vec![],
            }),

            _ => implicit_cast(env, expr, value, inner)
        }
    } else {
        Ok(value)
    }
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
        &CXTypeKind::Integer {
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
    to_type: &CXType,
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
        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { _type, .. })
            if (*_type == CXIntegerType::I64) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (CXTypeKind::Integer { signed, .. }, CXTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::IntToPtr { sextend: *signed })
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::StrongPointer { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                "No explicit cast from {} to {}",
                from_type,
                to_type
            );
        }
    }
}

pub fn implicit_cast(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRValue,
    to_type: &CXType,
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
        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (
            CXTypeKind::Integer {
                _type: t1,
                signed: s1,
                ..
            },
            CXTypeKind::Integer {
                _type: t2,
                signed: s2,
                ..
            },
        ) => {
            if t1.bytes() > t2.bytes() {
                coerce(MIRCoercion::Integral {
                    to_type: *t1,
                    sextend: *s1,
                })
            } else if t1.bytes() < t2.bytes() {
                coerce(MIRCoercion::Integral {
                    to_type: *t2,
                    sextend: *s2,
                })
            } else {
                coerce(MIRCoercion::ReinterpretBits)
            }
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => coerce(MIRCoercion::IntToBool),
        (CXTypeKind::Bool, CXTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::BoolToInt { to_type: *_type })
        }
        (CXTypeKind::Float { .. }, CXTypeKind::Float { _type: to_type }) => {
            coerce(MIRCoercion::FloatCast { to_type: *to_type })
        }
        (CXTypeKind::Integer { signed, .. }, CXTypeKind::Float { _type, .. }) => {
            coerce(MIRCoercion::IntToFloat {
                to_type: *_type,
                sextend: *signed,
            })
        }
        (CXTypeKind::Float { .. }, CXTypeKind::Integer { _type, signed, .. }) => {
            coerce(MIRCoercion::FloatToInt {
                to_type: *_type,
                sextend: *signed,
            })
        }

        (CXTypeKind::StrongPointer { .. }, CXTypeKind::StrongPointer { .. })
        | (CXTypeKind::StrongPointer { .. }, CXTypeKind::PointerTo { .. })
        | (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (CXTypeKind::MemoryReference(inner), _) => {
            if !inner.copyable() {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Cannot implicitly copy value of type {} to type {}",
                    from_type,
                    to_type
                );
            }

            let result = env.builder.new_register();
            let MIRValue::Register { register, .. } = &value else {
                return log_typecheck_error!(
                    env,
                    expr,
                    "Expected register value for memory reference copy, got {}",
                    value
                );
            };

            if inner.is_structured() {
                env.builder
                    .add_instruction(MIRInstruction::CreateStackRegion {
                        result: result.clone(),
                        _type: *inner.clone(),
                    });

                env.builder.add_instruction(MIRInstruction::CopyRegionInto {
                    destination: result.clone(),
                    source: register.clone(),
                    _type: *inner.clone(),
                });
            } else {
                env.builder.add_instruction(MIRInstruction::MemoryRead {
                    result: result.clone(),
                    source: value,
                    _type: *inner.clone(),
                })
            }

            let result_value = MIRValue::Register {
                register: result,
                _type: *inner.clone()
            };
            
            implicit_cast(env, expr, result_value, to_type)
        }

        (_, CXTypeKind::MemoryReference(inner))
        | (
            _,
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) && from_type.is_structured() => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            CXTypeKind::Array {
                inner_type: _type, ..
            },
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(_type, inner) => coerce(MIRCoercion::ReinterpretBits),

        (
            CXTypeKind::Function { .. },
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) => Ok(value),

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                "No implicit cast from {} to {}",
                from_type,
                to_type
            );
        }
    }
}
