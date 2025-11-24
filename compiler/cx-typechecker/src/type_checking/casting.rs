use cx_parsing_data::ast::{CXCastType, CXExpr};
use cx_typechecker_data::{
    ast::{TCExpr, TCExprKind},
    mir::{
        expression::{MIRCoercion, MIRInstruction, MIRValue},
        types::{CXIntegerType, CXType, CXTypeKind, same_type},
    },
};
use cx_util::{CXError, CXResult, log_error};

use crate::{
    environment::TCEnvironment, log_typecheck_error
};

pub(crate) fn coerce_value(env: &mut TCEnvironment, expr: &CXExpr, mut value: MIRValue) -> CXResult<MIRValue> {
    if let Some(inner) = value.get_type().mem_ref_inner() {
        value = implicit_cast(env, expr, value, inner)?;
    }

    match &value.get_type().kind {
        CXTypeKind::Array { inner_type, .. } => {
            let pointer_to = inner_type.clone().pointer_to();
            let new_register = env.builder.new_register();
            
            env.builder.add_instruction(
                MIRInstruction::Coercion {
                    result: new_register.clone(),
                    operand: value,
                    cast_type: MIRCoercion::ArrayToPointerDecay,
                }
            );

            Ok(MIRValue::Register {
                register: new_register,
                _type: pointer_to,
            })
        }

        CXTypeKind::Function { .. } => {
            let pointer_to = expr._type.clone().pointer_to();

            *expr = TCExpr {
                _type: pointer_to,
                kind: TCExprKind::Coercion {
                    operand: Box::new(std::mem::take(expr)),
                    cast_type: CXCastType::FunctionToPointerDecay,
                },
            };

            Ok(())
        }

        _ => Ok(()),
    }
}

pub(crate) fn coerce_condition(
    expr: &CXExpr,
    value: MIRValue,
) -> CXResult<MIRValue> {
    let value = coerce_value(value)?;

    if value.get_type().is_integer() {
        return Ok(value);
    }

    implicit_cast(
        expr,
        value,
        _type,
        &CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        }
        .into(),
    )
}

pub(crate) fn explicit_cast(
    env: &mut TCEnvironment,
    expr: &CXExpr,
    value: MIRValue,
    to_type: &CXType,
) -> CXResult<MIRValue> {
    if let Ok(val) = implicit_cast(expr, value.clone(), _type, to_type) {
        return Ok(val);
    }

    let Some(cast_type) = valid_explicit_cast(_type, to_type) else {
        return log_typecheck_error!(env, expr, "No explicit cast from {} to {}", _type, to_type);
    };
    
}

pub fn implicit_cast(
    env: &mut TCEnvironment,
    expr: &CXExpr,
    value: MIRValue,
    to_type: &CXType,
) -> CXResult<MIRValue> {
    if same_type(_type, to_type) {
        return Ok(value);
    }

    let from_type = expr._type.clone();
    let mut coerce = |coercion_type: CXCastType| {
        add_coercion(expr, to_type.clone(), coercion_type);
    };

    match (&from_type.kind, &to_type.kind) {
        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { .. }) => coerce(CXCastType::PtrToInt),

        (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. }) => {
            if b1 > b2 {
                coerce(CXCastType::IntegralTrunc)
            } else if b1 < b2 {
                coerce(CXCastType::IntegralCast)
            } else {
                coerce(CXCastType::BitCast)
            }
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => coerce(CXCastType::IntToBool),
        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => coerce(CXCastType::IntegralCast),
        (CXTypeKind::Float { .. }, CXTypeKind::Float { .. }) => coerce(CXCastType::FloatCast),
        (CXTypeKind::Integer { .. }, CXTypeKind::Float { .. }) => coerce(CXCastType::IntToFloat),
        (CXTypeKind::Float { .. }, CXTypeKind::Integer { .. }) => coerce(CXCastType::FloatToInt),

        (CXTypeKind::StrongPointer { .. }, CXTypeKind::StrongPointer { .. })
        | (CXTypeKind::StrongPointer { .. }, CXTypeKind::PointerTo { .. })
        | (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => {
            coerce(CXCastType::BitCast)
        }

        (CXTypeKind::MemoryReference(inner), _)
            if same_type(inner.as_ref(), to_type) && to_type.has_move_semantics() =>
        {
            if !inner.copyable() {
                log_error!(
                    "Cannot implicitly move value of type {} to type {}",
                    from_type,
                    to_type
                );
            }

            *expr = TCExpr {
                _type: *inner.clone(),
                kind: TCExprKind::Copy {
                    expr: Box::new(std::mem::take(expr)),
                },
            };
        }

        (CXTypeKind::MemoryReference(inner), _) if inner.is_structured() => {
            expr._type = *inner.clone();

            implicit_cast(expr, to_type)?;
        }

        (CXTypeKind::MemoryReference(inner), _) => {
            let mut loaded = TCExpr {
                _type: *inner.clone(),
                kind: TCExprKind::ImplicitLoad {
                    operand: Box::new(std::mem::take(expr)),
                },
            };

            let Ok(_) = implicit_cast(&mut loaded, to_type) else {
                let TCExprKind::ImplicitLoad { operand } = loaded.kind else {
                    unreachable!();
                };

                *expr = *operand;
                return CXError::create_result(format!(
                    "No implicit cast from {} to {}",
                    from_type, to_type
                ));
            };

            *expr = loaded;
        }

        (_, CXTypeKind::MemoryReference(inner))
        | (
            _,
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) && from_type.is_structured() => {
            coerce(CXCastType::Reinterpret)
        }

        (
            CXTypeKind::Array {
                inner_type: _type, ..
            },
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(_type, inner) => coerce(CXCastType::NOOP),

        (
            CXTypeKind::Function { .. },
            CXTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if same_type(inner.as_ref(), &from_type) => coerce(CXCastType::FunctionToPointerDecay),

        _ => log_typecheck_error!(expr, "No implicit cast from {} to {}", from_type, to_type),
    }

    Ok(Some(()))
}

pub fn valid_explicit_cast(from_type: &CXType, to_type: &CXType) -> Option<CXCastType> {
    match (&from_type.kind, &to_type.kind) {
        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => Some(CXCastType::BitCast),

        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { _type, .. }) if (*_type == CXIntegerType::I64) => {
            Some(CXCastType::BitCast)
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { .. }) => {
            Some(CXCastType::IntegralTrunc)
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo { .. }) => Some(CXCastType::IntToPtr),

        (CXTypeKind::PointerTo { .. }, CXTypeKind::StrongPointer { .. }) => {
            Some(CXCastType::BitCast)
        }

        _ => None,
    }
}
