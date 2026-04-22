use cx_ast::data::CX_CONST;
use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp,
        MIRPtrBinOp,
    },
    r#type::{MIRIntegerType, MIRType, MIRTypeKind, same_type},
};
use cx_util::unsafe_float::FloatWrapper;
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{coercion::CoercionResult, result::TypecheckResult},
};

pub mod array_to_ptr;
pub mod fn_to_ptr;
pub mod integer;
pub mod lvalue;

pub fn std_rval_promotion(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    match array_to_ptr::try_conversion(env, expr)
        .or_else(|expr| fn_to_ptr::try_conversion(env, expr))
        .or_else(|expr| lvalue::try_conversion(env, expr))
    {
        // If we successfully transformed the value, we should try to apply the same transformation again
        CoercionResult::Success {
            expr: transformed, ..
        } => std_rval_promotion(env, transformed),

        // If no transformation was applied, we can return the original expression as the final result
        CoercionResult::CastNotFound(expr) => Ok(expr),

        // If an error was encountered during transformation, we should log it and return the original expression as the final result to avoid cascading errors
        CoercionResult::Error { message, expr } => log_typecheck_error!(
            env,
            expr.token_range,
            "Error: {}",
            message
        ),
    }
}

fn coercion_expr(
    expr: MIRExpression,
    target_type: MIRType,
    conversion: MIRCoercion,
) -> CoercionResult {
    let coerced = MIRExpression {
        token_range: expr.token_range.clone(),
        _type: target_type,
        kind: MIRExpressionKind::TypeConversion {
            operand: Box::new(expr),
            conversion,
        },
    };

    CoercionResult::some(coerced)
}

fn compare_to_zero(expr: MIRExpression) -> CoercionResult {
    let result_type = MIRType::bool();

    match expr.get_type().kind {
        MIRTypeKind::Integer { _type, .. } => CoercionResult::some(
            TypecheckResult::binary_op(
                TypecheckResult::from(expr),
                TypecheckResult::from(MIRExpression::int_literal(0, _type, false)),
                MIRBinOp::Integer {
                    itype: _type,
                    op: MIRIntegerBinOp::NE,
                },
                result_type,
            )
            .into_expression(),
        ),

        MIRTypeKind::Float { _type } => {
            let zero = MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::FloatLiteral(FloatWrapper::from(0.0), _type),
                _type: MIRType::from(MIRTypeKind::Float { _type }),
            };

            CoercionResult::some(
                TypecheckResult::binary_op(
                    TypecheckResult::from(expr),
                    TypecheckResult::from(zero),
                    MIRBinOp::Float {
                        ftype: _type,
                        op: MIRFloatBinOp::FNE,
                    },
                    result_type,
                )
                .into_expression(),
            )
        }

        MIRTypeKind::PointerTo { .. } => {
            let zero_type = MIRIntegerType::from_bytes(std::mem::size_of::<usize>() as u8).unwrap();

            CoercionResult::some(
                TypecheckResult::binary_op(
                    TypecheckResult::from(expr),
                    TypecheckResult::from(MIRExpression::int_literal(0, zero_type, false)),
                    MIRBinOp::Pointer {
                        op: MIRPtrBinOp::NE,
                    },
                    result_type,
                )
                .into_expression(),
            )
        }

        _ => CoercionResult::none(expr),
    }
}

fn compatible_reference_inner(from_inner: &MIRType, to_inner: &MIRType) -> bool {
    if same_type(from_inner, to_inner) {
        return true;
    }

    let from_unconst = from_inner.without_specifier(CX_CONST);
    let to_unconst = to_inner.without_specifier(CX_CONST);

    same_type(&from_unconst, &to_unconst)
        && !from_inner.get_specifier(CX_CONST)
        && to_inner.get_specifier(CX_CONST)
}

fn compatible_pointer_inner(from_inner: &MIRType, to_inner: &MIRType) -> bool {
    if same_type(from_inner, to_inner) {
        return true;
    }

    let from_unconst = from_inner.without_specifier(CX_CONST);
    let to_unconst = to_inner.without_specifier(CX_CONST);

    if same_type(&from_unconst, &to_unconst)
        && !from_inner.get_specifier(CX_CONST)
        && to_inner.get_specifier(CX_CONST)
    {
        return true;
    }

    !from_inner.is_function()
        && !to_inner.is_function()
        && (from_unconst.is_unit() || to_unconst.is_unit())
}

fn decay_array_value(env: &mut TypeEnvironment, expr: MIRExpression) -> CoercionResult {
    let Some(element_type) = env.type_context.array_inner(&expr._type).cloned() else {
        return CoercionResult::none(expr);
    };

    let new_type = env.type_context.pointer_to(element_type.clone());
    let decayed = TypecheckResult::array_access(
        TypecheckResult::from(expr),
        TypecheckResult::from(MIRExpression::int_literal(0, MIRIntegerType::I64, false)),
        element_type,
        new_type,
    )
    .into_expression();

    CoercionResult::some(decayed)
}

fn recurse_lvalue(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CoercionResult {
    match lvalue::try_conversion(env, expr) {
        CoercionResult::Success {
            expr: transformed, ..
        } => try_implicit_cast(env, transformed, target_type),

        CoercionResult::Error { message, expr } => CoercionResult::error(message, expr),

        CoercionResult::CastNotFound(expr) => {
            let Some(mem_inner) = env.type_context.mem_ref_inner(&expr._type).cloned() else {
                return CoercionResult::none(expr);
            };

            if !mem_inner.is_array() {
                return CoercionResult::none(expr);
            }

            if !env.is_copyable(&mem_inner) {
                return CoercionResult::error(
                    format!("Cannot implicitly copy value of type {}", mem_inner),
                    expr,
                );
            }

            let result_type = mem_inner.without_specifier(CX_CONST);
            let copied = MIRExpression {
                token_range: expr.token_range.clone(),
                _type: result_type.clone(),
                kind: MIRExpressionKind::RegionDuplicate {
                    source: Box::new(expr),
                    _type: result_type.clone(),
                },
            };

            try_implicit_cast(env, copied, target_type)
        }
    }
}

pub fn try_implicit_cast(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
    target_type: &MIRType,
) -> CoercionResult {
    let from_type = expr.get_type();

    if same_type(&from_type, target_type) {
        return CoercionResult::some(expr);
    }

    if !from_type.is_memory_reference()
        && !target_type.is_memory_reference()
        && same_type(
            &from_type.without_specifier(CX_CONST),
            &target_type.without_specifier(CX_CONST),
        )
    {
        return CoercionResult::some(expr);
    }

    if from_type.is_function() {
        return match fn_to_ptr::try_conversion(env, expr) {
            CoercionResult::Success {
                expr: transformed, ..
            } => try_implicit_cast(env, transformed, target_type),
            other => other,
        };
    }

    if from_type.is_array() {
        return match decay_array_value(env, expr) {
            CoercionResult::Success {
                expr: transformed, ..
            } => try_implicit_cast(env, transformed, target_type),
            other => other,
        };
    }

    match (&from_type.kind, &target_type.kind) {
        (
            MIRTypeKind::MemoryReference {
                inner_type: from_inner,
            },
            MIRTypeKind::MemoryReference {
                inner_type: to_inner,
            },
        ) if env
            .type_context
            .get(*from_inner.as_ref())
            .zip(env.type_context.get(*to_inner.as_ref()))
            .map(|(from_inner, to_inner)| compatible_reference_inner(from_inner, to_inner))
            .unwrap_or(false) =>
        {
            return coercion_expr(expr, target_type.clone(), MIRCoercion::ReinterpretBits);
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: from_inner,
            },
            MIRTypeKind::PointerTo {
                inner_type: to_inner,
            },
        ) if matches!(
            env.type_context
                .get(*from_inner.as_ref())
                .map(|ty| &ty.kind),
            Some(MIRTypeKind::Str)
        ) && matches!(
            env.type_context.get(*to_inner.as_ref()).map(|ty| &ty.kind),
            Some(MIRTypeKind::Integer {
                _type: MIRIntegerType::I8,
                ..
            })
        ) =>
        {
            return coercion_expr(expr, target_type.clone(), MIRCoercion::ReinterpretBits);
        }

        _ => {}
    }

    if let Some(mem_inner) = env.type_context.mem_ref_inner(&from_type).cloned() {
        if mem_inner.is_array()
            && !matches!(
                target_type.kind,
                MIRTypeKind::Array { .. } | MIRTypeKind::MemoryReference { .. }
            )
        {
            return match array_to_ptr::try_conversion(env, expr) {
                CoercionResult::Success {
                    expr: transformed, ..
                } => try_implicit_cast(env, transformed, target_type),
                other => other,
            };
        }

        return recurse_lvalue(env, expr, target_type);
    }

    if matches!(
        target_type.kind,
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I1,
            signed: false,
        }
    ) {
        return compare_to_zero(expr);
    }

    if let integer_cast @ CoercionResult::Success { .. } =
        integer::try_conversion(env, expr.clone(), target_type)
    {
        return integer_cast;
    }

    match (&from_type.kind, &target_type.kind) {
        (MIRTypeKind::Float { _type: from_float }, MIRTypeKind::Float { _type: to_float }) => {
            if from_float == to_float {
                let coerced = MIRExpression {
                    token_range: expr.token_range.clone(),
                    _type: target_type.clone(),
                    kind: MIRExpressionKind::Typechange(Box::new(expr)),
                };

                CoercionResult::some(coerced)
            } else {
                coercion_expr(
                    expr,
                    target_type.clone(),
                    MIRCoercion::FloatCast { to_type: *to_float },
                )
            }
        }

        (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::Float { _type }) => coercion_expr(
            expr,
            target_type.clone(),
            MIRCoercion::IntToFloat {
                to_type: *_type,
                sextend: *signed,
            },
        ),

        (MIRTypeKind::Float { .. }, MIRTypeKind::Integer { _type, signed }) => coercion_expr(
            expr,
            target_type.clone(),
            MIRCoercion::FloatToInt {
                to_type: *_type,
                sextend: *signed,
            },
        ),

        (
            MIRTypeKind::PointerTo {
                inner_type: from_inner,
            },
            MIRTypeKind::PointerTo {
                inner_type: to_inner,
            },
        ) if env
            .type_context
            .get(*from_inner.as_ref())
            .zip(env.type_context.get(*to_inner.as_ref()))
            .map(|(from_inner, to_inner)| compatible_pointer_inner(from_inner, to_inner))
            .unwrap_or(false) =>
        {
            coercion_expr(expr, target_type.clone(), MIRCoercion::ReinterpretBits)
        }

        (_, MIRTypeKind::MemoryReference { inner_type })
            if env
                .type_context
                .get(*inner_type.as_ref())
                .map(|inner| same_type(inner, &from_type))
                .unwrap_or(false)
                && from_type.is_memory_resident() =>
        {
            coercion_expr(expr, target_type.clone(), MIRCoercion::ReinterpretBits)
        }

        _ => CoercionResult::none(expr),
    }
}
