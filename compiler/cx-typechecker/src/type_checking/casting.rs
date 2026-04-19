use cx_ast::ast::CXExpr;
use cx_ast::data::CX_CONST;
use cx_mir::mir::{
    data::{MIRIntegerType, MIRType, MIRTypeKind, same_type},
    expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRIntegerBinOp},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment, log_typecheck_error, type_checking::result::TypecheckResult,
};

pub(crate) fn coerce_value(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRExpression,
) -> CXResult<MIRExpression> {
    let value_type = value.get_type();
    let mem_ref_inner = env.type_context.mem_ref_inner(&value_type).cloned();

    let Some(mem_ref_inner) = mem_ref_inner else {
        return Ok(value);
    };

    // str is unsized — never auto-dereference &str
    if mem_ref_inner.is_str() {
        return Ok(value);
    }

    if !env.is_copyable(&mem_ref_inner) {
        return Ok(value);
    }

    implicit_cast(env, expr, value, &mem_ref_inner)
}

pub(crate) fn coerce_condition(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRExpression,
) -> CXResult<MIRExpression> {
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
            _type: MIRIntegerType::I1,
        }
        .into(),
    )
}

pub(crate) fn explicit_cast(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    if let Ok(val) = implicit_cast(env, expr, value.clone(), to_type) {
        return Ok(val);
    }

    let from_type = value.get_type();

    let coerce = |coercion_type: MIRCoercion| -> CXResult<MIRExpression> {
        Ok(TypecheckResult::type_conversion(
            TypecheckResult::from(value.clone()),
            coercion_type,
            to_type.clone(),
        )
        .into_expression())
    };

    match (&from_type.kind, &to_type.kind) {
        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. })
            if (*_type == MIRIntegerType::I64) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (MIRTypeKind::Integer { signed, .. }, MIRTypeKind::PointerTo { .. }) => {
            coerce(MIRCoercion::IntToPtr { sextend: *signed })
        }

        (
            MIRTypeKind::PointerTo {
                inner_type: ptr_inner,
            },
            MIRTypeKind::MemoryReference {
                inner_type: str_inner,
            },
        ) if matches!(
            env.type_context.get(*ptr_inner.as_ref()).map(|ty| &ty.kind),
            Some(MIRTypeKind::Integer {
                _type: MIRIntegerType::I8,
                ..
            })
        ) && matches!(
            env.type_context.get(*str_inner.as_ref()).map(|ty| &ty.kind),
            Some(MIRTypeKind::Str)
        ) =>
        {
            coerce(MIRCoercion::CStrToStr)
        }

        (MIRTypeKind::MemoryReference { inner_type }, _) => {
            let inner_type = env
                .type_context
                .get(*inner_type.as_ref())
                .unwrap_or_else(|| panic!("Unknown type id {}", inner_type.0))
                .clone();
            cast_recurse(env, expr, value, to_type, &inner_type, explicit_cast)
        }

        _ => {
            log_typecheck_error!(
                env,
                expr.token_range(),
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
    value: MIRExpression,
    to_type: &MIRType,
) -> CXResult<MIRExpression> {
    let from_type = value.get_type();

    if same_type(&from_type, to_type) {
        return Ok(value);
    }

    let coerce = |coercion_type: MIRCoercion| -> CXResult<MIRExpression> {
        Ok(MIRExpression {
            token_range: None,
            _type: to_type.clone(),
            kind: MIRExpressionKind::TypeConversion {
                operand: Box::new(value.clone()),
                conversion: coercion_type,
            },
        })
    };

    match (&from_type.kind, &to_type.kind) {
        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::Integer { _type, .. }) => {
            coerce(MIRCoercion::PtrToInt { to_type: *_type })
        }

        (
            MIRTypeKind::Integer { _type, .. },
            MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                ..
            },
        ) => {
            // Convert integer to boolean by comparing with 0
            let zero = MIRExpression::int_literal(0, *_type, false);
            Ok(TypecheckResult::binary_op(
                TypecheckResult::from(value),
                TypecheckResult::from(zero),
                MIRBinOp::Integer {
                    itype: *_type,
                    op: MIRIntegerBinOp::NE,
                },
                to_type.clone(),
            )
            .into_expression())
        }

        (
            MIRTypeKind::Integer { _type: t1, .. },
            MIRTypeKind::Integer {
                _type: t2,
                signed: s2,
                ..
            },
        ) => coerce(MIRCoercion::Integral {
            from_type: *t1,
            to_type: *t2,
            sextend: *s2,
        }),

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
            .map(|(from_inner, to_inner)| same_type(from_inner, to_inner))
            .unwrap_or(false) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: from_inner,
            },
            MIRTypeKind::MemoryReference {
                inner_type: to_inner,
            },
        ) if {
            let from_inner = env.type_context.get(*from_inner.as_ref()).unwrap();
            let to_inner = env.type_context.get(*to_inner.as_ref()).unwrap();
            let from_unconst = from_inner.without_specifier(CX_CONST);
            let to_unconst = to_inner.without_specifier(CX_CONST);

            same_type(&from_unconst, &to_unconst)
                && !from_inner.get_specifier(CX_CONST)
                && to_inner.get_specifier(CX_CONST)
        } =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: inner1, ..
            },
            MIRTypeKind::PointerTo {
                inner_type: inner2, ..
            },
        ) if env
            .type_context
            .get(*inner1.as_ref())
            .zip(env.type_context.get(*inner2.as_ref()))
            .map(|(inner1, inner2)| same_type(inner1, inner2))
            .unwrap_or(false) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: inner1, ..
            },
            MIRTypeKind::PointerTo {
                inner_type: inner2, ..
            },
        ) if env
            .type_context
            .get(*inner1.as_ref())
            .map(|ty| ty.is_array())
            .unwrap_or(false) =>
        {
            let inner1_inner = env
                .type_context
                .get(*inner1.as_ref())
                .and_then(|ty| env.type_context.array_inner(ty))
                .unwrap();

            if env
                .type_context
                .get(*inner2.as_ref())
                .map(|inner2| same_type(inner1_inner, inner2))
                .unwrap_or(false)
            {
                coerce(MIRCoercion::ReinterpretBits)
            } else {
                log_typecheck_error!(
                    env,
                    expr.token_range(),
                    "No implicit cast from {} to {}",
                    from_type,
                    to_type
                )
            }
        }

        // &str -> char* / const char* coercion (not allowed in safe context)
        (
            MIRTypeKind::MemoryReference {
                inner_type: from_inner,
                ..
            },
            MIRTypeKind::PointerTo {
                inner_type: to_inner,
                ..
            },
        ) if env
            .type_context
            .get(*from_inner.as_ref())
            .map(|ty| ty.is_str())
            .unwrap_or(false)
            && matches!(
                env.type_context.get(*to_inner.as_ref()).map(|ty| &ty.kind),
                Some(MIRTypeKind::Integer {
                    _type: MIRIntegerType::I8,
                    ..
                })
            ) =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::MemoryReference {
                inner_type: inner, ..
            },
            _,
        ) => {
            let inner = env
                .type_context
                .get(*inner.as_ref())
                .unwrap_or_else(|| panic!("Unknown type id {}", inner.0))
                .clone();
            cast_recurse(env, expr, value, to_type, &inner, implicit_cast)
        }

        (
            _,
            MIRTypeKind::MemoryReference {
                inner_type: inner, ..
            },
        )
        | (
            _,
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if env
            .type_context
            .get(*inner.as_ref())
            .map(|inner| same_type(inner, &from_type))
            .unwrap_or(false)
            && from_type.is_memory_resident() =>
        {
            coerce(MIRCoercion::ReinterpretBits)
        }

        (
            MIRTypeKind::Array {
                inner_type: _type, ..
            },
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if env
            .type_context
            .get(*_type.as_ref())
            .zip(env.type_context.get(*inner.as_ref()))
            .map(|(lhs, rhs)| same_type(lhs, rhs))
            .unwrap_or(false) =>
        {
            // Array to pointer decay: access element 0
            let inner_type = env.type_context.get(*inner.as_ref()).unwrap().clone();
            Ok(TypecheckResult::array_access(
                TypecheckResult::from(value),
                TypecheckResult::from(MIRExpression::int_literal(0, MIRIntegerType::I64, false)),
                inner_type,
                to_type.clone(),
            )
            .into_expression())
        }

        (
            MIRTypeKind::Function { .. },
            MIRTypeKind::PointerTo {
                inner_type: inner, ..
            },
        ) if env
            .type_context
            .get(*inner.as_ref())
            .map(|inner| same_type(inner, &from_type))
            .unwrap_or(false) =>
        {
            coerce(MIRCoercion::GetFnPtr)
        }

        _ => {
            log_typecheck_error!(
                env,
                expr.token_range(),
                "No implicit cast from {} to {}",
                from_type,
                to_type
            )
        }
    }
}

fn cast_recurse<Func>(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    value: MIRExpression,
    to_type: &MIRType,
    inner_type: &MIRType,
    cast: Func,
) -> CXResult<MIRExpression>
where
    Func: Fn(&mut TypeEnvironment, &CXExpr, MIRExpression, &MIRType) -> CXResult<MIRExpression>,
{
    if !env.is_copyable(inner_type) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Cannot implicitly copy value of type {}",
            inner_type,
        );
    }

    if inner_type.is_memory_resident() {
        let copied =
            TypecheckResult::copy_region(TypecheckResult::from(value.clone()), inner_type.clone());
        cast(env, expr, copied.into_expression(), to_type)
    } else {
        // Need to read from memory reference and then cast
        let loaded =
            TypecheckResult::memory_read(TypecheckResult::from(value.clone()), inner_type.clone());
        cast(env, expr, loaded.into_expression(), to_type)
    }
}
