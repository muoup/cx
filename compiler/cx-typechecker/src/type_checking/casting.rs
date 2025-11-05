use cx_parsing_data::ast::CXCastType;
use cx_typechecker_data::ast::{TCExpr, TCExprKind};
use cx_typechecker_data::cx_types::{CXType, CXTypeKind, same_type};
use cx_util::log_error;

use crate::type_checking::structured_initialization::coerce_initializer_list;

pub(crate) fn coerce_value(expr: &mut TCExpr) -> Option<()> {
    match &expr._type.kind {
        CXTypeKind::MemoryReference(inner) => {
            let inner = *inner.clone();
            implicit_cast(expr, &inner)
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

            Some(())
        }

        _ => Some(()),
    }
}

pub(crate) fn coerce_condition(expr: &mut TCExpr) -> Option<()> {
    coerce_value(expr);

    if !expr._type.is_integer() {
        try_implicit_cast(
            expr,
            &CXTypeKind::Integer {
                signed: false,
                bytes: 8,
            }
            .into(),
        )?;
    }

    Some(())
}

pub(crate) fn implicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    let Some(_) = try_implicit_cast(expr, to_type) else {
        log_error!(
            " Cannot implicitly cast value of type {} to type {}",
            expr._type,
            to_type
        );
    };

    Some(())
}

pub(crate) fn explicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    let Some(_) = try_explicit_cast(expr, to_type) else {
        log_error!(
            " Cannot explicitly cast value of type {} to type {}",
            expr._type,
            to_type
        );
    };

    Some(())
}

pub(crate) fn try_explicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    if try_implicit_cast(expr, to_type).is_some() {
        return Some(());
    }

    let cast_type = valid_explicit_cast(&expr._type, to_type)?;

    add_coercion(expr, to_type.clone(), cast_type);
    Some(())
}

pub fn add_coercion(expr: &mut TCExpr, to_type: CXType, cast_type: CXCastType) {
    *expr = TCExpr {
        _type: to_type,
        kind: TCExprKind::Coercion {
            operand: Box::new(std::mem::take(expr)),
            cast_type,
        },
    }
}

pub fn try_implicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    if matches!(expr.kind, TCExprKind::InitializerList { .. }) {
        return coerce_initializer_list(expr, to_type);
    }

    if same_type(&expr._type, to_type) {
        return Some(());
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

        (CXTypeKind::MemoryReference(inner), _)
            if inner.is_structured() =>
        {
            expr._type = *inner.clone();
            
            let Some(_) = try_implicit_cast(expr, to_type) else {
                return None;
            };
         
            return Some(());
        }

        (CXTypeKind::MemoryReference(inner), _) => {
            let mut loaded = TCExpr {
                _type: *inner.clone(),
                kind: TCExprKind::ImplicitLoad {
                    operand: Box::new(std::mem::take(expr)),
                },
            };

            let Some(_) = try_implicit_cast(&mut loaded, to_type) else {
                let TCExprKind::ImplicitLoad { operand } = loaded.kind else {
                    unreachable!();
                };

                *expr = *operand;
                return None;
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

        _ => return None,
    }

    Some(())
}

pub fn valid_explicit_cast(from_type: &CXType, to_type: &CXType) -> Option<CXCastType> {
    match (&from_type.kind, &to_type.kind) {
        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => Some(CXCastType::BitCast),

        (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { bytes, .. }) if *bytes == 8 => {
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
