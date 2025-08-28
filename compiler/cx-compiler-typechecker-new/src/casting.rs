use cx_data_ast::parse::ast::CXCastType;
use cx_data_typechecker::cx_types::{same_type, CXType, CXTypeKind};
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_util::log_error;
use crate::structured_initialization::coerce_initializer_list;

pub(crate) fn coerce_value(expr: &mut TCExpr) {
    if let Some(inner) = expr._type.mem_ref_inner() {
        *expr = TCExpr {
            _type: inner.clone(),
            kind: TCExprKind::ImplicitLoad {
                operand: Box::new(std::mem::take(expr))
            }
        };
    }

    match &expr._type.kind {
        CXTypeKind::Function { prototype } => {
            *expr = TCExpr {
                _type: prototype.return_type.clone(),
                kind: TCExprKind::Coercion {
                    operand: Box::new(std::mem::take(expr)),
                    cast_type: CXCastType::FunctionToPointerDecay
                }
            };
        },

        _ => {}
    }
}

pub(crate) fn coerce_condition(expr: &mut TCExpr) -> Option<()> {
    coerce_value(expr);

    if !expr._type.is_integer() {
        try_implicit_cast(expr, &CXTypeKind::Integer { signed: false, bytes: 8 }.into())?;
    }

    Some(())
}

pub(crate) fn implicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    if matches!(expr.kind, TCExprKind::InitializerList { .. }) {
        return coerce_initializer_list(expr, to_type);
    };

    let Some(_) = try_implicit_cast(expr, to_type) else {
        println!("[DEBUG] Expr: {:#?}", expr);
        log_error!("TYPE ERROR: Cannot implicitly cast value of type {} to type {}", expr._type, to_type);
    };

    Some(())
}

pub(crate) fn try_implicit_cast(expr: &mut TCExpr, to_type: &CXType) -> Option<()> {
    if same_type(&expr._type, to_type) {
        return Some(())
    }

    let Some(cast_type) = valid_implicit_cast(&expr._type, to_type)? else {
        return None;
    };

    add_coercion(expr, to_type.clone(), cast_type);
    Some(())
}

pub fn add_coercion(expr: &mut TCExpr, to_type: CXType, cast_type: CXCastType) {
    *expr = TCExpr {
        _type: to_type,
        kind: TCExprKind::Coercion {
            operand: Box::new(std::mem::take(expr)),
            cast_type
        }
    }
}

pub fn valid_implicit_cast(from_type: &CXType, to_type: &CXType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (&from_type.kind, &to_type.kind) {
            (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { .. }) => {
                Some(CXCastType::PtrToInt)
            },

            (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. })
            => if b1 > b2 {
                Some(CXCastType::IntegralTrunc)
            } else if b1 < b2 {
                Some(CXCastType::IntegralCast)
            } else {
                Some(CXCastType::BitCast)
            },

            (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => Some(CXCastType::IntegralCast),

            (CXTypeKind::Float { .. }, CXTypeKind::Float { .. }) => Some(CXCastType::FloatCast),

            (CXTypeKind::Integer { .. }, CXTypeKind::Float { .. }) => Some(CXCastType::IntToFloat),
            (CXTypeKind::Float { .. }, CXTypeKind::Integer { .. }) => Some(CXCastType::FloatToInt),

            (CXTypeKind::MemoryReference(inner), CXTypeKind::Structured { .. })
                if same_type(inner.as_ref(), to_type) => Some(CXCastType::FauxLoad),

            (CXTypeKind::StrongPointer { .. }, CXTypeKind::StrongPointer { .. }) |
            (CXTypeKind::StrongPointer { .. }, CXTypeKind::PointerTo { .. }) |
            (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. })
            => Some(CXCastType::BitCast),

            (CXTypeKind::Array { inner_type: _type, .. }, CXTypeKind::PointerTo { inner_type: inner, .. })
            if same_type(_type, inner) => Some(CXCastType::BitCast),

            (CXTypeKind::Function { .. }, CXTypeKind::PointerTo { inner_type: inner, .. })
            if same_type(inner.as_ref(), from_type) => Some(CXCastType::FunctionToPointerDecay),

            _ => None
        }
    )
}

pub fn valid_explicit_cast(from_type: &CXType, to_type: &CXType) -> Option<Option<CXCastType>> {
    Some(
        match (&from_type.kind, &to_type.kind) {
            (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. })
            => Some(CXCastType::BitCast),

            (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { bytes, .. })
            if *bytes == 8 => Some(CXCastType::BitCast),

            (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { .. })
            => Some(CXCastType::IntegralTrunc),

            (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo { .. })
            => Some(CXCastType::IntToPtr),

            (CXTypeKind::PointerTo { .. }, CXTypeKind::StrongPointer { .. })
            => Some(CXCastType::BitCast),

            _ => None
        }
    )
}