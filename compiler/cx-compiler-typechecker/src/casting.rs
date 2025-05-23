use std::clone;
use std::ops::Deref;
use crate::TypeEnvironment;
use cx_data_ast::parse::ast::{CXCastType, CXExpr, CXExprKind};
use cx_data_ast::parse::value_type::{same_type, CXTypeKind, CXType};
use cx_util::log_error;
use crate::checker::coerce_value;

pub fn valid_implicit_cast(env: &TypeEnvironment, from_type: &CXType, to_type: &CXType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type(env.type_map).cloned()?,
               to_type.intrinsic_type(env.type_map).cloned()?) {

            (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. })
                => if b1 > b2 {
                    Some(CXCastType::IntegralTrunc)
                } else if b1 < b2 {
                    Some(CXCastType::IntegralCast)
                } else {
                    Some(CXCastType::BitCast)
                },

            (CXTypeKind::Float { .. }, CXTypeKind::Float { .. }) => Some(CXCastType::FloatCast),

            (CXTypeKind::Integer { .. }, CXTypeKind::Float { .. }) => Some(CXCastType::IntToFloat),
            (CXTypeKind::Float { .. }, CXTypeKind::Integer { .. }) => Some(CXCastType::FloatToInt),

            (CXTypeKind::PointerTo(inner), CXTypeKind::PointerTo(inner2))
                if valid_implicit_cast(env, inner.as_ref(), inner2.as_ref()).is_some()
                    => Some(CXCastType::BitCast),

            (CXTypeKind::Function { .. }, CXTypeKind::PointerTo(inner))
                if same_type(env.type_map, inner.as_ref(), from_type) => Some(CXCastType::FunctionToPointerDecay),

            _ => None
        }
    )
}

pub fn valid_explicit_cast(env: &TypeEnvironment, from_type: &CXType, to_type: &CXType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type(env.type_map).cloned()?,
               to_type.intrinsic_type(env.type_map).cloned()?) {

            (CXTypeKind::PointerTo(_), CXTypeKind::PointerTo(_))
                => Some(CXCastType::BitCast),

            (CXTypeKind::PointerTo(_), CXTypeKind::Integer { bytes, .. })
                if bytes == 8 => Some(CXCastType::BitCast),

            (CXTypeKind::PointerTo(_), CXTypeKind::Integer { .. })
                => Some(CXCastType::IntegralTrunc),

            (CXTypeKind::Integer { bytes, .. }, CXTypeKind::PointerTo(_))
                if bytes == 8 => Some(CXCastType::BitCast),

            (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo(_))
                => Some(CXCastType::IntegralCast),

            _ => None
        }
    )
}

pub fn implicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: &CXType, to_type: &CXType)
                     -> Option<Option<()>> {
    if same_type(env.type_map, from_type, to_type) {
        return Some(Some(()));
    }

    let Some(cast) = valid_implicit_cast(env, from_type, to_type)? else {
        return Some(None);
    };
    
    add_implicit_cast(expr, from_type.clone(), to_type.clone(), cast)?;

    Some(Some(()))
}

pub fn explicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: &CXType, to_type: &CXType)
                     -> Option<()> {
    if let Some(()) = implicit_cast(env, expr, from_type, to_type)? {
        return Some(())
    }

    let Some(expl_cast_type) = valid_explicit_cast(env, from_type, to_type)? else {
        return None;
    };

    add_implicit_cast(expr, from_type.clone(), to_type.clone(), expl_cast_type)?;

    Some(())
}

pub(crate) fn add_implicit_cast(expr: &mut CXExpr, from_type: CXType, to_type: CXType, cast_type: CXCastType) -> Option<()> {
    let old_expr = std::mem::take(expr);
    
    *expr = CXExprKind::ImplicitCast {
        expr: Box::new(old_expr),
        from_type,
        to_type,
        cast_type
    }.into();
    
    Some(())
}

pub(crate) fn alg_bin_op_coercion(env: &mut TypeEnvironment,
                                  lhs: &mut Box<CXExpr>, rhs: &mut Box<CXExpr>)
                                  -> Option<CXType> {
    let l_type = coerce_value(env, lhs)?;
    let r_type = coerce_value(env, rhs)?;
    
    if same_type(env.type_map, &l_type, &r_type) {
        return Some(l_type);
    }

    match (l_type.intrinsic_type(env.type_map).cloned()?,
           r_type.intrinsic_type(env.type_map).cloned()?) {

        (CXTypeKind::PointerTo(_), CXTypeKind::Integer { .. }) => {
            add_implicit_cast(rhs, r_type.clone(), l_type.clone(), CXCastType::IntToScaledPtrDiff)?;
            
            Some(l_type)
        },

        (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo(_)) => {
            add_implicit_cast(lhs, l_type.clone(), r_type.clone(), CXCastType::IntToScaledPtrDiff)?;
            
            Some(r_type)
        },

        (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. }) => {
            if b1 > b2 {
                add_implicit_cast(rhs, r_type.clone(), l_type.clone(), CXCastType::IntegralCast)?;
                Some(l_type)
            } else if b1 < b2 {
                add_implicit_cast(lhs, l_type.clone(), r_type.clone(), CXCastType::IntegralCast)?;
                Some(r_type)
            } else {
                Some(l_type)
            }
        },

        _ => log_error!("Cannot perform binary operation on types {l_type} and {r_type}"),
    }
}
