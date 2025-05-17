use std::clone;
use crate::TypeEnvironment;
use cx_data_ast::parse::ast::{CXCastType, CXExpr};
use cx_data_ast::parse::value_type::{same_type, CXTypeUnion, CXValType};
use cx_util::log_error;
use crate::checker::coerce_value;

pub fn valid_implicit_cast(env: &TypeEnvironment, from_type: &CXValType, to_type: &CXValType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type(env.type_map).cloned()?,
               to_type.intrinsic_type(env.type_map).cloned()?) {

            (CXTypeUnion::Integer { bytes: b1, .. }, CXTypeUnion::Integer { bytes: b2, .. })
                => if b1 > b2 {
                    Some(CXCastType::IntegralTrunc)
                } else if b1 < b2 {
                    Some(CXCastType::IntegralCast)
                } else {
                    Some(CXCastType::BitCast)
                },

            (CXTypeUnion::Float { .. }, CXTypeUnion::Float { .. }) => Some(CXCastType::FloatCast),

            (CXTypeUnion::Integer { .. }, CXTypeUnion::Float { .. }) => Some(CXCastType::IntToFloat),
            (CXTypeUnion::Float { .. }, CXTypeUnion::Integer { .. }) => Some(CXCastType::FloatToInt),

            (CXTypeUnion::PointerTo(inner), CXTypeUnion::PointerTo(inner2))
                if valid_implicit_cast(env, inner.as_ref(), inner2.as_ref()).is_some()
                    => Some(CXCastType::BitCast),

            (CXTypeUnion::Function { .. }, CXTypeUnion::PointerTo(inner))
                if same_type(env.type_map, inner.as_ref(), from_type) => Some(CXCastType::FunctionToPointerDecay),

            _ => None
        }
    )
}

pub fn valid_explicit_cast(env: &TypeEnvironment, from_type: &CXValType, to_type: &CXValType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type(env.type_map).cloned()?,
               to_type.intrinsic_type(env.type_map).cloned()?) {

            (CXTypeUnion::PointerTo(_), CXTypeUnion::PointerTo(_))
                => Some(CXCastType::BitCast),

            (CXTypeUnion::PointerTo(_), CXTypeUnion::Integer { bytes, .. })
                if bytes == 8 => Some(CXCastType::BitCast),

            (CXTypeUnion::PointerTo(_), CXTypeUnion::Integer { .. })
                => Some(CXCastType::IntegralTrunc),

            (CXTypeUnion::Integer { bytes, .. }, CXTypeUnion::PointerTo(_))
                if bytes == 8 => Some(CXCastType::BitCast),

            (CXTypeUnion::Integer { .. }, CXTypeUnion::PointerTo(_))
                => Some(CXCastType::IntegralCast),

            _ => None
        }
    )
}

pub fn implicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: &CXValType, to_type: &CXValType)
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

pub fn explicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: &CXValType, to_type: &CXValType)
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

pub(crate) fn add_implicit_cast(expr: &mut CXExpr, from_type: CXValType, to_type: CXValType, cast_type: CXCastType) -> Option<()> {
    let old_expr = std::mem::replace(expr, CXExpr::Taken);
    *expr = CXExpr::ImplicitCast {
        expr: Box::new(old_expr),
        from_type, to_type, cast_type,
    };

    Some(())
}

pub(crate) fn alg_bin_op_coercion(env: &mut TypeEnvironment,
                                  lhs: &mut CXExpr, rhs: &mut CXExpr)
                                  -> Option<CXValType> {
    let l_type = coerce_value(env, lhs)?;
    let r_type = coerce_value(env, rhs)?;
    
    if same_type(env.type_map, &l_type, &r_type) {
        return Some(l_type);
    }

    match (l_type.intrinsic_type(env.type_map).cloned()?,
           r_type.intrinsic_type(env.type_map).cloned()?) {

        (CXTypeUnion::PointerTo(_), CXTypeUnion::Integer { .. }) => {
            add_implicit_cast(rhs, r_type.clone(), l_type.clone(), CXCastType::IntToScaledPtrDiff)?;
            
            Some(l_type)
        },

        (CXTypeUnion::Integer { .. }, CXTypeUnion::PointerTo(_)) => {
            add_implicit_cast(lhs, l_type.clone(), r_type.clone(), CXCastType::IntToScaledPtrDiff)?;
            
            Some(r_type)
        },

        (CXTypeUnion::Integer { bytes: b1, .. }, CXTypeUnion::Integer { bytes: b2, .. }) => {
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
