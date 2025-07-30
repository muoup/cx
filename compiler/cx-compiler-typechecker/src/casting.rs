use crate::TypeEnvironment;
use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind};
use cx_data_ast::parse::value_type::{same_type, CXTypeKind, CXType};
use cx_util::{expr_error_log, log_error};
use crate::checker::{coerce_value, type_check_traverse};

pub fn valid_implicit_cast(env: &TypeEnvironment, from_type: &CXType, to_type: &CXType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type_kind(env.type_map).cloned()?,
               to_type.intrinsic_type_kind(env.type_map).cloned()?) {
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
                if same_type(env.type_map, inner.as_ref(), to_type) => Some(CXCastType::FauxLoad),

            (CXTypeKind::StrongPointer { .. }, CXTypeKind::StrongPointer { .. }) |
            (CXTypeKind::StrongPointer { .. }, CXTypeKind::PointerTo { .. }) |
            (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. })
                => Some(CXCastType::BitCast),

            (CXTypeKind::Function { .. }, CXTypeKind::PointerTo { inner, .. })
                if same_type(env.type_map, inner.as_ref(), from_type) => Some(CXCastType::FunctionToPointerDecay),

            _ => None
        }
    )
}

pub fn valid_explicit_cast(env: &TypeEnvironment, from_type: &CXType, to_type: &CXType)
                           -> Option<Option<CXCastType>> {
    Some(
        match (from_type.intrinsic_type_kind(env.type_map).cloned()?,
               to_type.intrinsic_type_kind(env.type_map).cloned()?) {

            (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. })
                => Some(CXCastType::BitCast),

            (CXTypeKind::PointerTo { .. }, CXTypeKind::Integer { bytes, .. })
                if bytes == 8 => Some(CXCastType::BitCast),

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

pub fn implicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: &CXType, to_type: &CXType)
                     -> Option<Option<()>> {
    if same_type(env.type_map, from_type, to_type) {
        return Some(Some(()));
    }

    let Some(cast) = valid_implicit_cast(env, from_type, to_type)? else {
        return Some(None);
    };
    
    add_implicit_cast(env, expr, from_type.clone(), to_type.clone(), cast)?;

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

    add_implicit_cast(env, expr, from_type.clone(), to_type.clone(), expl_cast_type)?;

    Some(())
}

pub(crate) fn add_implicit_cast(env: &mut TypeEnvironment, expr: &mut CXExpr, from_type: CXType, to_type: CXType, cast_type: CXCastType) -> Option<()> {
    let old_expr = std::mem::take(expr);
    let start_index = old_expr.start_index;
    let end_index = old_expr.end_index;
    
    *expr = CXExprKind::ImplicitCast {
        expr: Box::new(old_expr),
        from_type,
        to_type: to_type.clone(),
        cast_type
    }.into_expr(start_index, end_index);
    env.typecheck_data.insert(expr, to_type);
    
    Some(())
}

pub(crate) fn alg_bin_op_coercion(env: &mut TypeEnvironment, op: CXBinOp,
                                  lhs: &mut CXExpr, rhs: &mut CXExpr) -> Option<CXType> {
    let l_type = coerce_value(env, lhs)?;
    let r_type = coerce_value(env, rhs)?;
    
    if same_type(env.type_map, &l_type, &r_type) {
        return binop_type(&op, None, &l_type);
    }

    match (l_type.intrinsic_type_kind(env.type_map).cloned()?,
           r_type.intrinsic_type_kind(env.type_map).cloned()?) {

        (CXTypeKind::PointerTo { inner: l_inner, .. }, CXTypeKind::Integer { .. }) => {
            ptr_int_binop_coercion(env, op, l_inner.as_ref(), rhs)
        },

        (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo { inner: r_inner, .. }) => {
            if matches!(op, CXBinOp::Subtract) {
                log_error!("Invalid operation [integer] - [pointer] for types {l_type} and {r_type}");
            }
            std::mem::swap(lhs, rhs);

            ptr_int_binop_coercion(env, op, r_inner.as_ref(), lhs)
        },

        (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. }) => {
            if b1 > b2 {
                add_implicit_cast(env, rhs, r_type.clone(), l_type.clone(), CXCastType::IntegralCast)?;
            } else if b1 < b2 {
                add_implicit_cast(env, lhs, l_type.clone(), r_type.clone(), CXCastType::IntegralCast)?;
            }
            
            binop_type(&op, None, &l_type)
        },

        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => {
            add_implicit_cast(env, rhs, r_type.clone(), l_type.clone(), CXCastType::IntegralCast)?;
            binop_type(&op, None, &l_type)
        },

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => {
            add_implicit_cast(env, lhs, l_type.clone(), r_type.clone(), CXCastType::IntegralCast)?;
            binop_type(&op, None, &r_type)
        },

        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) 
            => ptr_ptr_binop_coercion(env, op, &l_type, rhs),

        (CXTypeKind::StrongPointer { .. }, _) |
        (_, CXTypeKind::StrongPointer { .. }) => {
            eprintln!("R-value strong pointers must be assigned to an l-value before being used in binary operations, found '{op}' with types {l_type} and {r_type}");
            None
        },

        _ => None,
    }
}

pub(crate) fn ptr_int_binop_coercion(env: &mut TypeEnvironment, op: CXBinOp,
                                     pointer_inner: &CXType, non_pointer: &mut CXExpr) -> Option<CXType> {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add |
        CXBinOp::Subtract |
        CXBinOp::ArrayIndex => {
            let _type = type_check_traverse(env, non_pointer)?.clone();

            add_implicit_cast(env, non_pointer, _type.clone(), pointer_inner.clone().pointer_to(), CXCastType::IntToPtrDiff)?;
        },

        // Requires two pointers
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Less | CXBinOp::Greater |
        CXBinOp::Equal | CXBinOp::NotEqual => {
            let _type = type_check_traverse(env, non_pointer)?.clone();

            add_implicit_cast(env, non_pointer, _type.clone(), pointer_inner.clone().pointer_to(), CXCastType::IntToPtr)?;
        },

        _ => panic!("Invalid binary operation {op} for pointer type")
    };
    
    binop_type(&op, Some(pointer_inner), &pointer_inner.clone().pointer_to())
}

pub(crate) fn ptr_ptr_binop_coercion(env: &mut TypeEnvironment, op: CXBinOp,
                                     pointer_inner: &CXType, non_pointer: &mut CXExpr) -> Option<CXType> {
    match op {
        // Requires two pointers
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Less | CXBinOp::Greater |
        CXBinOp::Equal | CXBinOp::NotEqual => {
            let _type = type_check_traverse(env, non_pointer)?.clone();

            add_implicit_cast(env, non_pointer, _type.clone(), pointer_inner.clone(), CXCastType::BitCast)?;
            type_check_traverse(env, non_pointer)?;
        },

        _ => panic!("Invalid binary operation {op} for pointer type")
    };
    
    binop_type(&op, Some(pointer_inner), &pointer_inner.clone())
}

pub(crate) fn binop_type(op: &CXBinOp, pointer_inner: Option<&CXType>, lhs: &CXType) -> Option<CXType> {
    match op {
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::Multiply | CXBinOp::Divide | CXBinOp::Modulus => {
            Some(lhs.clone())
        },
        
        CXBinOp::ArrayIndex => {
            Some(
                CXType::new(
                    pointer_inner?.specifiers,
                    CXTypeKind::MemoryReference(Box::new(pointer_inner?.clone()))
                )
            )
        },
        
        CXBinOp::LAnd | CXBinOp::LOr |
        CXBinOp::Less | CXBinOp::Greater | 
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Equal | CXBinOp::NotEqual => Some(CXTypeKind::Bool.to_val_type()),

        _ => panic!("Invalid binary operation {op} for type {lhs}")
    }
}