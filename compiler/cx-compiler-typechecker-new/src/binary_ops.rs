use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr};
use cx_data_ast::parse::value_type::{same_type, CXType, CXTypeKind};
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_data_typechecker::TCEnvironment;
use cx_util::{log_error, CXResult};
use crate::casting::{add_coercion, coerce_value, try_implicit_cast};
use crate::typechecker::typecheck_expr;

pub(crate) fn typecheck_binop(op: CXBinOp, mut lhs: TCExpr, mut rhs: TCExpr) -> Option<TCExpr> {
    coerce_value(&mut lhs);
    coerce_value(&mut rhs);

    let final_type = match (&lhs._type.kind, &rhs._type.kind) {
        (_, _) if same_type(&lhs._type, &rhs._type) => {
            lhs._type.clone()
        },

        (CXTypeKind::PointerTo { inner_type: l_inner, .. }, CXTypeKind::Integer { .. }) => {
            ptr_int_binop_coercion(op.clone(), l_inner.as_ref(), &mut rhs);
            binop_type(&op, &lhs)?
        },

        (CXTypeKind::Integer { .. }, CXTypeKind::PointerTo { inner_type: r_inner, .. }) => {
            if matches!(op, CXBinOp::Subtract) {
                log_error!("TYPE ERROR: Invalid operation [integer] - [pointer] for types {} and {}",
                    lhs._type, rhs._type);
            }

            ptr_int_binop_coercion(op.clone(), r_inner.as_ref(), &mut lhs);
            binop_type(&op, &rhs)?
        },

        (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. }) => {
            if b1 > b2 {
                add_coercion(&mut rhs, lhs._type.clone(), CXCastType::IntegralCast);
            } else if b1 < b2 {
                add_coercion(&mut lhs, rhs._type.clone(), CXCastType::IntegralCast);
            }

            binop_type(&op, &lhs)?
        },

        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => {
            add_coercion(&mut rhs, lhs._type.clone(), CXCastType::IntegralCast);
            binop_type(&op, &lhs)?
        },

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => {
            add_coercion(&mut lhs, rhs._type.clone(), CXCastType::IntegralCast);
            binop_type(&op, &rhs)?
        },

        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. })
            => ptr_ptr_binop_coercion(op.clone(), &lhs._type, &mut rhs)?,

        (CXTypeKind::StrongPointer { .. }, _) |
        (_, CXTypeKind::StrongPointer { .. }) => {
            log_error!("R-value strong pointers must be assigned to an l-value before being used \
                in binary operations, found '{op}' with types {} and {}",
                lhs._type, rhs._type);
        },

        _ => return None,
    };

    Some(
        TCExpr {
            _type: final_type.clone(),
            kind: TCExprKind::BinOp {
                op: op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs)
            }
        }
    )
}

pub(crate) fn ptr_int_binop_coercion(op: CXBinOp, pointer_inner: &CXType, non_pointer: &mut TCExpr) {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add |
        CXBinOp::Subtract |
        CXBinOp::ArrayIndex => {
            add_coercion(non_pointer, pointer_inner.clone().pointer_to(), CXCastType::IntToPtrDiff);
        },

        // Requires two pointers
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Less | CXBinOp::Greater |
        CXBinOp::Equal | CXBinOp::NotEqual => {
            add_coercion(non_pointer, pointer_inner.clone().pointer_to(), CXCastType::IntToPtr);
        },

        _ => panic!("Invalid binary operation {op} for pointer type")
    };
}

pub(crate) fn ptr_ptr_binop_coercion(op: CXBinOp, pointer_inner: &CXType, non_pointer: &mut TCExpr)
    -> CXResult<CXType> {
    match op {
        // Requires two pointers
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Less | CXBinOp::Greater |
        CXBinOp::Equal | CXBinOp::NotEqual => {
            add_coercion(non_pointer, pointer_inner.clone(), CXCastType::BitCast);
            Some(CXTypeKind::Bool.into())
        },

        _ => log_error!("Invalid binary operation {op} for pointer type")
    }
}

pub(crate) fn binop_type(op: &CXBinOp, lhs: &TCExpr) -> Option<CXType> {
    match op {
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::Multiply | CXBinOp::Divide | CXBinOp::Modulus => {
            Some(lhs._type.clone())
        },

        CXBinOp::ArrayIndex => {
            let CXTypeKind::PointerTo { inner_type: pointer_inner, .. } = &lhs._type.kind else {
                panic!("Array index operation requires a pointer type, found {}", lhs._type);
            };

            Some(
                CXType::from(CXTypeKind::MemoryReference(pointer_inner.clone()))
            )
        },

        CXBinOp::LAnd | CXBinOp::LOr |
        CXBinOp::Less | CXBinOp::Greater |
        CXBinOp::LessEqual | CXBinOp::GreaterEqual |
        CXBinOp::Equal | CXBinOp::NotEqual => Some(CXTypeKind::Bool.into()),

        _ => log_error!("Invalid binary operation {op} for type {:?}", lhs)
    }
}