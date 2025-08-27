use cx_data_ast::parse::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind};
use cx_data_typechecker::cx_types::{same_type, CXFunctionPrototype, CXType, CXTypeKind};
use cx_data_typechecker::ast::{TCExpr, TCExprKind};
use cx_data_typechecker::TCEnvironment;
use cx_util::{log_error, CXResult};
use cx_util::mangling::mangle_member_function;
use crate::casting::{add_coercion, coerce_value, try_implicit_cast};
use crate::typechecker::typecheck_expr;

pub fn comma_separated<'a>(env: &mut TCEnvironment, expr: &CXExpr) -> Option<Vec<TCExpr>> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    while let CXExprKind::BinOp { lhs, rhs, op: CXBinOp::Comma } = &expr_iter.kind {
        let tc_expr = typecheck_expr(env, lhs)?;

        exprs.push(tc_expr);
    }

    exprs.push(typecheck_expr(env, expr_iter)?);

    Some(exprs)
}

pub fn typecheck_method_call(env: &mut TCEnvironment, lhs: &CXExpr, rhs: &CXExpr) -> Option<TCExpr> {
    let mut lhs = typecheck_expr(env, lhs)?;
    let mut tc_args = comma_separated(env, rhs)?;

    let prototype = match lhs.kind {
        TCExprKind::FunctionReference { ref name } => {
            let Some(prototype) = env.get_func(name.as_str()).cloned() else {
                log_error!("TYPE ERROR: Function '{}' not found in the current environment", name.as_string());
            };

            prototype
        },

        TCExprKind::MemberFunctionReference { target, name } => {
            let Some(target_name) = target._type.get_name() else {
                unreachable!("TYPE ERROR: Expected named type for method call target, found {}", target._type);
            };
            let mangled_name = mangle_member_function(name.as_str(), target_name);

            tc_args.insert(0, *target);

            let Some(prototype) = env.get_func(&mangled_name).cloned() else {
                log_error!("TYPE ERROR: Method '{}' not found for type {}", name.as_string(), lhs._type);
            };

            lhs = TCExpr {
                _type: CXTypeKind::Function { prototype: Box::new(prototype.clone()) }.into(),
                kind: TCExprKind::FunctionReference { name: name.clone() }
            };

            prototype
        },

        _ => log_error!("TYPE ERROR: Expected function reference on the left side of method call, found {:?}", lhs),
    };

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        log_error!("TYPE ERROR: Method {} expects {} arguments, found {}", prototype.name.as_string(), prototype.params.len(), tc_args.len());
    }

    if tc_args.len() < prototype.params.len() {
        log_error!("TYPE ERROR: Method {} expects at least {} arguments, found {}", prototype.name.as_string(), prototype.params.len(), tc_args.len());
    }

    let canon_params = prototype.params.len();

    for (arg, param) in tc_args.iter_mut().zip(prototype.params.iter()) {
        coerce_value(arg);

        let Some(_) = try_implicit_cast(arg, &param._type) else {
            log_error!("TYPE ERROR: Cannot coerce argument of type {} to expected type {} for method {}",
                arg._type, param._type, prototype.name.as_string());
        };
    }

    for arg in tc_args[canon_params..].iter_mut() {
        coerce_value(arg);

        match &arg._type.kind {
            CXTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            },

            CXTypeKind::Integer { bytes, signed } => {
                try_implicit_cast(arg, &CXTypeKind::Integer { bytes: 8, signed: *signed }.into())?;
            },

            CXTypeKind::Float { bytes } => {
                todo!()
            },

            CXTypeKind::Bool => {
                try_implicit_cast(arg, &CXTypeKind::Integer { bytes: 8, signed: false }.into())?;
            },

            _ => log_error!("TYPE ERROR: Cannot coerce value {:?} for varargs, expected intrinsic type or pointer!", arg),
        }
    }

    Some(
        TCExpr {
            _type: prototype.return_type.clone(),
            kind: TCExprKind::FunctionCall {
                function: Box::new(lhs),
                arguments: tc_args,
            }
        }
    )
}

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