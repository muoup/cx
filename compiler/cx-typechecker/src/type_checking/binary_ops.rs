use crate::environment::TCEnvironment;
use crate::type_checking::casting::{add_coercion, coerce_value, implicit_cast};
use crate::type_checking::typechecker::typecheck_expr;
use crate::log_typecheck_error;
use crate::type_completion::prototypes::contextualize_template_args;
use cx_parsing_data::parse::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind};
use cx_parsing_data::preparse::{FunctionTypeIdent, NaiveFnKind};
use cx_typechecker_data::ast::{TCExpr, TCExprKind};
use cx_typechecker_data::cx_types::{CXType, CXTypeKind, same_type};
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn typecheck_access(
    env: &mut TCEnvironment,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> Option<TCExpr> {
    let mut tc_lhs = typecheck_expr(env, lhs)?;

    let inner = match tc_lhs._type.mem_ref_inner() {
        Some(inner) if inner.is_pointer() => {
            coerce_value(&mut tc_lhs);
            &tc_lhs._type
        }
        Some(inner) => inner,
        _ => &tc_lhs._type,
    };

    let inner = inner.ptr_inner().unwrap_or(inner);

    let fields = match &inner.kind {
        CXTypeKind::Structured { fields, .. }
        | CXTypeKind::Union {
            variants: fields, ..
        } => fields,

        _ => log_typecheck_error!(
            env,
            expr,
            "Member access on {}, expected struct or union type",
            inner
        ),
    };

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(i) = fields
                .iter()
                .position(|(field_name, _)| field_name.as_str() == name.as_str())
            {
                let (field_name, field_type) = &fields[i];

                let field_name = CXIdent::from(field_name.as_str());
                let field_type = field_type.clone().mem_ref_to();

                return Some(TCExpr {
                    _type: field_type,
                    kind: TCExprKind::Access {
                        struct_type: inner.clone(),
                        target: Box::new(tc_lhs),
                        field: field_name,
                    },
                });
            }

            let Some(type_name) = inner.get_name() else {
                log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a type name",
                    inner
                );
            };

            let fn_ident = CXFunctionKind::Member {
                base_type: CXIdent::from(type_name),
                name: name.clone(),
            };

            let Some(prototype) = env.get_realized_func(&fn_ident.into()) else {
                log_typecheck_error!(
                    env,
                    lhs,
                    " Member access on {} with invalid member name {name}",
                    tc_lhs._type
                );
            };

            Some(TCExpr {
                _type: CXTypeKind::Function {
                    prototype: Box::new(prototype),
                }
                .into(),
                kind: TCExprKind::MemberFunctionReference {
                    target_type: inner.clone(),
                    target: Box::new(tc_lhs),
                },
            })
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let Some(type_name) = inner.get_identifier() else {
                log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a base name",
                    inner
                );
            };

            let ident = NaiveFnKind::MemberFunction {
                function_name: CXIdent::from(name.as_str()),
                _type: FunctionTypeIdent::Standard(type_name.clone()),
            };
            let input = contextualize_template_args(env, template_input)?;

            let Some(prototype) = env.get_func_templated(&ident, &input) else {
                println!("Templated functions: {:?}", env.base_data.fn_data);
                
                log_typecheck_error!(
                    env,
                    expr,
                    " Could not find templated member function '{}' for type {}",
                    name,
                    tc_lhs._type,
                );
            };

            Some(TCExpr {
                _type: CXTypeKind::Function {
                    prototype: Box::new(prototype),
                }
                .into(),
                kind: TCExprKind::MemberFunctionReference {
                    target_type: inner.clone(),
                    target: Box::new(tc_lhs),
                },
            })
        }

        _ => log_typecheck_error!(
            env,
            expr,
            " Invalid rhs for access expression, found {:?}",
            rhs
        ),
    }
}

pub(crate) fn comma_separated(env: &mut TCEnvironment, expr: &CXExpr) -> Option<Vec<TCExpr>> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    if matches!(expr.kind, CXExprKind::Unit) {
        return Some(exprs);
    }

    while let CXExprKind::BinOp {
        lhs,
        rhs,
        op: CXBinOp::Comma,
    } = &expr_iter.kind
    {
        exprs.push(typecheck_expr(env, rhs)?);
        expr_iter = lhs;
    }

    exprs.push(typecheck_expr(env, expr_iter)?);
    exprs.reverse();

    Some(exprs)
}

pub(crate) fn typecheck_method_call(
    env: &mut TCEnvironment,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> Option<TCExpr> {
    let mut tc_lhs = typecheck_expr(env, lhs)?;
    let mut tc_args = comma_separated(env, rhs)?;

    let (direct, prototype) = match tc_lhs.kind {
        TCExprKind::FunctionReference => {
            let CXTypeKind::Function { prototype } = &tc_lhs._type.kind else {
                unreachable!(
                    "PANIC: Expected function type for function call, found {}",
                    tc_lhs._type
                );
            };

            let Some(prototype) = env.get_realized_func(&prototype.name) else {
                log_typecheck_error!(
                    env,
                    expr,
                    " Function '{}' not found in the current environment",
                    prototype
                );
            };

            (true, prototype)
        }

        TCExprKind::MemberFunctionReference { target, target_type } => {
            let CXTypeKind::Function { prototype } = &tc_lhs._type.kind else {
                unreachable!(
                    "PANIC: Expected function type for function call, found {}",
                    tc_lhs._type
                );
            };

            let Some(target_name) = target_type.get_name() else {
                unreachable!(
                    "PANIC: Expected named type for method call target, found {}",
                    target._type
                );
            };

            let Some(prototype) = env.get_realized_func(&prototype.name) else {
                log_typecheck_error!(
                    env,
                    expr,
                    " Method '{}' not found for type {}",
                    prototype,
                    target_name
                );
            };
            tc_args.insert(0, *target);

            tc_lhs = TCExpr {
                _type: CXTypeKind::Function {
                    prototype: Box::new(prototype.clone()),
                }
                .into(),
                kind: TCExprKind::FunctionReference,
            };

            (true, prototype)
        }

        _ => {
            let mut try_mem_ref = || -> Option<_> {
                let inner = tc_lhs._type.mem_ref_inner()?;

                if let CXTypeKind::Function { prototype } = &inner.kind {
                    Some(prototype.as_ref().clone())
                } else {
                    coerce_value(&mut tc_lhs);
                    None
                }
            };

            if let Some(prototype) = try_mem_ref() {
                (false, prototype)
            } else {
                let Some(inner) = tc_lhs._type.ptr_inner() else {
                    log_typecheck_error!(
                        env,
                        expr,
                        " Attempted to call non-function type {}",
                        tc_lhs._type
                    );
                };

                let CXTypeKind::Function { prototype } = &inner.kind else {
                    log_typecheck_error!(
                        env,
                        expr,
                        " Attempted to call non-function type {}",
                        tc_lhs._type
                    );
                };

                (false, *prototype.clone())
            }
        }
    };

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        log_typecheck_error!(
            env,
            expr,
            " Method {} expects {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < prototype.params.len() {
        log_typecheck_error!(
            env,
            expr,
            " Method {} expects at least {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    let canon_params = prototype.params.len();

    // Standard argument coercion
    for (arg, param) in tc_args.iter_mut().zip(prototype.params.iter()) {
        implicit_cast(arg, &param._type)?;
    }

    // Varargs argument coercion
    for arg in tc_args.iter_mut().skip(canon_params) {
        // All varargs arguments must be lvalues, coerce_value is necessary here
        coerce_value(arg);

        match &arg._type.kind {
            CXTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            }

            CXTypeKind::Integer { signed, .. } => {
                implicit_cast(
                    arg,
                    &CXTypeKind::Integer {
                        bytes: 8,
                        signed: *signed,
                    }
                    .into(),
                )?;
            }

            CXTypeKind::Float { bytes: 4 } => {
                implicit_cast(arg, &CXTypeKind::Float { bytes: 8 }.into())?;
            }

            CXTypeKind::Float { bytes: 8 } => {
                // Already the correct type for varargs
            }

            CXTypeKind::Bool => {
                implicit_cast(
                    arg,
                    &CXTypeKind::Integer {
                        bytes: 8,
                        signed: false,
                    }
                    .into(),
                )?;
            }

            _ => log_typecheck_error!(
                env,
                expr,
                " Cannot coerce value {} for varargs, expected intrinsic type or pointer!",
                arg._type
            ),
        }
    }

    Some(TCExpr {
        _type: prototype.return_type.clone(),
        kind: TCExprKind::FunctionCall {
            function: Box::new(tc_lhs),
            arguments: tc_args,
            direct_call: direct,
        },
    })
}

pub(crate) fn typecheck_is(
    env: &mut TCEnvironment,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> Option<TCExpr> {
    let mut tc_expr = typecheck_expr(env, lhs)?;
    coerce_value(&mut tc_expr);

    let CXTypeKind::TaggedUnion {
        name: expected_union_name,
        variants,
        ..
    } = &tc_expr._type.kind
    else {
        log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a tagged union on the left-hand side, found {}",
            tc_expr._type
        );
    };

    let CXExprKind::TypeConstructor {
        union_name,
        variant_name,
        inner,
    } = &rhs.kind
    else {
        log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a type constructor on the right-hand side, found {:?}",
            rhs
        );
    };

    if expected_union_name != union_name {
        log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name.as_string()
        );
    }

    let CXExprKind::Identifier(inner_var_name) = &inner.kind else {
        log_typecheck_error!(
            env,
            inner,
            " 'is' operator requires a variant name identifier in the type constructor, found {:?}",
            inner
        );
    };

    if tc_expr._type.get_name() != Some(union_name.as_str()) {
        log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side type {} does not match right-hand side tagged union type {}",
            tc_expr._type,
            union_name.as_string()
        );
    }

    let Some((tag_value, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == variant_name.as_str())
        .map(|(i, (_, _ty))| (i, _ty))
    else {
        log_typecheck_error!(
            env,
            expr,
            " 'is' operator variant name '{}' not found in tagged union {}",
            variant_name,
            union_name
        );
    };

    env.insert_symbol(inner_var_name.as_string(), variant_type.clone());

    let union_type = tc_expr._type.clone();
    let var_name = inner_var_name.clone();
    let variant_type = variant_type.clone();

    Some(TCExpr {
        _type: CXTypeKind::Bool.into(),
        kind: TCExprKind::ConstructorMatchIs {
            expr: Box::new(tc_expr),
            union_type,
            var_name,
            variant_type,
            variant_tag: tag_value as u64,
        },
    })
}

pub(crate) fn typecheck_binop(
    env: &TCEnvironment,
    op: CXBinOp,
    mut lhs: TCExpr,
    mut rhs: TCExpr,
    expr: &CXExpr,
) -> Option<TCExpr> {
    coerce_value(&mut lhs);
    coerce_value(&mut rhs);

    let final_type = match (&lhs._type.kind, &rhs._type.kind) {
        (_, _) if same_type(&lhs._type, &rhs._type) => binop_type(env, &op, &lhs, expr)?,

        (
            CXTypeKind::PointerTo {
                inner_type: l_inner,
                ..
            },
            CXTypeKind::Integer { .. },
        ) => {
            ptr_int_binop_coercion(op.clone(), l_inner.as_ref(), &mut rhs);
            binop_type(env, &op, &lhs, expr)?
        }

        (
            CXTypeKind::Integer { .. },
            CXTypeKind::PointerTo {
                inner_type: r_inner,
                ..
            },
        ) => {
            if matches!(op, CXBinOp::Subtract) {
                log_typecheck_error!(
                    env,
                    expr,
                    " Invalid operation [integer] - [pointer] for types {} and {}",
                    lhs._type,
                    rhs._type
                );
            }

            ptr_int_binop_coercion(op.clone(), r_inner.as_ref(), &mut lhs);
            binop_type(env, &op, &rhs, expr)?
        }

        (CXTypeKind::Integer { bytes: b1, .. }, CXTypeKind::Integer { bytes: b2, .. }) => {
            if b1 > b2 {
                add_coercion(&mut rhs, lhs._type.clone(), CXCastType::IntegralCast);
            } else if b1 < b2 {
                add_coercion(&mut lhs, rhs._type.clone(), CXCastType::IntegralCast);
            }

            binop_type(env, &op, &lhs, expr)?
        }

        (CXTypeKind::Bool, CXTypeKind::Integer { .. }) => {
            add_coercion(&mut lhs, rhs._type.clone(), CXCastType::IntegralCast);
            binop_type(env, &op, &rhs, expr)?
        }

        (CXTypeKind::Integer { .. }, CXTypeKind::Bool) => {
            add_coercion(&mut lhs, rhs._type.clone(), CXCastType::IntegralCast);
            binop_type(env, &op, &rhs, expr)?
        }

        (CXTypeKind::PointerTo { .. }, CXTypeKind::PointerTo { .. }) => {
            ptr_ptr_binop_coercion(env, op.clone(), &lhs._type, &mut rhs, expr)?
        }

        (CXTypeKind::StrongPointer { .. }, _) | (_, CXTypeKind::StrongPointer { .. }) => {
            log_typecheck_error!(
                env,
                expr,
                "R-value strong pointers must be assigned to an l-value before being used \
                in binary operations, found '{op}' with types {} and {}",
                lhs._type,
                rhs._type
            );
        }

        _ => return None,
    };

    Some(TCExpr {
        _type: final_type.clone(),
        kind: TCExprKind::BinOp {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    })
}

pub(crate) fn ptr_int_binop_coercion(
    op: CXBinOp,
    pointer_inner: &CXType,
    non_pointer: &mut TCExpr,
) {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::ArrayIndex => {
            add_coercion(
                non_pointer,
                pointer_inner.clone().pointer_to(),
                CXCastType::IntToPtrDiff,
            );
        }

        // Requires two pointers
        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => {
            add_coercion(
                non_pointer,
                pointer_inner.clone().pointer_to(),
                CXCastType::IntToPtr,
            );
        }

        _ => panic!("Invalid binary operation {op} for pointer type"),
    };
}

pub(crate) fn ptr_ptr_binop_coercion(
    env: &TCEnvironment,
    op: CXBinOp,
    pointer_inner: &CXType,
    non_pointer: &mut TCExpr,
    expr: &CXExpr,
) -> CXResult<CXType> {
    match op {
        // Requires two pointers
        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => {
            add_coercion(non_pointer, pointer_inner.clone(), CXCastType::BitCast);
            Some(CXTypeKind::Bool.into())
        }

        _ => log_typecheck_error!(env, expr, "Invalid binary operation {op} for pointer type"),
    }
}

pub(crate) fn binop_type(
    env: &TCEnvironment,
    op: &CXBinOp,
    lhs: &TCExpr,
    expr: &CXExpr,
) -> Option<CXType> {
    match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus => Some(lhs._type.clone()),

        CXBinOp::ArrayIndex => {
            let CXTypeKind::PointerTo {
                inner_type: pointer_inner,
                ..
            } = &lhs._type.kind
            else {
                panic!(
                    "Array index operation requires a pointer type, found {}",
                    lhs._type
                );
            };

            Some(CXType::from(CXTypeKind::MemoryReference(
                pointer_inner.clone(),
            )))
        }

        CXBinOp::LAnd
        | CXBinOp::LOr
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => Some(CXTypeKind::Bool.into()),

        _ => log_typecheck_error!(
            env,
            expr,
            "Invalid binary operation {op} for type {:?}",
            lhs
        ),
    }
}
