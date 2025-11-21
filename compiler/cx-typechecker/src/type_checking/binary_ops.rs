use crate::environment::TCEnvironment;
use crate::log_typecheck_error;
use crate::type_checking::casting::{coerce_value, implicit_cast};
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXCastType, CXExpr, CXExprKind};
use cx_parsing_data::data::{FunctionTypeIdent, NaiveFnKind};
use cx_typechecker_data::ast::{TCBaseMappings, TCExpr, TCExprKind};
use cx_typechecker_data::function_map::CXFunctionKind;
use cx_typechecker_data::mir::expression::{MIRInstruction, MIRValue};
use cx_typechecker_data::mir::types::{CXFloatType, CXIntegerType, CXType, CXTypeKind, same_type};
use cx_util::identifier::CXIdent;
use cx_util::{CXError, CXResult};

pub(crate) fn typecheck_access(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let mut lhs_val = typecheck_expr(env, base_data, lhs)?;
    let lhs_type = lhs_val.get_type();

    let lhs_inner = match lhs_type.mem_ref_inner() {
        Some(inner) if inner.is_pointer() => {
            lhs_val = coerce_value(env, lhs_val)?;
            lhs_val.get_type()
        }
        Some(inner) => inner.clone(),

        _ => lhs_type.clone(),
    };

    let lhs_inner = lhs_inner
        .ptr_inner()
        .map(CXType::clone)
        .unwrap_or(lhs_inner);

    let fields = match &lhs_inner.kind {
        CXTypeKind::Structured { fields, .. }
        | CXTypeKind::Union {
            variants: fields, ..
        } => fields,

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                "Member access on {}, expected struct or union type",
                lhs_inner
            );
        }
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

                let result = env.builder.new_register();

                match &lhs_inner.kind {
                    CXTypeKind::Structured { .. } => {
                        env.builder.add_instruction(MIRInstruction::StructGet {
                            result: result.clone(),
                            source: lhs_val,
                            field_index: i,
                            struct_type: lhs_inner.clone(),
                        })
                    }

                    CXTypeKind::Union { .. } => {
                        env.builder.add_instruction(MIRInstruction::Alias {
                            result: result.clone(),
                            value: lhs_val,
                        })
                    }

                    _ => unreachable!(),
                }

                return Ok(MIRValue::Register {
                    register: result,
                    _type: field_type.mem_ref_to(),
                });
            }

            let Some(type_name) = lhs_inner.get_name() else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a type name",
                    lhs_inner
                );
            };

            let fn_ident = CXFunctionKind::Member {
                base_type: CXIdent::from(type_name),
                name: name.clone(),
            };

            let Some(prototype) = env.get_realized_func(&fn_ident.into()) else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member access on {} with invalid member name {name}",
                    lhs_inner
                );
            };

            Ok(MIRValue::FunctionReference {
                prototype,
                implicit_variables: vec![lhs_val],
            })
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let Some(type_name) = lhs_inner.get_identifier() else {
                return log_typecheck_error!(
                    env,
                    lhs,
                    " Member function call on {} without a base name",
                    lhs_inner
                );
            };

            let ident = NaiveFnKind::MemberFunction {
                function_name: CXIdent::from(name.as_str()),
                _type: FunctionTypeIdent::Standard(type_name.clone()),
            };
            let input = complete_template_args(env, base_data, template_input)?;
            let prototype = env.get_func_templated(base_data, &ident, &input)?;

            Ok(MIRValue::FunctionReference {
                prototype,
                implicit_variables: vec![lhs_val],
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

pub(crate) fn comma_separated<'a>(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    expr: &'a CXExpr,
) -> CXResult<Vec<(&'a CXExpr, MIRValue)>> {
    let mut expr_iter = expr;
    let mut exprs = Vec::new();

    if matches!(expr.kind, CXExprKind::Unit) {
        return Ok(exprs);
    }

    while let CXExprKind::BinOp {
        lhs,
        rhs,
        op: CXBinOp::Comma,
    } = &expr_iter.kind
    {
        exprs.push((rhs, typecheck_expr(env, base_data, rhs)?));
        expr_iter = lhs;
    }

    exprs.push((expr_iter, typecheck_expr(env, base_data, expr_iter)?));
    exprs.reverse();

    Ok(exprs)
}

pub(crate) fn typecheck_method_call(
    env: &mut TCEnvironment,
    base_data: &TCBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<MIRValue> {
    let lhs_val = typecheck_expr(env, base_data, lhs)?;

    let loaded_lhs = coerce_value(env, lhs_val.clone())?;
    let loaded_lhs_type = loaded_lhs.get_type();

    let loaded_lhs_type = match loaded_lhs_type.kind {
        CXTypeKind::PointerTo { inner_type, .. } => *inner_type,

        _ => loaded_lhs_type,
    };

    let CXTypeKind::Function { prototype } = &loaded_lhs_type.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Attempted to call non-function type {}",
            loaded_lhs_type
        );
    };

    let tc_args = comma_separated(env, base_data, rhs)?;

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
        arg.1 = implicit_cast(env, arg.0, std::mem::take(&mut arg.1), &param._type)?;
    }

    // Varargs argument coercion
    for arg in tc_args.iter_mut().skip(canon_params) {
        // All varargs arguments must be lvalues, coerce_value is necessary here
        arg.1 = coerce_value(env, std::mem::take(&mut arg.1))?;
        let arg_type = arg.1.get_type();

        match &arg_type.kind {
            CXTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            }

            CXTypeKind::Integer { signed, .. } => {
                arg.1 = implicit_cast(
                    env,
                    &arg.0,
                    std::mem::take(&mut arg.1),
                    &CXTypeKind::Integer {
                        _type: CXIntegerType::I64,
                        signed: *signed,
                    }
                    .into(),
                )?;
            }

            CXTypeKind::Float {
                _type: CXFloatType::F32,
            } => {
                arg.1 = implicit_cast(
                    env,
                    &arg.0,
                    std::mem::take(&mut arg.1),
                    &CXTypeKind::Float {
                        _type: CXFloatType::F32,
                    }
                    .into(),
                )?;
            }

            CXTypeKind::Float {
                _type: CXFloatType::F64,
            } => {
                // Already the correct type for varargs
            }

            CXTypeKind::Bool => {
                arg.1 = implicit_cast(
                    env,
                    &arg.0,
                    std::mem::take(&mut arg.1),
                    &CXTypeKind::Integer {
                        _type: CXIntegerType::I64,
                        signed: false,
                    }
                    .into(),
                )?;
            }

            _ => return log_typecheck_error!(
                env,
                expr,
                " Cannot coerce value {} for varargs, expected intrinsic type or pointer!",
                arg_type
            ),
        }
    }

    let result = if prototype.return_type.is_unit() {
        None
    } else {
        Some(env.builder.new_register())
    };

    env.builder.add_instruction(MIRInstruction::CallFunction {
        result: result,
        function: loaded_lhs,
        arguments: tc_args.into_iter().map(|(_, val)| val).collect(),
    });
    
    match result {
        Some(reg) => Ok(MIRValue::Register {
            register: reg,
            _type: prototype.return_type
        }),
        None => Ok(MIRValue::NULL),
    }
}

pub(crate) fn typecheck_is<'a>(
    env: &'a mut TCEnvironment<'a>,
    base_data: &'a TCBaseMappings,
    lhs: &'a CXExpr,
    rhs: &'a CXExpr,
    expr: &'a CXExpr,
) -> CXResult<MIRValue> {
    let tc_lhs = typecheck_expr(env, base_data, lhs)?;
    let loaded_lhs_val = coerce_value(env, tc_lhs)?;
    let loaded_lhs_type = loaded_lhs_val.get_type();

    let CXTypeKind::TaggedUnion {
        name: expected_union_name,
        variants,
        ..
    } = &loaded_lhs_type.kind
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a tagged union on the left-hand side, found {}",
            loaded_lhs_type
        );
    };

    let CXExprKind::TypeConstructor {
        union_name,
        variant_name,
        inner,
    } = &rhs.kind
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a type constructor on the right-hand side, found {:?}",
            rhs
        );
    };

    if expected_union_name != union_name {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name.as_string()
        );
    }

    let CXExprKind::Identifier(inner_var_name) = &inner.kind else {
        return log_typecheck_error!(
            env,
            inner,
            " 'is' operator requires a variant name identifier in the type constructor, found {:?}",
            inner
        );
    };

    if loaded_lhs_type.get_name() != Some(union_name.as_str()) {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side type {} does not match right-hand side tagged union type {}",
            loaded_lhs_type,
            union_name.as_string()
        );
    }

    let Some((tag_value, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == variant_name.as_str())
        .map(|(i, (_, _ty))| (i, _ty))
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator variant name '{}' not found in tagged union {}",
            variant_name,
            union_name
        );
    };

    let comparison = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::TaggedUnionIs {
        result: comparison.clone(),
        source: loaded_lhs_val.clone(),
        tag_id: tag_value,
    });

    let result = env.builder.new_register();
    env.builder.add_instruction(MIRInstruction::TaggedUnionGet {
        result: result.clone(),
        source: loaded_lhs_val,
        variant_type: variant_type.clone(),
    });

    env.insert_symbol(
        inner_var_name.as_string(),
        result,
        variant_type.clone().mem_ref_to(),
    );

    Ok(MIRValue::Register {
        register: comparison,
        _type: CXTypeKind::Bool.into(),
    })
}

pub(crate) fn typecheck_binop(
    env: &TCEnvironment,
    op: CXBinOp,
    mut lhs: TCExpr,
    mut rhs: TCExpr,
    expr: &CXExpr,
) -> CXResult<TCExpr> {
    coerce_value(&mut lhs)?;
    coerce_value(&mut rhs)?;

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

        _ => log_typecheck_error!(
            env,
            expr,
            " Invalid binary operation {op} for types {} and {}",
            lhs._type,
            rhs._type
        ),
    };

    Ok(TCExpr {
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
            Ok(CXTypeKind::Bool.into())
        }

        _ => log_typecheck_error!(env, expr, "Invalid binary operation {op} for pointer type"),
    }
}

pub(crate) fn binop_type(
    env: &TCEnvironment,
    op: &CXBinOp,
    lhs: &TCExpr,
    expr: &CXExpr,
) -> CXResult<CXType> {
    try_binop_type(env, op, lhs, expr).ok_or_else(|| {
        CXError::create_boxed(format!(
            " Invalid binary operation {op} for type {}",
            lhs._type
        ))
    })
}

pub(crate) fn try_binop_type(
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
