use crate::environment::TypeEnvironment;
use crate::environment::function_query::query_static_member_function;
use crate::log_typecheck_error;
use crate::type_checking::accumulation::TypecheckResult;
use crate::type_checking::casting::{coerce_value, implicit_cast};
use crate::type_checking::structured_initialization::{
    TypeConstructor, deconstruct_type_constructor,
};
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_completion::prototypes::complete_template_args;
use cx_parsing_data::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_parsing_data::data::{CXNaivePrototype, CXNaiveType, CXNaiveTypeKind};
use cx_typechecker_data::mir::expression::{
    MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRFunctionContract,
    MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp,
};
use cx_typechecker_data::mir::program::MIRBaseMappings;
use cx_typechecker_data::mir::types::{CXFloatType, CXIntegerType, MIRType, MIRTypeKind};
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

pub(crate) fn typecheck_access(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: MIRExpression,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    // Here, out aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let (lhs, lhs_inner) = match lhs._type.mem_ref_inner() {
        // If we have a reference to a region containing a pointer, we need to
        // load the pointer and use that as our pointer
        Some(inner) if inner.is_pointer() => {
            let Some(ptr_inner) = inner.ptr_inner().cloned() else {
                unreachable!()
            };
            let inner = inner.clone();

            let loaded = MIRExpression {
                kind: MIRExpressionKind::MemoryRead {
                    source: Box::new(lhs),
                },
                _type: ptr_inner,
            };

            (loaded, inner)
        }

        // If we simply have a region reference, that is sufficient as a pointer,
        Some(inner) => {
            let inner = inner.clone();
            (lhs, inner)
        }

        // Technically speaking, if we have a owned struct / naked struct type,
        // we can also treat that type as a pointer, as a struct must exist
        // in memory, and its alias is thus a pointer by definition.
        //
        // This may have to change when we introduce structs that fit in registers,
        // but for now, this is acceptable.
        _ => {
            let lhs_type = lhs._type.clone();
            (lhs, lhs_type)
        }
    };

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(struct_field) = struct_field(&lhs_inner, name.as_str()) {
                return Ok(TypecheckResult::expr(
                    struct_field.field_type.mem_ref_to(),
                    MIRExpressionKind::StructFieldAccess {
                        base: Box::new(lhs.clone()),
                        field_index: struct_field.index,
                        field_offset: struct_field.offset,
                        struct_type: lhs_inner.clone(),
                    },
                ));
            }

            if let MIRTypeKind::Union { variants, .. } = &lhs_inner.kind {
                let Some((_, field_type)) = variants
                    .iter()
                    .find(|(field_name, _)| field_name.as_str() == name.as_str())
                else {
                    return log_typecheck_error!(
                        env,
                        expr,
                        " Union type {} has no field named {}",
                        lhs_inner,
                        name
                    );
                };

                return Ok(TypecheckResult::expr(
                    field_type.clone().mem_ref_to(),
                    MIRExpressionKind::UnionAliasAccess {
                        base: Box::new(lhs.clone()),
                        variant_type: field_type.clone(),
                        union_type: lhs_inner.clone(),
                    },
                ));
            }

            let prototype = env.get_member_function(base_data, expr, &lhs_inner, name, None)?;

            let lhs_val_as_pointer = MIRExpression {
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(lhs),
                    conversion: MIRCoercion::ReinterpretBits,
                },
                _type: lhs_inner.clone().pointer_to(),
            };

            Ok(TypecheckResult::expr(
                MIRTypeKind::Function {
                    prototype: Box::new(prototype),
                }
                .into(),
                MIRExpressionKind::FunctionReference {
                    implicit_variables: vec![lhs_val_as_pointer],
                },
            ))
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let input = complete_template_args(env, base_data, template_input)?;
            let prototype =
                env.get_member_function(base_data, expr, &lhs_inner, name, Some(&input))?;

            let lhs_val_as_pointer = MIRExpression {
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(lhs),
                    conversion: MIRCoercion::ReinterpretBits,
                },
                _type: lhs_inner.clone().pointer_to(),
            };

            Ok(TypecheckResult::expr(
                MIRTypeKind::Function {
                    prototype: Box::new(prototype),
                }
                .into(),
                MIRExpressionKind::FunctionReference {
                    implicit_variables: vec![lhs_val_as_pointer],
                },
            ))
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
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &'a CXExpr,
) -> CXResult<Vec<(&'a CXExpr, MIRExpression)>> {
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
        let tc_result = typecheck_expr(env, base_data, rhs, None)?;
        exprs.push((rhs, tc_result.into_expression()));
        expr_iter = lhs;
    }

    let tc_result = typecheck_expr(env, base_data, expr_iter, None)?;
    exprs.push((expr_iter, tc_result.into_expression()));
    exprs.reverse();

    Ok(exprs)
}

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    // Check if this is a scoped call (Type::method(args)) pattern
    if let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs: type_expr,
        rhs: method_expr,
    } = &lhs.kind
    {
        return typecheck_scoped_call(env, base_data, type_expr, method_expr, rhs, expr);
    }

    let lhs_val = typecheck_expr(env, base_data, lhs, None)?.into_expression();

    let loaded_lhs = coerce_value(env, lhs, lhs_val.clone())?;
    let loaded_lhs_type = loaded_lhs._type.clone();

    let loaded_lhs_type = match loaded_lhs_type.kind {
        MIRTypeKind::PointerTo { inner_type, .. } => *inner_type,

        _ => loaded_lhs_type,
    };

    let MIRTypeKind::Function { prototype } = &loaded_lhs_type.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Attempted to call non-function type {}",
            loaded_lhs_type
        );
    };

    let mut tc_args = comma_separated(env, base_data, rhs)?;

    let MIRExpressionKind::FunctionReference { implicit_variables } = &lhs_val.kind else {
        return log_typecheck_error!(
            env,
            expr,
            " Expected function reference for method call, found {}",
            lhs_val
        );
    };

    let faux_expr = CXExpr::default();

    tc_args = implicit_variables
        .iter()
        .map(|val| (&faux_expr, val.clone()))
        .chain(tc_args)
        .collect();

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        return log_typecheck_error!(
            env,
            expr,
            " Method {} expects {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < prototype.params.len() {
        return log_typecheck_error!(
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
    for ((expr, val), param) in tc_args.iter_mut().zip(prototype.params.iter()) {
        *val = implicit_cast(env, expr, std::mem::take(val), &param._type)?;
    }

    // Varargs argument coercion
    for (expr, val) in tc_args.iter_mut().skip(canon_params) {
        // All varargs arguments must be lvalues, coerce_value is necessary here
        *val = coerce_value(env, expr, std::mem::take(val))?;
        let arg_type = val._type.clone();

        match &arg_type.kind {
            MIRTypeKind::PointerTo { .. } => {
                // Pointer types are already compatible with varargs, no need to cast
            }

            MIRTypeKind::Integer { signed, .. } => {
                *val = implicit_cast(
                    env,
                    expr,
                    std::mem::take(val),
                    &MIRTypeKind::Integer {
                        _type: CXIntegerType::I64,
                        signed: *signed,
                    }
                    .into(),
                )?;
            }

            MIRTypeKind::Float {
                _type: CXFloatType::F32,
            } => {
                *val = implicit_cast(
                    env,
                    expr,
                    std::mem::take(val),
                    &MIRTypeKind::Float {
                        _type: CXFloatType::F64,
                    }
                    .into(),
                )?;
            }

            MIRTypeKind::Float {
                _type: CXFloatType::F64,
            } => {
                // Already the correct type for varargs
            }

            _ => {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Cannot coerce value {} for varargs, expected intrinsic type or pointer!",
                    arg_type
                );
            }
        }
    }

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();

    Ok(TypecheckResult::expr(
        prototype.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(MIRExpression {
                kind: MIRExpressionKind::FunctionReference {
                    implicit_variables: implicit_variables.clone(),
                },
                _type: MIRTypeKind::Function {
                    prototype: prototype.clone(),
                }
                .into(),
            }),
            arguments: args.clone(),
        },
    ))
}

fn typecheck_type_constructor(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    union_type: &MIRType,
    name: &CXIdent,
    inner: &CXExpr,
) -> CXResult<TypecheckResult> {
    let MIRTypeKind::TaggedUnion {
        name: union_name,
        variants,
        ..
    } = &union_type.kind
    else {
        unreachable!()
    };

    let Some((i, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (variant_name, _))| variant_name == name.as_str())
        .map(|(i, (_, variant_type))| (i, variant_type.clone()))
    else {
        return log_typecheck_error!(
            env,
            expr,
            " Variant '{}' not found in tagged union type {}",
            name,
            union_name
        );
    };

    let inner = typecheck_expr(env, base_data, inner, Some(&variant_type))
        .and_then(|v| implicit_cast(env, expr, v.into_expression(), &variant_type))?;

    Ok(TypecheckResult::expr(
        union_type.clone().mem_ref_to(),
        MIRExpressionKind::ConstructTaggedUnion {
            value: Box::new(inner),
            variant_index: i,
            sum_type: union_type.clone(),
        },
    ))
}

pub(crate) fn typecheck_scoped_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    type_expr: &CXExpr,
    method_expr: &CXExpr,
    args_expr: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let mir_type = match &type_expr.kind {
        CXExprKind::Identifier(name) => env.get_type(base_data, name.as_str())?,

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => env.complete_type(
            base_data,
            &CXNaiveType {
                kind: CXNaiveTypeKind::TemplatedIdentifier {
                    name: name.clone(),
                    input: template_input.clone(),
                },
                specifiers: 0,
            },
        )?,

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Expected type identifier before scope resolution operator, found {:?}",
                type_expr
            );
        }
    };

    let CXExprKind::Identifier(method_name) = &method_expr.kind else {
        return log_typecheck_error!(
            env,
            expr,
            "Expected identifier after scope resolution operator, found {:?}",
            method_expr
        );
    };

    if mir_type.is_tagged_union() {
        return typecheck_type_constructor(env, base_data, expr, &mir_type, method_name, args_expr);
    }

    let prototype = query_static_member_function(env, base_data, expr, &mir_type, method_name)?;

    let mut tc_args = comma_separated(env, base_data, args_expr)?;

    if tc_args.len() != prototype.params.len() && !prototype.var_args {
        return log_typecheck_error!(
            env,
            expr,
            " Static method {} expects {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < prototype.params.len() {
        return log_typecheck_error!(
            env,
            expr,
            " Static method {} expects at least {} arguments, found {}",
            prototype,
            prototype.params.len(),
            tc_args.len()
        );
    }

    let canon_params = prototype.params.len();

    for ((arg_expr, val), param) in tc_args.iter_mut().zip(prototype.params.iter()) {
        *val = implicit_cast(env, arg_expr, std::mem::take(val), &param._type)?;
    }

    for (arg_expr, val) in tc_args.iter_mut().skip(canon_params) {
        *val = coerce_value(env, arg_expr, std::mem::take(val))?;
    }

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();

    Ok(TypecheckResult::expr(
        prototype.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(MIRExpression {
                kind: MIRExpressionKind::FunctionReference {
                    implicit_variables: vec![],
                },
                _type: MIRTypeKind::Function {
                    prototype: Box::new(prototype.clone()),
                }
                .into(),
            }),
            arguments: args.clone(),
        },
    ))
}

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let tc_lhs = typecheck_expr(env, base_data, lhs, None)?;
    let tc_type = tc_lhs.get_type();
    let union_type = tc_type.mem_ref_inner().unwrap_or(&tc_type);

    let MIRTypeKind::TaggedUnion {
        name: expected_union_name,
        variants,
        ..
    } = &union_type.kind
    else {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator requires a tagged union on the left-hand side, found {}",
            union_type
        );
    };

    let TypeConstructor {
        union_name,
        variant_name,
        inner,
    } = deconstruct_type_constructor(env, rhs)?;

    if *expected_union_name != union_name {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name.as_string()
        );
    }

    if union_type.get_name().map(|x| x.as_str()) != Some(union_name.as_str()) {
        return log_typecheck_error!(
            env,
            expr,
            " 'is' operator left-hand side type {} does not match right-hand side tagged union type {}",
            union_type,
            union_name.as_string()
        );
    }

    let Some((expected_tag, variant_type)) = variants
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

    let inner_name = if let CXExprKind::Identifier(name) = &inner.kind {
        env.insert_symbol(
            name.to_string(),
            MIRExpression {
                kind: MIRExpressionKind::Variable(name.clone()),
                _type: variant_type.clone().mem_ref_to(),
            },
        );
        name.clone()
    } else if matches!(inner.kind, CXExprKind::Unit) {
        CXIdent::from("")
    } else {
        return log_typecheck_error!(
            env,
            expr,
            "unknown inner expression for 'is' operator: {:?}",
            inner
        );
    };

    Ok(TypecheckResult::expr(
        MIRType::bool(),
        MIRExpressionKind::PatternIs {
            lhs: Box::new(tc_lhs.into_expression()),
            sum_type: union_type.clone(),
            variant_index: expected_tag,
            inner_name,
        },
    ))
}

pub(crate) fn typecheck_binop(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    op: CXBinOp,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let mir_lhs = typecheck_expr(env, base_data, lhs, None)?;
    let mir_rhs = typecheck_expr(env, base_data, rhs, None)?;

    typecheck_binop_mir_vals(env, op, mir_lhs.expression, mir_rhs.expression, expr)
}

fn binop_coerce_value(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    val: MIRExpression,
) -> CXResult<MIRExpression> {
    let val_type = val.get_type();

    let Some(inner) = val_type.mem_ref_inner() else {
        return Ok(val);
    };

    match &inner.kind {
        MIRTypeKind::Array { inner_type, .. } => {
            implicit_cast(env, expr, val, &inner_type.clone().pointer_to())
        }

        _ => coerce_value(env, expr, val),
    }
}

pub(crate) fn typecheck_binop_mir_vals(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let mir_lhs = binop_coerce_value(env, expr, lhs)?;
    let mir_rhs = binop_coerce_value(env, expr, rhs)?;

    match (&mir_lhs._type.kind, &mir_rhs._type.kind) {
        (
            MIRTypeKind::PointerTo {
                inner_type: l_inner,
                ..
            },
            MIRTypeKind::Integer { .. },
        ) => typecheck_ptr_int_binop(env, op.clone(), l_inner.clone().as_ref(), mir_lhs, mir_rhs),

        (
            MIRTypeKind::Integer { .. },
            MIRTypeKind::PointerTo {
                inner_type: r_inner,
                ..
            },
        ) => typecheck_int_ptr_binop(env, op.clone(), r_inner.clone().as_ref(), mir_lhs, mir_rhs),

        (MIRTypeKind::Integer { .. }, MIRTypeKind::Integer { .. }) => {
            typecheck_int_int_binop(env, op, mir_lhs, mir_rhs, expr)
        }

        (MIRTypeKind::Float { .. }, MIRTypeKind::Float { .. }) => {
            typecheck_float_float_binop(env, op, mir_lhs, mir_rhs, expr)
        }

        (MIRTypeKind::PointerTo { .. }, MIRTypeKind::PointerTo { .. }) => {
            typecheck_ptr_ptr_binop(env, op, mir_lhs, mir_rhs, expr)
        }
        _ => {
            log_typecheck_error!(
                env,
                expr,
                " Invalid binary operation {op} for types {} and {}",
                mir_lhs._type,
                mir_rhs._type
            )
        }
    }
}

pub(crate) fn typecheck_float_float_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let lhs_type = lhs.get_type();
    let rhs_type = rhs.get_type();

    let MIRTypeKind::Float { _type: lhs_ftype } = &lhs_type.kind else {
        unreachable!("Expected float type for lhs in float-float binop");
    };

    let MIRTypeKind::Float { _type: rhs_ftype } = &rhs_type.kind else {
        unreachable!("Expected float type for rhs in float-float binop");
    };

    let ftype = if rhs_ftype.bytes() > lhs_ftype.bytes() {
        lhs = implicit_cast(env, expr, lhs, &rhs_type)?;
        *rhs_ftype
    } else if rhs_ftype.bytes() < lhs_ftype.bytes() {
        rhs = implicit_cast(env, expr, rhs, &lhs_type)?;
        *lhs_ftype
    } else {
        *lhs_ftype
    };

    let (result_type, fp_op) = match op {
        CXBinOp::Add => (lhs_type.clone(), MIRFloatBinOp::FADD),
        CXBinOp::Subtract => (lhs_type.clone(), MIRFloatBinOp::FSUB),
        CXBinOp::Multiply => (lhs_type.clone(), MIRFloatBinOp::FMUL),
        CXBinOp::Divide => (lhs_type.clone(), MIRFloatBinOp::FDIV),

        CXBinOp::Equal => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::EQ,
        ),
        CXBinOp::NotEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::NEQ,
        ),
        CXBinOp::Less => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FLT,
        ),
        CXBinOp::Greater => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FGT,
        ),
        CXBinOp::LessEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FLE,
        ),
        CXBinOp::GreaterEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FGE,
        ),

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid float binary operation {op} for types {} and {}",
                lhs_type,
                rhs_type
            );
        }
    };

    Ok(TypecheckResult::expr(
        result_type,
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Float { ftype, op: fp_op },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub(crate) fn typecheck_int_int_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let mut lhs_type = lhs.get_type();
    let mut rhs_type = rhs.get_type();

    let MIRTypeKind::Integer {
        _type: lhs_itype,
        signed: _,
    } = &lhs_type.kind
    else {
        unreachable!("Expected integer type for lhs in int-int binop");
    };

    let MIRTypeKind::Integer {
        _type: rhs_itype,
        signed: _,
    } = &rhs_type.kind
    else {
        unreachable!("Expected integer type for rhs in int-int binop");
    };

    // The else here handles both the case where the types are of equal bytes
    // (i.e. default to lhs type for signedness), and if lhs is bigger, which
    // also defaults to lhs type. It's possible this is not ~exactly~ adherent
    // to the language specs, and is worth revisiting later.
    let result_type = if rhs_itype.bytes() > lhs_itype.bytes() {
        rhs_type.clone()
    } else {
        lhs_type.clone()
    };

    let MIRTypeKind::Integer {
        signed: result_signed,
        ..
    } = &result_type.kind
    else {
        unreachable!("Expected integer type for result in int-int binop");
    };

    let itype = if lhs_itype.bytes() > rhs_itype.bytes() {
        rhs = implicit_cast(env, expr, rhs.clone(), &lhs_type)?;
        rhs_type = lhs_type.clone();

        *lhs_itype
    } else if rhs_itype.bytes() > lhs_itype.bytes() {
        lhs = implicit_cast(env, expr, lhs.clone(), &rhs_type)?;
        lhs_type = rhs_type.clone();

        *rhs_itype
    } else {
        *lhs_itype
    };

    let operator = MIRBinOp::Integer {
        itype,
        op: match op {
            CXBinOp::Add => MIRIntegerBinOp::ADD,
            CXBinOp::Subtract => MIRIntegerBinOp::SUB,
            CXBinOp::Multiply => MIRIntegerBinOp::MUL,
            CXBinOp::Divide => MIRIntegerBinOp::DIV,
            CXBinOp::Modulus => MIRIntegerBinOp::MOD,

            CXBinOp::Less if !*result_signed => MIRIntegerBinOp::LT,
            CXBinOp::Less if *result_signed => MIRIntegerBinOp::ILT,

            CXBinOp::Greater if !*result_signed => MIRIntegerBinOp::GT,
            CXBinOp::Greater if *result_signed => MIRIntegerBinOp::IGT,

            CXBinOp::LessEqual if !*result_signed => MIRIntegerBinOp::LE,
            CXBinOp::LessEqual if *result_signed => MIRIntegerBinOp::ILE,

            CXBinOp::GreaterEqual if !*result_signed => MIRIntegerBinOp::GE,
            CXBinOp::GreaterEqual if *result_signed => MIRIntegerBinOp::IGE,

            CXBinOp::Equal => MIRIntegerBinOp::EQ,
            CXBinOp::NotEqual => MIRIntegerBinOp::NE,

            CXBinOp::LOr => MIRIntegerBinOp::LOR,
            CXBinOp::LAnd => MIRIntegerBinOp::LAND,

            CXBinOp::BitAnd => MIRIntegerBinOp::BAND,
            CXBinOp::BitOr => MIRIntegerBinOp::BOR,
            CXBinOp::BitXor => MIRIntegerBinOp::BXOR,

            _ => {
                return log_typecheck_error!(
                    env,
                    expr,
                    " Invalid integer binary operation {op} for types {} and {}",
                    lhs_type,
                    rhs_type
                );
            }
        },
    };

    let result_type = match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus
        | CXBinOp::BitAnd
        | CXBinOp::BitOr
        | CXBinOp::BitXor => result_type.clone(),

        CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => MIRTypeKind::Integer {
            _type: CXIntegerType::I1,
            signed: false,
        }
        .into(),

        CXBinOp::LAnd | CXBinOp::LOr => {
            lhs = implicit_cast(
                env,
                expr,
                lhs,
                &MIRTypeKind::Integer {
                    _type: CXIntegerType::I1,
                    signed: false,
                }
                .into(),
            )?;

            rhs = implicit_cast(
                env,
                expr,
                rhs,
                &MIRTypeKind::Integer {
                    _type: CXIntegerType::I1,
                    signed: false,
                }
                .into(),
            )?;

            MIRTypeKind::Integer {
                _type: CXIntegerType::I1,
                signed: false,
            }
            .into()
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid integer binary operation {op} for types {} and {}",
                lhs_type,
                rhs_type
            );
        }
    };

    Ok(TypecheckResult::expr(
        result_type,
        MIRExpressionKind::BinaryOperation {
            op: operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub(crate) fn typecheck_int_ptr_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    pointer_inner: &MIRType,
    non_pointer: MIRExpression,
    pointer: MIRExpression,
) -> CXResult<TypecheckResult> {
    if op == CXBinOp::Subtract {
        return log_typecheck_error!(
            env,
            &CXExpr::default(),
            " Invalid operation [integer] - [pointer] for types {} and {}",
            non_pointer.get_type(),
            pointer.get_type()
        );
    }

    typecheck_ptr_int_binop(env, op, pointer_inner, pointer, non_pointer)
}

pub(crate) fn typecheck_ptr_int_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    pointer_inner: &MIRType,
    pointer: MIRExpression,
    integer: MIRExpression,
) -> CXResult<TypecheckResult> {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::ArrayIndex => {
            let rhs_type = integer.get_type();
            let int_sized_pointer = MIRTypeKind::Integer {
                _type: CXIntegerType::I64,
                signed: true,
            }
            .into();

            let coerced_integer =
                implicit_cast(env, &CXExpr::default(), integer, &int_sized_pointer)?;

            let MIRTypeKind::Integer { _type, .. } = &rhs_type.kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            let operation = match op {
                CXBinOp::Add | CXBinOp::ArrayIndex => MIRBinOp::PtrDiff {
                    op: MIRPtrDiffBinOp::ADD,
                    ptr_inner: Box::new(pointer_inner.clone()),
                },

                CXBinOp::Subtract => MIRBinOp::PtrDiff {
                    op: MIRPtrDiffBinOp::SUB,
                    ptr_inner: Box::new(pointer_inner.clone()),
                },

                _ => unreachable!(),
            };

            let return_type = match op {
                CXBinOp::ArrayIndex => pointer_inner.clone().mem_ref_to(),

                _ => pointer_inner.clone().pointer_to(),
            };

            Ok(TypecheckResult::expr(
                return_type.clone(),
                MIRExpressionKind::BinaryOperation {
                    op: operation,
                    lhs: Box::new(pointer),
                    rhs: Box::new(coerced_integer),
                },
            ))
        }

        // Requires two pointers
        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => {
            let MIRTypeKind::Integer { signed, _type } = integer.get_type().kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            let mir_op = match op {
                CXBinOp::LessEqual => MIRPtrBinOp::LE,
                CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
                CXBinOp::Less => MIRPtrBinOp::LT,
                CXBinOp::Greater => MIRPtrBinOp::GT,
                CXBinOp::Equal => MIRPtrBinOp::EQ,

                _ => unreachable!(),
            };

            let coerced_val = implicit_cast(
                env,
                &CXExpr::default(),
                integer,
                &MIRTypeKind::Integer {
                    _type: CXIntegerType::I64,
                    signed,
                }
                .into(),
            )?;

            Ok(TypecheckResult::expr(
                MIRTypeKind::Integer {
                    _type: CXIntegerType::I1,
                    signed: false,
                }
                .into(),
                MIRExpressionKind::BinaryOperation {
                    op: MIRBinOp::Pointer { op: mir_op },
                    lhs: Box::new(pointer),
                    rhs: Box::new(coerced_val),
                },
            ))
        }

        _ => panic!("Invalid binary operation {op} for pointer type"),
    }
}

pub(crate) fn typecheck_ptr_ptr_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let operator = match op {
        CXBinOp::LessEqual => MIRPtrBinOp::LE,
        CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
        CXBinOp::Less => MIRPtrBinOp::LT,
        CXBinOp::Greater => MIRPtrBinOp::GT,
        CXBinOp::Equal => MIRPtrBinOp::EQ,
        CXBinOp::NotEqual => MIRPtrBinOp::NE,

        _ => {
            return log_typecheck_error!(
                env,
                expr,
                " Invalid binary operation {op} for pointer types",
            );
        }
    };

    Ok(TypecheckResult::expr(
        MIRTypeKind::Integer {
            _type: CXIntegerType::I1,
            signed: false,
        }
        .into(),
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Pointer { op: operator },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub struct StructField {
    pub index: usize,
    pub offset: usize,
    pub field_type: MIRType,
}

pub fn struct_field_offset(struct_type: &MIRType, field_index: usize) -> Option<usize> {
    let struct_type = struct_type.memory_resident_type();

    let MIRTypeKind::Structured { fields, .. } = &struct_type.kind else {
        unreachable!("Invalid type for struct_field_offset: {}", struct_type);
    };

    let mut field_offset = 0;

    for (i, (_, field_type)) in fields.iter().enumerate() {
        let field_alignment = field_type.type_alignment();

        field_offset = (field_offset + field_alignment - 1) / field_alignment * field_alignment;

        if i == field_index {
            return Some(field_offset);
        }

        field_offset += field_type.type_size();
    }

    None
}

pub fn struct_field<'a>(struct_type: &MIRType, field_name: &str) -> Option<StructField> {
    let struct_type = struct_type.memory_resident_type();

    let MIRTypeKind::Structured { fields, .. } = &struct_type.kind else {
        unreachable!("Invalid type for struct_field: {}", struct_type);
    };

    fields
        .iter()
        .position(|(name, _)| name.as_str() == field_name)
        .and_then(|index| {
            let offset = struct_field_offset(struct_type, index)?;

            Some(StructField {
                index,
                offset,
                field_type: fields[index].1.clone(),
            })
        })
}

pub(crate) fn typecheck_contract(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    function_name: &CXIdent,
    prototype: &CXNaivePrototype,
) -> CXResult<MIRFunctionContract> {
    let naive_contract = &prototype.contract;

    env.push_scope(false, false);

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            let mir_type = env.complete_type(base_data, &param._type)?;
            env.insert_symbol(
                name.to_string(),
                MIRExpression {
                    kind: MIRExpressionKind::ContractVariable {
                        name: name.clone(),
                        parent_function: function_name.clone(),
                    },
                    _type: mir_type,
                },
            );
        }
    }

    let precondition = if let Some(pre_expr) = &naive_contract.precondition {
        let tc_pre = typecheck_expr(env, base_data, pre_expr, Some(&MIRType::bool()))
            .and_then(|v| coerce_value(env, pre_expr, v.into_expression()))
            .and_then(|v| implicit_cast(env, pre_expr, v, &MIRType::bool()))?;
        Some(Box::new(tc_pre))
    } else {
        None
    };

    let postcondition = if let Some((ret_name, post_expr)) = &naive_contract.postcondition {
        if let Some(ret_name) = ret_name {
            let mir_type = env.complete_type(base_data, &prototype.return_type)?;
            env.insert_symbol(
                ret_name.to_string(),
                MIRExpression {
                    kind: MIRExpressionKind::Variable(ret_name.clone()),
                    _type: mir_type,
                },
            );
        }

        let tc_post = typecheck_expr(env, base_data, post_expr, Some(&MIRType::bool()))
            .and_then(|v| coerce_value(env, post_expr, v.into_expression()))
            .and_then(|v| implicit_cast(env, post_expr, v, &MIRType::bool()))?;
        Some((ret_name.clone(), Box::new(tc_post)))
    } else {
        None
    };

    env.pop_scope();

    Ok(MIRFunctionContract {
        precondition,
        postcondition,
    })
}
