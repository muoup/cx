use crate::environment::BindingMoveState;
use crate::environment::TypeEnvironment;
use crate::environment::function_query::{
    query_deduced_member_function, query_deduced_standard_function,
    query_deduced_static_member_function, query_static_member_function,
};
use crate::log_typecheck_error;
use crate::type_checking::casting::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::lvalue;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::structured_initialization::{
    TypeConstructor, deconstruct_type_constructor,
};
use crate::type_checking::typechecker::{ensure_binding_available, typecheck_expr};
use cx_ast::ast::{CXBinOp, CXExpr, CXExprKind};
use cx_ast::data::CXReceiverMode;
use cx_ast::data::{CX_CONST, CXFunctionPrototype, CXTypeKind};
use cx_mir::mir::data::{MIRFloatType, MIRFunctionPrototype, MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRCoercion, MIRExpression, MIRExpressionKind, MIRFunctionContract};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::CXResult;
use cx_util::identifier::CXIdent;

fn resolve_access_base(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    lhs: MIRExpression,
) -> CXResult<(MIRExpression, MIRExpression, MIRType, bool)> {
    let lhs_source = lhs.clone();

    // Here, our aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let mut lhs_ref_const = false;
    let mut lhs = lhs;
    let lhs_inner = loop {
        let lhs_type = lhs._type.clone();

        if let Some(inner_type) = env.types.context.mem_ref_inner(&lhs_type).cloned() {
            lhs_ref_const |= inner_type.get_specifier(CX_CONST);

            if let Some(ptr_inner) = env.types.context.ptr_inner(&inner_type).cloned() {
                lhs_ref_const |= ptr_inner.get_specifier(CX_CONST);

                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: env.types.context.pointer_to(ptr_inner.clone()),
                };

                break ptr_inner;
            }

            if env.types.context.mem_ref_inner(&inner_type).is_some() {
                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: inner_type.clone(),
                };
            } else {
                break inner_type;
            }
        } else if let Some(inner_type) = env.types.context.ptr_inner(&lhs_type).cloned() {
            lhs_ref_const |= inner_type.get_specifier(CX_CONST);
            break inner_type;
        } else {
            break lhs_type;
        }
    };

    if !matches!(
        lhs_inner.kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
    ) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Expected a struct or union type on the left-hand side of an access expression, found {}",
            lhs_inner
        );
    }

    Ok((lhs_source, lhs, lhs_inner, lhs_ref_const))
}

fn build_function_reference(prototype: &MIRFunctionPrototype) -> MIRExpression {
    MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::FunctionReference {
            name: prototype.name.clone(),
        },
        _type: MIRTypeKind::Function {
            signature: Box::new(prototype.signature()),
        }
        .into(),
    }
}

fn finish_function_call<'a>(
    env: &mut TypeEnvironment,
    callee_expr: &'a CXExpr,
    expr: &'a CXExpr,
    function: TypecheckResult,
    mut tc_args: Vec<(&'a CXExpr, MIRExpression)>,
) -> CXResult<TypecheckResult> {
    let (function, implicit_parameters) = function.into_parts();
    tc_args = implicit_parameters
        .iter()
        .map(|val| (expr, val.clone()))
        .chain(tc_args)
        .collect();

    let loaded_function = lvalue::try_conversion(env, function).catch_err(|expr, msg| {
        log_typecheck_error!(env, expr.token_range.as_ref(), "Unreachable: {}", msg)
    })?;
    let loaded_function_type = loaded_function.get_type();
    let loaded_function_type = env
        .types
        .context
        .ptr_inner(&loaded_function_type)
        .cloned()
        .unwrap_or(loaded_function_type);

    let MIRTypeKind::Function { signature } = &loaded_function_type.kind else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Attempted to call value of non-function type {}",
            loaded_function_type
        );
    };

    if tc_args.len() != signature.params.len() && !signature.var_args {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects {} arguments, found {}",
            signature,
            signature.params.len(),
            tc_args.len()
        );
    }

    if tc_args.len() < signature.params.len() {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Call to {} expects at least {} arguments, found {}",
            signature,
            signature.params.len(),
            tc_args.len()
        );
    }

    let canon_params = signature.params.len();

    for ((arg_expr, val), param) in tc_args.iter_mut().zip(signature.params.iter()) {
        *val = implicit_cast(env, std::mem::take(val), &param._type)?;
    }

    for (arg_expr, val) in tc_args.iter_mut().skip(canon_params) {
        *val = std_rval_promotion(env, std::mem::take(val))?;

        let arg_type = val._type.clone();

        match &arg_type.kind {
            MIRTypeKind::PointerTo { .. } => {}
            MIRTypeKind::Integer { .. } => {}
            MIRTypeKind::Float {
                _type: MIRFloatType::F32,
            } => {
                *val = implicit_cast(
                    env,
                    std::mem::take(val),
                    &MIRTypeKind::Float {
                        _type: MIRFloatType::F64,
                    }
                    .into(),
                )?;
            }
            MIRTypeKind::Float {
                _type: MIRFloatType::F64,
            } => {}
            _ => {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Cannot pass {} to varargs: expected an intrinsic type or pointer",
                    arg_type
                );
            }
        }
    }

    let args = tc_args.into_iter().map(|(_, val)| val).collect::<Vec<_>>();

    Ok(TypecheckResult::new_base(
        signature.return_type.clone(),
        MIRExpressionKind::CallFunction {
            function: Box::new(loaded_function),
            arguments: args,
        },
    ))
}

pub(crate) fn typecheck_access(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: MIRExpression,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let (lhs_source, lhs, lhs_inner, lhs_ref_const) = resolve_access_base(env, expr, lhs)?;

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(struct_field) = struct_field(&lhs_inner, &env.types.context, name.as_str())
            {
                return Ok(TypecheckResult::new_base(
                    env.types.context.mem_ref_to(
                        struct_field
                            .field_type
                            .clone()
                            .with_specifier(if lhs_ref_const { CX_CONST } else { 0 }),
                    ),
                    MIRExpressionKind::StructFieldAccess {
                        base: Box::new(lhs),
                        field_index: struct_field.index,
                        field_offset: struct_field.offset,
                        struct_type: lhs_inner.clone(),
                    },
                ));
            }

            if let Some(variants) = lhs_inner.aggregate_fields(&env.types.context) {
                if matches!(lhs_inner.kind, MIRTypeKind::Union { .. }) {
                    let Some((_, field_type)) = variants
                        .iter()
                        .find(|(field_name, _)| field_name.as_str() == name.as_str())
                    else {
                        return log_typecheck_error!(
                            env,
                            Some(expr.token_range()),
                            "Union type {} has no field named {}",
                            lhs_inner,
                            name
                        );
                    };

                    return Ok(TypecheckResult::new_base(
                        env.types.context.mem_ref_to(
                            field_type.clone().with_specifier(if lhs_ref_const {
                                CX_CONST
                            } else {
                                0
                            }),
                        ),
                        MIRExpressionKind::UnionAliasAccess {
                            base: Box::new(lhs),
                            variant_type: field_type.clone(),
                            union_type: lhs_inner.clone(),
                        },
                    ));
                }
            }

            let prototype = env.get_member_function(base_data, expr, &lhs_inner, name, None)?;
            let receiver = build_member_receiver_argument(
                env,
                expr,
                &lhs_source,
                lhs,
                &lhs_inner,
                &prototype,
            )?;

            Ok(TypecheckResult::from(build_function_reference(&prototype))
                .with_implicit_parameters(vec![receiver]))
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let prototype =
                env.get_member_function(base_data, expr, &lhs_inner, name, Some(template_input))?;
            let receiver = build_member_receiver_argument(
                env,
                expr,
                &lhs_source,
                lhs,
                &lhs_inner,
                &prototype,
            )?;

            Ok(TypecheckResult::from(build_function_reference(&prototype))
                .with_implicit_parameters(vec![receiver]))
        }

        _ => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Invalid right-hand side for access expression, found {:?}",
            rhs
        ),
    }
}

fn build_member_receiver_argument(
    env: &mut TypeEnvironment,
    expr: &CXExpr,
    lhs_source: &MIRExpression,
    lhs: MIRExpression,
    lhs_inner: &MIRType,
    prototype: &cx_mir::mir::data::MIRFunctionPrototype,
) -> CXResult<MIRExpression> {
    match prototype
        .source_prototype
        .kind
        .receiver()
        .map(|receiver| receiver.mode)
    {
        None | Some(CXReceiverMode::None) => {
            unreachable!("member function reference missing receiver mode")
        }
        Some(CXReceiverMode::ByRef) => Ok(MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::TypeConversion {
                operand: Box::new(lhs),
                conversion: MIRCoercion::ReinterpretBits,
            },
            _type: env.types.context.mem_ref_to(lhs_inner.clone()),
        }),
        Some(CXReceiverMode::ByMove) => {
            if let Some(inner_type) = env.types.context.mem_ref_inner(&lhs_source._type).cloned() {
                let MIRExpressionKind::Variable(name) = &lhs_source.kind else {
                    return log_typecheck_error!(
                        env,
                        expr.token_range(),
                        "Consuming member calls currently require a named binding or owned struct rvalue"
                    );
                };

                ensure_binding_available(env, expr, name)?;
                if env.is_nocopy(&inner_type) {
                    env.set_tracked_binding_state(name.as_str(), BindingMoveState::Moved);
                }

                Ok(MIRExpression {
                    token_range: None,
                    _type: inner_type,
                    kind: MIRExpressionKind::RegionMove {
                        source: Box::new(lhs_source.clone()),
                    },
                })
            } else {
                Ok(lhs_source.clone())
            }
        }
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

fn deduced_callee(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    expr: &CXExpr,
    arg_types: &[MIRType],
) -> CXResult<Option<TypecheckResult>> {
    match &lhs.kind {
        CXExprKind::Identifier(name) => {
            if env.symbol_value(name.as_str()).is_some() {
                return Ok(None);
            }

            let Some(prototype) =
                query_deduced_standard_function(env, base_data, expr, name, arg_types)?
            else {
                return Ok(None);
            };

            Ok(Some(TypecheckResult::from(build_function_reference(
                &prototype,
            ))))
        }

        CXExprKind::BinOp {
            op: CXBinOp::Access,
            lhs: receiver_expr,
            rhs: member_expr,
        } => {
            let CXExprKind::Identifier(name) = &member_expr.kind else {
                return Ok(None);
            };

            let receiver_source =
                typecheck_expr(env, base_data, receiver_expr, None)?.into_expression();
            let (receiver_root, receiver_value, receiver_type, _) =
                resolve_access_base(env, expr, receiver_source)?;

            if struct_field(&receiver_type, &env.types.context, name.as_str()).is_some() {
                return Ok(None);
            }

            if matches!(receiver_type.kind, MIRTypeKind::Union { .. })
                && receiver_type
                    .aggregate_fields(&env.types.context)
                    .map(|fields| {
                        fields
                            .iter()
                            .any(|(field_name, _)| field_name == name.as_str())
                    })
                    .unwrap_or(false)
            {
                return Ok(None);
            }

            let Some(prototype) = query_deduced_member_function(
                env,
                base_data,
                expr,
                &receiver_type,
                name,
                arg_types,
            )?
            else {
                return Ok(None);
            };

            let receiver = build_member_receiver_argument(
                env,
                expr,
                &receiver_root,
                receiver_value,
                &receiver_type,
                &prototype,
            )?;

            Ok(Some(
                TypecheckResult::from(build_function_reference(&prototype))
                    .with_implicit_parameters(vec![receiver]),
            ))
        }

        _ => Ok(None),
    }
}

pub(crate) fn typecheck_method_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    if let CXExprKind::BinOp {
        op: CXBinOp::ScopeRes,
        lhs: type_expr,
        rhs: method_expr,
    } = &lhs.kind
    {
        return typecheck_scoped_call(env, base_data, type_expr, method_expr, rhs, expr);
    }

    let tc_args = comma_separated(env, base_data, rhs)?;
    let arg_types = &tc_args
        .iter()
        .map(|(_, val)| val.get_type())
        .collect::<Vec<_>>();

    let function = match typecheck_expr(env, base_data, lhs, None) {
        Ok(function) => function,
        Err(err) => match deduced_callee(env, base_data, lhs, expr, &arg_types)? {
            Some(function) => function,
            None => return Err(err),
        },
    };

    finish_function_call(env, lhs, expr, function, tc_args)
}

fn typecheck_type_constructor(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpr,
    union_type: &MIRType,
    name: &CXIdent,
    inner: &CXExpr,
) -> CXResult<TypecheckResult> {
    let Some(variants) = union_type.aggregate_fields(&env.types.context) else {
        unreachable!()
    };
    let variants = variants.clone();
    let union_name = union_type.get_name().unwrap();

    let Some((i, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (variant_name, _))| variant_name == name.as_str())
        .map(|(i, (_, variant_type))| (i, variant_type.clone()))
    else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Variant '{}' not found in tagged union type {}",
            name,
            union_name
        );
    };

    let inner = typecheck_expr(env, base_data, inner, Some(&variant_type))
        .and_then(|v| std_rval_promotion(env, v.into_expression()))
        .and_then(|v| implicit_cast(env, v, &variant_type))?;

    Ok(TypecheckResult::new_base(
        union_type.clone(),
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
        CXExprKind::Identifier(name) => env.get_type(base_data, type_expr, name.as_str())?,

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let cx_type = CXTypeKind::TemplatedIdentifier {
                name: name.clone(),
                input: template_input.clone(),
            }
            .to_type();

            env.complete_type(base_data, type_expr, &cx_type)?
        }

        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Expected a type identifier before scope resolution operator, found {:?}",
                type_expr
            );
        }
    };

    let (method_name, template_input) = match &method_expr.kind {
        CXExprKind::Identifier(method_name) => (method_name, None),
        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => (name, Some(template_input)),
        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Expected identifier after scope resolution operator, found {:?}",
                method_expr
            );
        }
    };

    if mir_type.is_tagged_union() {
        if template_input.is_some() {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Tagged union constructors may not use template arguments after scope resolution"
            );
        }

        return typecheck_type_constructor(env, base_data, expr, &mir_type, method_name, args_expr);
    }

    let tc_args = comma_separated(env, base_data, args_expr)?;
    let arg_types = &tc_args
        .iter()
        .map(|(_, val)| val.get_type())
        .collect::<Vec<_>>();

    let prototype = if let Some(template_input) = template_input {
        query_static_member_function(
            env,
            base_data,
            expr,
            &mir_type,
            method_name,
            Some(template_input),
        )?
    } else if let Some(prototype) = query_deduced_static_member_function(
        env,
        base_data,
        expr,
        &mir_type,
        method_name,
        &arg_types,
    )? {
        prototype
    } else {
        query_static_member_function(env, base_data, expr, &mir_type, method_name, None)?
    };

    let function = TypecheckResult::from(build_function_reference(&prototype));
    finish_function_call(env, expr, expr, function, tc_args)
}

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpr,
    rhs: &CXExpr,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let tc_lhs: MIRExpression = typecheck_expr(env, base_data, lhs, None)
        .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
    let tc_type = tc_lhs.get_type();
    let owned_union_type;
    let union_type = if let Some(inner) = env.types.context.mem_ref_inner(&tc_type) {
        owned_union_type = inner.clone();
        &owned_union_type
    } else {
        &tc_type
    };

    let Some(variants) = union_type.aggregate_fields(&env.types.context) else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator requires a tagged union on the left-hand side, found {}",
            union_type
        );
    };
    let variants = variants.clone();
    let expected_union_name = union_type.get_name().unwrap();

    let TypeConstructor {
        union_type,
        variant_name,
        inner,
    } = deconstruct_type_constructor(env, base_data, rhs)?;
    let union_name = union_type.get_name().unwrap().as_str();

    if expected_union_name.as_str() != union_name {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name
        );
    }

    if union_type.get_name().map(|x| x.as_str()) != Some(union_name) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator left-hand side type {} does not match right-hand side tagged union type {}",
            union_type,
            union_name
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
            expr.token_range(),
            "'is' operator variant name '{}' not found in tagged union {}",
            variant_name,
            union_name
        );
    };

    let inner_name = match inner {
        None => CXIdent::from(""),
        Some(inner) if matches!(inner.kind, CXExprKind::Unit) => CXIdent::from(""),

        Some(inner) => {
            let CXExprKind::Identifier(name) = &inner.kind else {
                return log_typecheck_error!(
                    env,
                    expr.token_range(),
                    "'is' operator inner expression must be an identifier or unit, found {:?}",
                    inner
                );
            };

            let variant_ref_type = env.types.context.mem_ref_to(variant_type.clone());
            env.insert_symbol(
                name.as_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::Variable(name.clone()),
                    _type: variant_ref_type,
                },
            );

            name.clone()
        }
    };

    Ok(TypecheckResult::new_base(
        MIRType::bool(),
        MIRExpressionKind::PatternIs {
            lhs: Box::new(tc_lhs),
            sum_type: union_type.clone(),
            variant_index: expected_tag,
            inner_name,
        },
    ))
}

pub struct StructField {
    pub index: usize,
    pub offset: usize,
    pub field_type: MIRType,
}

pub fn struct_field_offset(
    struct_type: &MIRType,
    definitions: &cx_mir::mir::data::MIRTypeContext,
    field_index: usize,
) -> Option<usize> {
    let struct_type = definitions.memory_resident_type(struct_type);
    if !matches!(struct_type.kind, MIRTypeKind::Structured { .. }) {
        return None;
    }
    let Some(fields) = struct_type.aggregate_fields(definitions) else {
        unreachable!("Invalid type for struct_field_offset: {}", struct_type);
    };

    let mut field_offset = 0;

    for (i, (_, field_type)) in fields.iter().enumerate() {
        let field_alignment = field_type.type_alignment(definitions);

        field_offset = (field_offset + field_alignment - 1) / field_alignment * field_alignment;

        if i == field_index {
            return Some(field_offset);
        }

        field_offset += field_type.type_size(definitions);
    }

    None
}

pub fn struct_field<'a>(
    struct_type: &MIRType,
    definitions: &cx_mir::mir::data::MIRTypeContext,
    field_name: &str,
) -> Option<StructField> {
    let struct_type = definitions.memory_resident_type(struct_type);
    if !matches!(struct_type.kind, MIRTypeKind::Structured { .. }) {
        return None;
    }
    let Some(fields) = struct_type.aggregate_fields(definitions) else {
        return None;
    };

    fields
        .iter()
        .position(|(name, _)| name.as_str() == field_name)
        .and_then(|index| {
            let offset = struct_field_offset(struct_type, definitions, index)?;

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
    prototype: &CXFunctionPrototype,
) -> CXResult<MIRFunctionContract> {
    let naive_contract = &prototype.contract;
    let previous_mode = env.push_contract_mode(naive_contract.safe);

    env.push_scope(false, false);

    for param in prototype.params.iter() {
        if let Some(name) = &param.name {
            let mir_type = env.complete_type(base_data, &CXExpr::default(), &param._type)?;
            env.insert_symbol(
                name.to_string(),
                MIRExpression {
                    token_range: None,
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
            .and_then(|v| std_rval_promotion(env, v.into_expression()))
            .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
        Some(Box::new(tc_pre))
    } else {
        None
    };

    let postcondition = if let Some((ret_name, post_expr)) = &naive_contract.postcondition {
        if let Some(ret_name) = ret_name {
            let mir_type =
                env.complete_type(base_data, &CXExpr::default(), &prototype.return_type)?;
            env.insert_symbol(
                ret_name.to_string(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::Variable(ret_name.clone()),
                    _type: mir_type,
                },
            );
        }

        let tc_post = typecheck_expr(env, base_data, post_expr, Some(&MIRType::bool()))
            .and_then(|v| std_rval_promotion(env, v.into_expression()))
            .and_then(|v| implicit_cast(env, v, &MIRType::bool()))?;
        Some((ret_name.clone(), Box::new(tc_post)))
    } else {
        None
    };

    env.pop_scope()?;
    env.restore_function_mode(previous_mode);

    Ok(MIRFunctionContract {
        safe: naive_contract.safe,
        precondition,
        postcondition,
    })
}
