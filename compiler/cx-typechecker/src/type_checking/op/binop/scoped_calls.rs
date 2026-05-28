use crate::environment::functions::query::{member_function_qualified_name, query_function};
use crate::environment::{MIRFunctionGenRequest, TypeEnvironment};
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::conversion::try_argument_conversion;
use crate::type_checking::op::binop::calls::{
    build_function_reference, comma_separated, finish_function_call, ready_arg_types,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXExprKind, CXExpression};
use cx_ast::data::{
    CXFunctionContract, CXFunctionKind, CXFunctionPrototype, CXLinkageMode, CXParameter,
    CXTypeKind, PredeclarationType,
};
use cx_mir::mir::data::{MIRFunctionPrototype, MIRParameter, MIRType};
use cx_mir::mir::expression::MIRExpressionKind;
use cx_mir::mir::name_mangling::base_mangle_static_member;
use cx_mir::mir::program::MIRBaseMappings;
use cx_tokens::TokenRange;
use cx_util::{
    CXResult,
    identifier::CXIdent,
    namespace::{NamespacePath, QualifiedName},
};

fn qualified_type_namespace_and_member(name: &QualifiedName) -> Option<(String, CXIdent)> {
    (!name.namespace.is_root()).then(|| (name.namespace.as_scope_string(), name.name.clone()))
}

pub(crate) fn query_qualified_scoped_callee(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    arg_types: &[MIRType],
) -> CXResult<Option<MIRFunctionPrototype>> {
    let Some((type_name, method_name)) = qualified_type_namespace_and_member(name) else {
        return Ok(None);
    };
    let Ok(mir_type) = env.get_type(base_data, expr, &type_name) else {
        return Ok(None);
    };

    if mir_type.is_tagged_union()
        && tagged_union_has_variant(env, &mir_type, &method_name)
        && let Some((variant_index, variant_type)) =
            tagged_union_variant_data(env, &mir_type, &method_name)
    {
        return Ok(Some(tagged_union_constructor_prototype(
            env,
            &mir_type,
            &method_name,
            variant_index,
            variant_type,
        )));
    }

    query_function(env, base_data, expr, name, None, arg_types)
}

pub(crate) fn typecheck_qualified_type_constructor_call(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    args_expr: &CXExpression,
) -> CXResult<Option<TypecheckResult>> {
    let Some((type_name, method_name)) = qualified_type_namespace_and_member(name) else {
        return Ok(None);
    };
    let Ok(mir_type) = env.get_type(base_data, expr, &type_name) else {
        return Ok(None);
    };

    if mir_type.is_tagged_union() && tagged_union_has_variant(env, &mir_type, &method_name) {
        return typecheck_type_constructor(
            env,
            base_data,
            expr,
            &mir_type,
            &method_name,
            args_expr,
        )
        .map(Some);
    }

    Ok(None)
}

pub(crate) fn typecheck_qualified_scoped_reference(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    name: &QualifiedName,
    template_input: Option<&cx_ast::data::CXTemplateInput>,
) -> CXResult<Option<TypecheckResult>> {
    let Some((type_name, method_name)) = qualified_type_namespace_and_member(name) else {
        return Ok(None);
    };
    let Ok(mir_type) = env.get_type(base_data, expr, &type_name) else {
        return Ok(None);
    };

    if base_data.fn_data.get_standard(name).is_some()
        && let Some(prototype) = query_function(env, base_data, expr, name, template_input, &[])?
    {
        return Ok(Some(TypecheckResult::from(build_function_reference(
            &prototype,
        ))));
    }

    if base_data.fn_data.get_template(name).is_some() {
        if let Some(template_input) = template_input {
            if let Some(prototype) =
                query_function(env, base_data, expr, name, Some(template_input), &[])?
            {
                return Ok(Some(TypecheckResult::from(build_function_reference(
                    &prototype,
                ))));
            }
        } else {
            return Ok(Some(TypecheckResult::incomplete_templated_callee(
                name.clone(),
                None,
            )));
        }
    }

    if mir_type.is_tagged_union()
        && template_input.is_none()
        && let Some((variant_index, variant_type)) =
            tagged_union_variant_data(env, &mir_type, &method_name)
    {
        let prototype = tagged_union_constructor_prototype(
            env,
            &mir_type,
            &method_name,
            variant_index,
            variant_type,
        );
        return Ok(Some(TypecheckResult::from(build_function_reference(
            &prototype,
        ))));
    }

    Ok(None)
}

fn resolve_scoped_type_and_method<'a>(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    type_expr: &CXExpression,
    method_expr: &'a CXExpression,
    expr: &CXExpression,
) -> CXResult<(MIRType, CXIdent, Option<&'a cx_ast::data::CXTemplateInput>)> {
    if let Some((mir_type, method_name)) =
        resolve_flat_scoped_type_and_method(env, base_data, type_expr, method_expr)
    {
        return Ok((mir_type, method_name, None));
    }

    let flattened_type_name = scope_expr_flat_name(type_expr);
    let mir_type = if let Some(type_name) = flattened_type_name.as_deref()
        && let Ok(mir_type) = env.get_type(base_data, type_expr, type_name)
    {
        mir_type
    } else {
        match &type_expr.kind {
            CXExprKind::Identifier(name) => {
                env.get_type(base_data, type_expr, &name.as_flat_name())?
            }

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
        }
    };

    let (method_name, template_input) = match &method_expr.kind {
        CXExprKind::Identifier(method_name) => (method_name.name.clone(), None),
        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => (name.name.clone(), Some(template_input)),
        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Expected identifier after scope resolution operator, found {:?}",
                method_expr
            );
        }
    };

    Ok((mir_type, method_name, template_input))
}

fn resolve_flat_scoped_type_and_method(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    type_expr: &CXExpression,
    method_expr: &CXExpression,
) -> Option<(MIRType, CXIdent)> {
    let full_name = scoped_qualified_name(type_expr, method_expr)?;
    let segments = full_name
        .as_flat_name()
        .as_str()
        .split("::")
        .map(str::to_owned)
        .collect::<Vec<_>>();

    for split in (1..segments.len()).rev() {
        let type_name = segments[..split].join("::");
        let method_name = segments[split..].join("::");
        if method_name.contains("::") {
            continue;
        }

        if let Ok(mir_type) = env.get_type(base_data, type_expr, &type_name) {
            return Some((mir_type, CXIdent::new(method_name)));
        }
    }

    None
}

fn scope_expr_flat_name(expr: &CXExpression) -> Option<String> {
    match &expr.kind {
        CXExprKind::Identifier(name) => Some(name.as_flat_name()),
        CXExprKind::TemplatedIdentifier { name, .. } => Some(name.as_flat_name()),
        CXExprKind::BinOp {
            op: cx_ast::ast::CXBinOp::ScopeRes,
            lhs,
            rhs,
        } => {
            let lhs = scope_expr_flat_name(lhs)?;
            let rhs = scope_expr_flat_name(rhs)?;
            Some(format!("{lhs}::{rhs}"))
        }
        _ => None,
    }
}

fn scoped_qualified_name(
    type_expr: &CXExpression,
    method_expr: &CXExpression,
) -> Option<QualifiedName> {
    let lhs = scope_expr_flat_name(type_expr)?;
    let rhs = scope_expr_flat_name(method_expr)?;
    let path = NamespacePath::from_scoped_path(&format!("{lhs}::{rhs}"));
    let (namespace, name) = path.parent_and_name()?;
    Some(QualifiedName::new(namespace, name))
}

fn scoped_method_template_input(
    method_expr: &CXExpression,
) -> Option<&cx_ast::data::CXTemplateInput> {
    match &method_expr.kind {
        CXExprKind::TemplatedIdentifier { template_input, .. } => Some(template_input),
        _ => None,
    }
}

fn scoped_function_qualified_name(
    type_expr: &CXExpression,
    method_expr: &CXExpression,
    mir_type: &MIRType,
    method_name: &CXIdent,
) -> Option<QualifiedName> {
    scoped_qualified_name(type_expr, method_expr)
        .or_else(|| member_function_qualified_name(mir_type, method_name))
}

fn tagged_union_has_variant(env: &TypeEnvironment, union_type: &MIRType, name: &CXIdent) -> bool {
    union_type
        .aggregate_fields(&env.symbols.context)
        .is_some_and(|variants| {
            variants
                .iter()
                .any(|(variant_name, _)| variant_name == name.as_str())
        })
}

fn tagged_union_variant_data(
    env: &TypeEnvironment,
    union_type: &MIRType,
    name: &CXIdent,
) -> Option<(usize, MIRType)> {
    union_type
        .aggregate_fields(&env.symbols.context)?
        .iter()
        .enumerate()
        .find(|(_, (variant_name, _))| variant_name == name.as_str())
        .map(|(index, (_, variant_type))| (index, variant_type.clone()))
}

fn tagged_union_constructor_prototype(
    env: &mut TypeEnvironment,
    union_type: &MIRType,
    name: &CXIdent,
    variant_index: usize,
    variant_type: MIRType,
) -> MIRFunctionPrototype {
    let mangled_name = base_mangle_static_member(&env.symbols.context, name.as_str(), union_type);

    let prototype = MIRFunctionPrototype {
        name: CXIdent::new(mangled_name.clone()),
        source_prototype: CXFunctionPrototype {
            kind: CXFunctionKind::StaticMemberFunction {
                member_type: cx_ast::data::CXFunctionTypeIdent::Standard(QualifiedName::new_raw(
                    CXIdent::new(
                        union_type
                            .get_name()
                            .map(CXIdent::as_str)
                            .unwrap_or("__anonymous_tagged_union"),
                    ),
                )),
                name: name.clone(),
            },
            params: vec![CXParameter {
                name: Some(CXIdent::new("value")),
                _type: CXTypeKind::Identifier {
                    name: QualifiedName::new_raw(CXIdent::new("__variant_payload")),
                    predeclaration: PredeclarationType::None,
                }
                .to_type(),
            }],
            return_type: CXTypeKind::Identifier {
                name: QualifiedName::new_raw(CXIdent::new("__tagged_union")),
                predeclaration: PredeclarationType::None,
            }
            .to_type(),
            var_args: false,
            contract: CXFunctionContract::default(),
            linkage: CXLinkageMode::Static,
            range: TokenRange::default(),
        },
        return_type: union_type.clone(),
        params: vec![MIRParameter {
            name: Some(CXIdent::new("value")),
            _type: variant_type.clone(),
        }],
        var_args: false,
        contract: CXFunctionContract::default(),
        linkage: CXLinkageMode::Static,
    };

    if env.get_realized_func(&mangled_name).is_none() {
        env.items
            .realized_fns
            .insert(mangled_name.clone(), prototype.clone());
        env.request_function_generation(MIRFunctionGenRequest::TypeConstructor {
            name: mangled_name,
            union_type: union_type.clone(),
            variant_type,
            variant_index,
        });
    }

    prototype
}

pub(crate) fn typecheck_type_constructor(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    union_type: &MIRType,
    name: &CXIdent,
    inner: &CXExpression,
) -> CXResult<TypecheckResult> {
    let Some(variants) = union_type.aggregate_fields(&env.symbols.context) else {
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
        .and_then(|v| try_argument_conversion(env, v.into_expression()?, &variant_type))?;

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
    type_expr: &CXExpression,
    method_expr: &CXExpression,
    args_expr: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let resolved = resolve_scoped_type_and_method(env, base_data, type_expr, method_expr, expr);
    let (mir_type, method_name, template_input) = match resolved {
        Ok(resolved) => resolved,
        Err(type_error) => {
            let Some(function_name) = scoped_qualified_name(type_expr, method_expr) else {
                return Err(type_error);
            };
            let tc_args = comma_separated(env, base_data, args_expr)?;
            let arg_types = ready_arg_types(&tc_args)?.unwrap_or_default();

            let Some(prototype) = query_function(
                env,
                base_data,
                expr,
                &function_name,
                scoped_method_template_input(method_expr),
                &arg_types,
            )?
            else {
                return Err(type_error);
            };

            let function = TypecheckResult::from(build_function_reference(&prototype));
            return finish_function_call(env, base_data, expr, function, tc_args);
        }
    };

    let Some(function_name) =
        scoped_function_qualified_name(type_expr, method_expr, &mir_type, &method_name)
    else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Scoped function '{}::{}' not found",
            mir_type.display_with(&env.symbols.context),
            method_name
        );
    };

    let tc_args = comma_separated(env, base_data, args_expr)?;
    let arg_types = ready_arg_types(&tc_args)?.unwrap_or_default();

    if let Some(prototype) = query_function(
        env,
        base_data,
        expr,
        &function_name,
        template_input,
        &arg_types,
    )? {
        let function = TypecheckResult::from(build_function_reference(&prototype));
        return finish_function_call(env, base_data, expr, function, tc_args);
    }

    if template_input.is_none()
        && mir_type.is_tagged_union()
        && tagged_union_has_variant(env, &mir_type, &method_name)
    {
        typecheck_type_constructor(env, base_data, expr, &mir_type, &method_name, args_expr)
    } else {
        log_typecheck_error!(
            env,
            expr.token_range(),
            "Scoped function '{}::{}' not found",
            mir_type.display_with(&env.symbols.context),
            method_name
        )
    }
}

pub(crate) fn typecheck_scoped_reference(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    type_expr: &CXExpression,
    method_expr: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let resolved = resolve_scoped_type_and_method(env, base_data, type_expr, method_expr, expr);
    let (mir_type, method_name, template_input) = match resolved {
        Ok(resolved) => resolved,
        Err(type_error) => {
            let Some(function_name) = scoped_qualified_name(type_expr, method_expr) else {
                return Err(type_error);
            };
            let template_input = scoped_method_template_input(method_expr);
            if (base_data.fn_data.get_standard(&function_name).is_some()
                || template_input.is_some())
                && let Some(prototype) =
                    query_function(env, base_data, expr, &function_name, template_input, &[])?
            {
                return Ok(TypecheckResult::from(build_function_reference(&prototype)));
            }
            if base_data.fn_data.get_template(&function_name).is_some() {
                return Ok(TypecheckResult::incomplete_templated_callee(
                    function_name,
                    None,
                ));
            }
            return Err(type_error);
        }
    };

    if let Some(function_name) =
        scoped_function_qualified_name(type_expr, method_expr, &mir_type, &method_name)
    {
        if (base_data.fn_data.get_standard(&function_name).is_some() || template_input.is_some())
            && let Some(prototype) =
                query_function(env, base_data, expr, &function_name, template_input, &[])?
        {
            return Ok(TypecheckResult::from(build_function_reference(&prototype)));
        }
        if base_data.fn_data.get_template(&function_name).is_some() {
            return Ok(TypecheckResult::incomplete_templated_callee(
                function_name,
                None,
            ));
        }
    }

    if mir_type.is_tagged_union() {
        if template_input.is_none()
            && let Some((variant_index, variant_type)) =
                tagged_union_variant_data(env, &mir_type, &method_name)
        {
            let prototype = tagged_union_constructor_prototype(
                env,
                &mir_type,
                &method_name,
                variant_index,
                variant_type,
            );
            return Ok(TypecheckResult::from(build_function_reference(&prototype)));
        }

        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Tagged union constructor '{}::{}' requires an argument list",
            mir_type.display_with(&env.symbols.context),
            method_name
        );
    }

    log_typecheck_error!(
        env,
        expr.token_range(),
        "Function '{}::{}' not found",
        mir_type.display_with(&env.symbols.context),
        method_name
    )
}
