use crate::environment::TypeEnvironment;
use crate::environment::functions::query::{
    query_deduced_static_member_function, query_static_member_function,
};
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::implicit_cast;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::op::binop::calls::{
    build_function_reference, comma_separated, finish_function_call,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXExprKind, CXExpression};
use cx_ast::data::CXTypeKind;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::MIRExpressionKind;
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::{CXResult, identifier::CXIdent};

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
    type_expr: &CXExpression,
    method_expr: &CXExpression,
    args_expr: &CXExpression,
    expr: &CXExpression,
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
        arg_types,
    )? {
        prototype
    } else {
        query_static_member_function(env, base_data, expr, &mir_type, method_name, None)?
    };

    let function = TypecheckResult::from(build_function_reference(&prototype));
    finish_function_call(env, base_data, expr, function, tc_args)
}
