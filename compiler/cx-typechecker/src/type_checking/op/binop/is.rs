use crate::environment::TypeEnvironment;
use crate::log_typecheck_error;
use crate::symbol::completion::complete_template_input;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{expression::CXExpression, pattern::CXPattern};
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::contextual_eq::TypeContextEqual;
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind, SymbolValueOrigin};
use cx_mir::mir::pattern::MIRPattern;
use cx_mir::type_context::MIRTypeContext;
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &CXExpression,
    pattern: &CXPattern,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_lhs: MIRExpression = typecheck_expr(env, namespace, lhs, None)
        .and_then(|v| v.standard_ready_coerce(env, lhs.token_range()))
        .and_then(|v| std_rval_promotion(env, v))?;
    let tc_type = tc_lhs.get_type();
    let owned_union_type;
    let union_type = if let Some(inner) = env.symbols.mem_ref_inner(&tc_type) {
        owned_union_type = inner.clone();
        &owned_union_type
    } else {
        &tc_type
    };

    let Some(variants) = union_type.aggregate_fields(&env.symbols) else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator requires a tagged union on the left-hand side, found {}",
            union_type.display_with(&env.symbols)
        );
    };
    let variants = variants.clone();
    let expected_union_name = union_type.member_lookup_identifier().unwrap();

    let TypeConstructor {
        union_name,
        variant_name,
        template_input,
        inner_name,
    } = resolve_type_constructor_pattern(env, namespace, expr, pattern)?;

    if expected_union_name != &union_name {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
            union_name
        );
    }
    validate_variant_template_input(env, namespace, union_type, template_input.as_ref(), expr)?;

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
    if let Some(inner_name) = &inner_name {
        let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
        env.symbols.insert_local_value(
            QualifiedName::new_raw(inner_name.clone()),
            MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable {
                    name: inner_name.clone(),
                    location: SymbolValueOrigin::Local,
                },
                _type: variant_ref_type,
            },
        );
    }

    Ok(TypecheckResult::new(
        MIRType::bool(),
        MIRExpressionKind::PatternIs {
            lhs: Box::new(tc_lhs),
            pattern: MIRPattern::TaggedUnionVariant {
                sum_type: union_type.clone(),
                variant_index: expected_tag,
                inner_name,
            },
        },
    ))
}

fn validate_variant_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    union_type: &MIRType,
    template_input: Option<&cx_ast::ast::template::CXTemplateInput>,
    expr: &CXExpression,
) -> CXResult<()> {
    let Some(template_input) = template_input else {
        return Ok(());
    };
    let completed_input = complete_template_input(env, namespace, template_input)?;
    let Some(template_data) = union_type.get_template_data() else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Non-templated tagged union pattern may not have template arguments"
        );
    };

    if !completed_input.contextual_eq(&template_data.template_input, &env.symbols) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Tagged union pattern template arguments do not match the left-hand side type"
        );
    }

    Ok(())
}
