use crate::environment::TypeEnvironment;
use crate::symbol::completion::complete_template_input;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_checking::value::resolve_indirect_base;
use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::contextual_eq::TypeContextEqual;
use cx_thir::thir::data::THIRType;
use cx_thir::thir::expression::{
    SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID,
};
use cx_thir::thir::pattern::THIRPattern;
use cx_tokens::TokenRange;
use cx_util::namespace::QualifiedName;

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &HIRExpression,
    pattern: &HIRPattern,
    expr: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let tc_lhs = typecheck_expr(env, namespace, lhs, None)
        .and_then(|v| v.standard_ready_coerce(env, lhs.token_range()))
        .map(|v| resolve_indirect_base(env, v))?;
    let union_type = &tc_lhs.source_type;

    let Some(variants) = union_type.aggregate_fields(&env.symbols) else {
        return env.log_error(
            expr.token_range(),
            format!(
                "'is' operator requires a tagged union on the left-hand side, found {}",
                union_type.display_with(&env.symbols)
            ),
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
        return env.log_error(expr.token_range(), format!("'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}", expected_union_name, union_name));
    }
    validate_variant_template_input(env, namespace, union_type, template_input.as_ref(), expr)?;

    let Some((expected_tag, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == variant_name.as_str())
        .map(|(i, (_, _ty))| (i, _ty))
    else {
        return env.log_error(
            expr.token_range(),
            format!(
                "'is' operator variant name '{}' not found in tagged union {}",
                variant_name, union_name
            ),
        );
    };
    let inner_local_id = inner_name.as_ref().map(|_| THIRLocalID::fresh());
    if let (Some(inner_name), Some(inner_local_id)) = (&inner_name, inner_local_id) {
        let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
        env.symbols.insert_local_value(
            QualifiedName::new_raw(inner_name.clone()),
            THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::Variable {
                    name: inner_name.clone(),
                    local_id: Some(inner_local_id),
                    location: SymbolValueOrigin::Local,
                },
                _type: variant_ref_type,
            },
        );
    }

    Ok(TypecheckResult::new(
        THIRType::bool(),
        THIRExpressionKind::PatternIs {
            lhs: Box::new(tc_lhs.source),
            pattern: THIRPattern::TaggedUnionVariant {
                sum_type: union_type.clone(),
                variant_index: expected_tag,
                inner_name,
                inner_local_id,
            },
        },
    ))
}

fn validate_variant_template_input(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    union_type: &THIRType,
    template_input: Option<&cx_hir::ast::template::HIRTemplateInput>,
    expr: &HIRExpression,
) -> CXResult<()> {
    let Some(template_input) = template_input else {
        return Ok(());
    };
    let completed_input = complete_template_input(env, namespace, template_input)?;
    let Some(template_data) = union_type.get_template_data() else {
        return env.log_error(
            expr.token_range(),
            "Non-templated tagged union pattern may not have template arguments".to_string(),
        );
    };

    if !completed_input.contextual_eq(&template_data.template_input, &env.symbols) {
        return env.log_error(
            expr.token_range(),
            "Tagged union pattern template arguments do not match the left-hand side type"
                .to_string(),
        );
    }

    Ok(())
}
