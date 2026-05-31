use crate::environment::{TypeEnvironment, symbols::SymbolValueOrigin};
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{expression::CXExpression, pattern::CXPattern};
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::pattern::MIRPattern;
use cx_mir::mir::program::EnvironmentNamespace;
use cx_util::CXResult;

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: &CXExpression,
    pattern: &CXPattern,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_lhs: MIRExpression = typecheck_expr(env, namespace, lhs, None)
        .and_then(|v| std_rval_promotion(env, v.into_expression()?))?;
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
    let expected_union_name = union_type.get_base_identifier().unwrap();

    let TypeConstructor {
        union_name,
        variant_name,
        inner_name,
    } = resolve_type_constructor_pattern(env, namespace, expr, pattern)?;
    let union_name = union_name.as_flat_name();
    let expected_union_name = expected_union_name.as_str();

    if expected_union_name != union_name
        && union_name.rsplit("::").next() != Some(expected_union_name)
    {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator left-hand side tagged union type {} does not match right-hand side tagged union type {}",
            expected_union_name,
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
    if let Some(inner_name) = &inner_name {
        let variant_ref_type = env.symbols.mem_ref_to(variant_type.clone());
        env.symbols.insert_value(
            inner_name.clone(),
            MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::Variable(inner_name.clone()),
                _type: variant_ref_type,
            },
            Some(SymbolValueOrigin::Local),
        );
    }

    Ok(TypecheckResult::new_base(
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
