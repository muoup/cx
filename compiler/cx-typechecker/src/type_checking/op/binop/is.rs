use crate::environment::TypeEnvironment;
use crate::type_checking::pattern::tagged_union::{
    TypeConstructor, resolve_type_constructor_pattern,
};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use crate::type_checking::value::resolve_indirect_base;
use cx_hir::ast::{expression::HIRExpression, pattern::HIRPattern};
use cx_log::CXResult;
use cx_log::catalogue::typecheck::{self as catalogue};
use cx_namespace::module::NamespacePath;
use cx_namespace::module::QualifiedName;
use cx_thir::thir::data::THIRType;
use cx_thir::thir::expression::{THIRExpression, THIRExpressionKind, THIRLocalID};
use cx_thir::thir::pattern::THIRPattern;
use cx_tokens::TokenRange;

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    namespace: &NamespacePath,
    expr: &HIRExpression,
    pattern: &HIRPattern,
    lhs: &HIRExpression,
) -> CXResult<TypecheckResult> {
    let tc_lhs = typecheck_expr(env, namespace, lhs, None)
        .and_then(|v| v.standard_ready_coerce(env, lhs.token_range()))
        .map(|v| resolve_indirect_base(env, v))?;
    let union_type = &tc_lhs.source_type;

    let Some(variants) = union_type.aggregate_fields(&env.symbols) else {
        return env.log_error(
            expr.token_range(),
            &catalogue::TYPE_MISMATCH,
            (
                "is operator".into(),
                "tagged union type".into(),
                format!("{}", union_type.display_with(&env.symbols)),
            )
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

    if template_input.is_some() {
        return env.log_error(
            expr.token_range(),
            &catalogue::INVALID_FORM,
            ("template arguments".into(), "pattern".into()),
        );
    }

    if expected_union_name != &union_name {
        return env.log_error(
            expr.token_range(),
            &catalogue::UNKNOWN_MEMBER,
            (
                format!("{}", expected_union_name),
                format!("{}", union_name),
            ),
        );
    }
    
    let Some((expected_tag, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (name, _))| name == variant_name.as_str())
        .map(|(i, (_, _ty))| (i, _ty))
    else {
        return env.log_error(
            expr.token_range(),
            &catalogue::UNKNOWN_MEMBER,
            (format!("{}", variant_name), format!("{}", union_name)),
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
                    local_id: inner_local_id,
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
