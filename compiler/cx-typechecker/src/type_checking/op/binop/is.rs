use crate::environment::{TypeEnvironment, symbols::SymbolValueOrigin};
use crate::log_typecheck_error;
use crate::type_checking::coercion::implicit::promotion::std_rval_promotion;
use crate::type_checking::pattern::tagged_union::{TypeConstructor, deconstruct_type_constructor};
use crate::type_checking::result::TypecheckResult;
use crate::type_checking::typechecker::typecheck_expr;
use cx_ast::ast::{CXExprKind, CXExpression};
use cx_mir::mir::data::MIRType;
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::MIRBaseMappings;
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn typecheck_is(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    lhs: &CXExpression,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let tc_lhs: MIRExpression = typecheck_expr(env, base_data, lhs, None)
        .and_then(|v| std_rval_promotion(env, v.into_expression()))?;
    let tc_type = tc_lhs.get_type();
    let owned_union_type;
    let union_type = if let Some(inner) = env.symbols.context.mem_ref_inner(&tc_type) {
        owned_union_type = inner.clone();
        &owned_union_type
    } else {
        &tc_type
    };

    let Some(variants) = union_type.aggregate_fields(&env.symbols.context) else {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "'is' operator requires a tagged union on the left-hand side, found {}",
            union_type.display_with(&env.symbols.context)
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
            union_type.display_with(&env.symbols.context),
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

            let variant_ref_type = env.symbols.context.mem_ref_to(variant_type.clone());
            env.symbols.insert_value(
                name.clone(),
                MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::Variable(name.clone()),
                    _type: variant_ref_type,
                },
                Some(SymbolValueOrigin::Local),
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
