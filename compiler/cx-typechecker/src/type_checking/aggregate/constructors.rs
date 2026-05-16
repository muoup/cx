use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        coercion::implicit::conversion::try_argument_conversion, result::TypecheckResult,
        typechecker::typecheck_expr,
    },
};
use cx_ast::ast::CXExpression;
use cx_mir::mir::{expression::MIRExpressionKind, program::MIRBaseMappings};
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn typecheck_type_constructor_expr(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    type_name: &CXIdent,
    variant_name: &CXIdent,
    inner: &CXExpression,
) -> CXResult<TypecheckResult> {
    let union_type = env.get_type(base_data, expr, type_name.as_str())?;
    let Some(variants) = union_type.aggregate_fields(&env.symbols.context) else {
        return log_typecheck_error!(env, Some(expr.token_range()), "Unknown type: {}", type_name);
    };
    let variants = variants.clone();

    let Some((i, variant_type)) = variants
        .iter()
        .enumerate()
        .find(|(_, (candidate_name, _))| candidate_name == variant_name.as_str())
        .map(|(i, (_, variant_type))| (i, variant_type.clone()))
    else {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Variant '{}' not found in tagged union type {}",
            variant_name,
            type_name
        );
    };

    let inner = typecheck_expr(env, base_data, inner, Some(&variant_type))
        .and_then(|v| try_argument_conversion(env, v.into_expression(), &variant_type))?;

    let allocation = TypecheckResult::new_base(
        union_type.clone(),
        MIRExpressionKind::RegionCreate {
            _type: union_type.clone(),
            initial_value: None,
        },
    );

    Ok(TypecheckResult::new_base(
        env.symbols.context.mem_ref_to(union_type.clone()),
        MIRExpressionKind::TaggedUnionSet {
            target: Box::new(allocation.into_expression()),
            variant_index: i,
            inner_value: Box::new(inner),
            sum_type: union_type,
        },
    ))
}
