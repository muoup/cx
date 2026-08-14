use crate::{
    environment::{BindingMoveState, TypeEnvironment},
    symbol::completion::complete_type,
    type_checking::{
        coercion::implicit::implicit_cast,
        result::{BindingPlaceKind, TypecheckResult, TypecheckedBinding},
        typechecker::typecheck_expr,
        value::ensure_valid_allocation_type,
    },
};
use cx_hir::ast::{expression::HIRExpression, types::HIRType};
use cx_log::CXResult;
use cx_thir::{
    EnvironmentNamespace,
    thir::expression::{SymbolValueOrigin, THIRExpression, THIRExpressionKind, THIRLocalID},
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub(crate) fn ensure_binding_available(
    env: &mut TypeEnvironment,
    range: &TokenRange,
    expr: Option<&TypecheckedBinding>,
) -> CXResult<()> {
    let Some(binding) = expr else {
        return Ok(());
    };

    let Some(state) = env.function.tracked_binding(binding.local_id) else {
        return Ok(());
    };

    match state.state {
        BindingMoveState::Available => Ok(()),
        BindingMoveState::Moved => env.log_error(
            range,
            format!("Identifier '{}' has been moved", binding.root),
        ),
        BindingMoveState::ConditionallyMoved => env.log_error(
            range,
            format!(
                "Identifier '{}' was conditionally moved across a control-flow join",
                binding.root
            ),
        ),
    }
}

pub(crate) fn mark_binding(
    env: &mut TypeEnvironment,
    binding: &TypecheckedBinding,
    state: BindingMoveState,
) {
    if binding.kind == BindingPlaceKind::Local
        && env.function.tracked_binding(binding.local_id).is_some()
    {
        env.function
            .set_tracked_binding_state(binding.local_id, state);
    }
}

pub(crate) fn typecheck_var_declaration(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    expr: &HIRExpression,
    ty: &HIRType,
    name: &CXIdent,
    initial_value: Option<&HIRExpression>,
) -> CXResult<TypecheckResult> {
    let ty = complete_type(env, namespace, ty)?;

    ensure_valid_allocation_type(env, expr.token_range().clone(), "a variable", &ty)?;

    let mem_type = env.symbols.mem_ref_to(ty.clone());
    let local_id = THIRLocalID::fresh();
    let (initial_region, adopting) = match initial_value {
        Some(init_expr) => {
            let init_tc = typecheck_expr(env, namespace, init_expr, Some(&ty))?;
            let adopting = init_tc.is_adopting();
            let init_expr = init_tc
                .standard_ready_coerce(env, expr.token_range())
                .and_then(|v| implicit_cast(env, v, &ty))?;
            (Box::new(init_expr), adopting)
        }
        None => (
            Box::new(THIRExpression {
                token_range: TokenRange::internal(),
                kind: THIRExpressionKind::RegionCreate {
                    _type: ty.clone(),
                    initial_value: None,
                },
                _type: mem_type.clone(),
            }),
            false,
        ),
    };

    let binding = THIRExpression {
        token_range: TokenRange::internal(),
        kind: THIRExpressionKind::BindRegion {
            name: name.clone(),
            local_id,
            _type: ty.clone(),
            initial_region,
            adopting,
        },
        _type: mem_type.clone(),
    };

    env.symbols.insert_local_value(
        QualifiedName::new_raw(name.clone()),
        THIRExpression {
            token_range: TokenRange::internal(),
            kind: THIRExpressionKind::Variable {
                name: name.clone(),
                local_id: Some(local_id),
                location: SymbolValueOrigin::Local,
            },
            _type: mem_type,
        },
    );

    env.function
        .track_binding(local_id, name.clone(), ty.is_nodrop());

    Ok(TypecheckResult::from(binding))
}
