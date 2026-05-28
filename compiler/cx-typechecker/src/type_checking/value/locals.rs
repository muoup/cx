use crate::{
    environment::{BindingMoveState, TypeEnvironment, symbols::SymbolValueOrigin},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::{BindingPlaceKind, TypecheckResult, TypecheckedBinding},
        typechecker::typecheck_expr,
        value::ensure_valid_allocation_type,
    },
};
use cx_ast::ast::CXExpression;
use cx_ast::data::CXType;
use cx_mir::mir::{
    expression::{MIRExpression, MIRExpressionKind},
    program::MIRBaseMappings,
};
use cx_tokens::TokenRange;
use cx_util::{CXResult, identifier::CXIdent};

pub(crate) fn ensure_binding_available(
    env: &mut TypeEnvironment,
    range: Option<TokenRange>,
    name: &CXIdent,
) -> CXResult<()> {
    let Some(binding) = env.function.tracked_binding(name.as_str()) else {
        return Ok(());
    };

    match binding.state {
        BindingMoveState::Available => Ok(()),
        BindingMoveState::Moved => {
            log_typecheck_error!(env, range, "Identifier '{}' has been moved", name)
        }
        BindingMoveState::ConditionallyMoved => log_typecheck_error!(
            env,
            range,
            "Identifier '{}' was conditionally moved across a control-flow join",
            name
        ),
    }
}

pub(crate) fn mark_binding(
    env: &mut TypeEnvironment,
    binding: &TypecheckedBinding,
    state: BindingMoveState,
) {
    if binding.kind == BindingPlaceKind::Local
        && env
            .function
            .tracked_binding(binding.root.as_str())
            .is_some()
    {
        env.function
            .set_tracked_binding_state(binding.root.as_str(), state);
    }
}

pub(crate) fn typecheck_var_declaration(
    env: &mut TypeEnvironment,
    base_data: &MIRBaseMappings,
    expr: &CXExpression,
    ty: &CXType,
    name: &CXIdent,
    initial_value: Option<&Box<CXExpression>>,
) -> CXResult<TypecheckResult> {
    let ty = env.complete_type(base_data, expr, ty)?;

    ensure_valid_allocation_type(env, Some(expr.token_range().clone()), "a variable", &ty)?;

    let mem_type = env.symbols.context.mem_ref_to(ty.clone());
    let (initial_region, adopting) = match initial_value {
        Some(init_expr) => {
            let init_tc = typecheck_expr(env, base_data, init_expr, Some(&ty))?;
            let adopting = init_tc.is_adopting();
            let init_expr = std_rval_promotion(env, init_tc.into_expression()?)
                .and_then(|v| implicit_cast(env, v, &ty))?;
            (Box::new(init_expr), adopting)
        }
        None => (
            Box::new(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::RegionCreate {
                    _type: ty.clone(),
                    initial_value: None,
                },
                _type: mem_type.clone(),
            }),
            false,
        ),
    };

    let binding = MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::BindRegion {
            name: name.clone(),
            _type: ty.clone(),
            initial_region,
            adopting,
        },
        _type: mem_type.clone(),
    };
    env.symbols.insert_value(
        name.clone(),
        MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Variable(name.clone()),
            _type: mem_type,
        },
        Some(SymbolValueOrigin::Local),
    );

    if env.symbols.is_nocopy(&ty) {
        env.function
            .track_binding(name.as_string(), env.symbols.is_nodrop(&ty));
    }

    Ok(TypecheckResult::from(binding))
}
