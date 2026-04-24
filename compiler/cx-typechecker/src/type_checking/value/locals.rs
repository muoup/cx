use crate::{
    environment::{BindingMoveState, TypeEnvironment},
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::{BindingPlaceKind, TypecheckResult, TypecheckedBinding},
        typechecker::typecheck_expr,
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
    let ty = env.complete_type(base_data, expr, ty).map_err(|err| {
        let err: CXResult<()> = log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Failed to resolve type for variable '{}'\n{}",
            name,
            err.error_content()
        );

        err.err().unwrap()
    })?;

    if ty.is_str() {
        return log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Cannot create a variable of unsized type 'str'; use '&str' instead"
        );
    }

    let mem_type = env.symbols.context.mem_ref_to(ty.clone());
    let mir_initial_value = match initial_value {
        Some(init_expr) => {
            let init_tc = typecheck_expr(env, base_data, init_expr, Some(&ty))
                .and_then(|v| std_rval_promotion(env, v.into_expression()))
                .and_then(|v| implicit_cast(env, v, &ty))?;
            Some(Box::new(init_tc))
        }
        None => None,
    };

    let allocation = MIRExpression {
        token_range: None,
        kind: MIRExpressionKind::RegionCreate {
            name: Some(name.clone()),
            _type: ty.clone(),
            initial_value: mir_initial_value,
        },
        _type: mem_type.clone(),
    };

    env.function.insert_symbol(
        name.as_string(),
        MIRExpression {
            token_range: None,
            kind: MIRExpressionKind::Variable(name.clone()),
            _type: mem_type,
        },
    );

    if env.symbols.is_nocopy(&ty) {
        env.function
            .track_binding(name.as_string(), env.symbols.is_nodrop(&ty));
    }

    Ok(TypecheckResult::from(allocation))
}
