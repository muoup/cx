use crate::{
    environment::TypeEnvironment,
    symbol::completion::complete_type,
    type_checking::{
        coercion::implicit::implicit_cast, result::TypecheckResult, typechecker::typecheck_expr,
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

    Ok(TypecheckResult::from(binding))
}
