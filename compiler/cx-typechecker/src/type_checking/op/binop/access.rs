use crate::environment::TypeEnvironment;
use crate::symbol::{completion::complete_template_input, resolution::apply_template};
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::result::{
    IncompleteCalleeContext, PendingReceiver, TypecheckExtract, TypecheckResult,
};
use crate::type_checking::value::locals::ensure_binding_available;
use crate::type_checking::value::moves::typecheck_move;
use crate::{log_typecheck_error, typecheck_error};
use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_ast::ast::modifiers::CX_CONST;
use cx_log::CXResult;
use cx_mir::EnvironmentNamespace;
use cx_mir::mir::data::{MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRExpression, MIRExpressionKind};
use cx_mir::type_context::MIRTypeContext;

struct AccessBase {
    source: MIRExpression,
    source_type: MIRType,
}

fn resolve_access_base(
    env: &mut TypeEnvironment,
    _: &EnvironmentNamespace,
    expr: &CXExpression,
    lhs: MIRExpression,
) -> CXResult<AccessBase> {
    // Here, our aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let mut lhs = lhs;
    let lhs_inner = loop {
        let lhs_type = lhs._type.clone();

        if let Some(inner_type) = env.symbols.mem_ref_inner(&lhs_type).cloned() {
            if let Some(ptr_inner) = env.symbols.ptr_inner(&inner_type).cloned() {
                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: env.symbols.pointer_to(ptr_inner.clone()),
                };

                break ptr_inner;
            }

            if env.symbols.mem_ref_inner(&inner_type).is_some() {
                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: inner_type.clone(),
                };
            } else {
                break inner_type;
            }
        } else if let Some(inner_type) = env.symbols.ptr_inner(&lhs_type).cloned() {
            break inner_type;
        } else {
            break lhs_type;
        }
    };

    if !matches!(
        lhs_inner.kind,
        MIRTypeKind::Structured { .. }
            | MIRTypeKind::Union { .. }
            | MIRTypeKind::TaggedUnion { .. }
    ) {
        return log_typecheck_error!(
            env,
            expr.token_range(),
            "Expected a struct or union type on the left-hand side of an access expression, found {}",
            lhs_inner.display_with(&env.symbols)
        );
    }

    Ok(AccessBase {
        source: lhs,
        source_type: lhs_inner,
    })
}

pub fn typecheck_access(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: TypecheckResult,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    ensure_binding_available(env, Some(expr.token_range()), lhs.binding())?;
    let lhs_binding = lhs.binding().cloned();

    let base = resolve_access_base(
        env,
        namespace,
        expr,
        lhs.standard_ready_coerce(env, expr.token_range())?,
    )?;

    match &rhs.kind {
        CXExprKind::Identifier {
            name,
            template_input,
        } => {
            let Some(name) = name.clone().root_name() else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Expected an identifier without a namespace on the right-hand side of an access expression, found '{}'",
                    name
                );
            };

            if template_input.is_none()
                && let Some(struct_field) =
                    struct_field(&env.symbols, &base.source_type, name.as_str())
            {
                // First, we check if we are trying to access a struct member
                let mut result = TypecheckResult::new(
                    env.symbols
                        .mem_ref_to(struct_field.field_type.clone().with_specifier(
                            if base.source_type.get_specifier(CX_CONST) {
                                CX_CONST
                            } else {
                                0
                            },
                        )),
                    MIRExpressionKind::MemberAccess {
                        base: Box::new(base.source),
                        member_index: struct_field.index,
                        aggregate_type: base.source_type.clone(),
                    },
                );

                if let Some(binding) = lhs_binding.as_ref().map(|binding| binding.project()) {
                    result = result.with_binding(binding);
                }

                return Ok(result);
            }

            let Some(lookup_name) = base.source_type.member_lookup_identifier().cloned() else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Cannot access member '{}' on type '{}', which does not have a lookup identifier",
                    name,
                    base.source_type.display_with(&env.symbols)
                );
            };

            let query = lookup_name.child(name.clone());
            let mut symbol = env.get_symbol(namespace, &query)?.ok_or_else(|| {
                typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Member '{}' not found on type '{}'",
                    name,
                    base.source_type.display_with(&env.symbols)
                )
            })?;

            if let Some(completed_input) = template_input
                .as_ref()
                .map(|input| complete_template_input(env, namespace, input))
                .transpose()?
            {
                symbol = apply_template(env, &symbol, completed_input)?.ok_or_else(|| {
                    typecheck_error!(
                        env,
                        Some(expr.token_range()),
                        "Member '{}' on type '{}' does not accept template arguments",
                        name,
                        base.source_type.display_with(&env.symbols)
                    )
                })?;
            }

            let deduction_receiver_type = env.symbols.mem_ref_to(base.source_type.clone());
            let member_result = TypecheckResult::from_symbol(
                symbol,
                query,
                template_input.clone(),
                IncompleteCalleeContext::member(
                    deduction_receiver_type,
                    PendingReceiver::new(base.source.clone(), lhs_binding.clone()),
                ),
            )
            .map_err(|err| {
                typecheck_error!(env, Some(expr.token_range()), "{}", err.error_content())
            })?;

            let function = match member_result.try_into_expression() {
                TypecheckExtract::Succ(function) => function,
                TypecheckExtract::Fail(member_result) => return Ok(member_result),
            };

            let MIRTypeKind::Function { signature } = &function._type.kind else {
                unreachable!("function references must have function type")
            };

            let needs_move = signature
                .params
                .get(0)
                .map(|param| !param._type.is_memory_reference())
                .unwrap_or(false);

            let receiver = if needs_move {
                let mut receiver = TypecheckResult::from(base.source);
                if let Some(binding) = lhs_binding {
                    receiver = receiver.with_binding(binding);
                }

                typecheck_move(env, namespace, receiver, expr)
                    .and_then(|v| v.standard_ready_coerce(env, expr.token_range()))?
            } else {
                base.source
            };

            Ok(TypecheckResult::from(function).with_implicit_parameters(vec![receiver]))
        }

        _ => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Invalid right-hand side for access expression, found {:?}",
            rhs
        ),
    }
}
