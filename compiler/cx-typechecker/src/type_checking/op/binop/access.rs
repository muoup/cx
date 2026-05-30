use crate::environment::functions::query::{member_function_qualified_name, query_function};
use crate::environment::{BindingMoveState, TypeEnvironment};
use crate::log_typecheck_error;
use crate::type_checking::aggregate::fields::struct_field;
use crate::type_checking::op::binop::calls::build_function_reference;
use crate::type_checking::result::{BindingPlaceKind, TypecheckResult, TypecheckedBinding};
use crate::type_checking::value::locals::{ensure_binding_available, mark_binding};
use cx_ast::ast::expression::{CXExprKind, CXExpression};
use cx_ast::ast::function::CXReceiverMode;
use cx_ast::ast::modifiers::CX_CONST;
use cx_ast::symbols::UntypedSymbol;
use cx_mir::mir::data::{MIRType, MIRTypeKind};
use cx_mir::mir::expression::{MIRCoercion, MIRExpression, MIRExpressionKind};
use cx_mir::mir::program::EnvironmentNamespace;
use cx_util::CXResult;

pub(crate) fn resolve_access_base(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    lhs: MIRExpression,
) -> CXResult<(MIRExpression, MIRExpression, MIRType, bool)> {
    let lhs_source = lhs.clone();

    // Here, our aim is to continue with lhs_val being one indirection from the memory,
    // i.e. we need a pointer to the region.
    let mut lhs_ref_const = false;
    let mut lhs = lhs;
    let lhs_inner = loop {
        let lhs_type = lhs._type.clone();

        if let Some(inner_type) = env.symbols.context.mem_ref_inner(&lhs_type).cloned() {
            lhs_ref_const |= inner_type.get_specifier(CX_CONST);

            if let Some(ptr_inner) = env.symbols.context.ptr_inner(&inner_type).cloned() {
                lhs_ref_const |= ptr_inner.get_specifier(CX_CONST);

                lhs = MIRExpression {
                    token_range: None,
                    kind: MIRExpressionKind::RegionDuplicate {
                        source: Box::new(lhs),
                    },
                    _type: env.symbols.context.pointer_to(ptr_inner.clone()),
                };

                break ptr_inner;
            }

            if env.symbols.context.mem_ref_inner(&inner_type).is_some() {
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
        } else if let Some(inner_type) = env.symbols.context.ptr_inner(&lhs_type).cloned() {
            lhs_ref_const |= inner_type.get_specifier(CX_CONST);
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
            lhs_inner.display_with(&env.symbols.context)
        );
    }

    Ok((lhs_source, lhs, lhs_inner, lhs_ref_const))
}

pub(crate) fn typecheck_access(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    lhs: TypecheckResult,
    rhs: &CXExpression,
    expr: &CXExpression,
) -> CXResult<TypecheckResult> {
    let lhs_binding = lhs.binding().cloned();
    let lhs = lhs.into_expression()?;
    let (lhs_source, lhs, lhs_inner, lhs_ref_const) = resolve_access_base(env, expr, lhs)?;

    match &rhs.kind {
        CXExprKind::Identifier(name) => {
            if let Some(struct_field) =
                struct_field(&lhs_inner, &env.symbols.context, name.name.as_str())
            {
                let mut result = TypecheckResult::new_base(
                    env.symbols.context.mem_ref_to(
                        struct_field
                            .field_type
                            .clone()
                            .with_specifier(if lhs_ref_const { CX_CONST } else { 0 }),
                    ),
                    MIRExpressionKind::MemberAccess {
                        base: Box::new(lhs),
                        member_index: struct_field.index,
                        aggregate_type: lhs_inner.clone(),
                    },
                );

                if let Some(binding) = lhs_binding.as_ref() {
                    result = result.with_binding(binding.project());
                }

                return Ok(result);
            }

            let member_arg_types = vec![lhs_inner.clone()];
            let Some(function_name) = member_function_qualified_name(&lhs_inner, &name.name) else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Member '{}' not found on type '{}'",
                    name,
                    lhs_inner.display_with(&env.symbols.context)
                );
            };

            if let Some(prototype) = query_function(
                env,
                namespace,
                expr,
                &function_name,
                None,
                &member_arg_types,
            )? {
                let Some(receiver_mode) =
                    prototype.source_prototype.kind.receiver().map(|v| v.mode)
                else {
                    return log_typecheck_error!(
                        env,
                        Some(expr.token_range()),
                        "Member '{}' not found on type '{}'",
                        name,
                        lhs_inner.display_with(&env.symbols.context)
                    );
                };
                let receiver = build_member_receiver_argument(
                    env,
                    expr,
                    &lhs_source,
                    lhs_binding.as_ref(),
                    lhs,
                    &lhs_inner,
                    receiver_mode,
                )?;

                return Ok(TypecheckResult::from(build_function_reference(&prototype))
                    .with_implicit_parameters(vec![receiver]));
            }

            if let Some(UntypedSymbol::FunctionTemplate(template, _)) =
                env.symbols.global_symbols.resolve(&function_name)
            {
                let Some(receiver_mode) = template.resource.shell.kind.receiver().map(|v| v.mode)
                else {
                    return log_typecheck_error!(
                        env,
                        Some(expr.token_range()),
                        "Member '{}' not found on type '{}'",
                        name,
                        lhs_inner.display_with(&env.symbols.context)
                    );
                };
                let receiver = build_member_receiver_argument(
                    env,
                    expr,
                    &lhs_source,
                    lhs_binding.as_ref(),
                    lhs,
                    &lhs_inner,
                    receiver_mode,
                )?;

                return Ok(
                    TypecheckResult::incomplete_templated_callee(function_name, None)
                        .with_implicit_parameters(vec![receiver])
                        .with_deduction_arg_prefix(member_arg_types),
                );
            }

            log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Member '{}' not found on type '{}'",
                name,
                lhs_inner.display_with(&env.symbols.context)
            )
        }

        CXExprKind::TemplatedIdentifier {
            name,
            template_input,
        } => {
            let member_arg_types = vec![lhs_inner.clone()];
            let prototype = member_function_qualified_name(&lhs_inner, &name.name)
                .map(|function_name| {
                    query_function(
                        env,
                        namespace,
                        expr,
                        &function_name,
                        Some(template_input),
                        &member_arg_types,
                    )
                })
                .transpose()?
                .flatten();

            let Some(prototype) = prototype else {
                return log_typecheck_error!(
                    env,
                    Some(expr.token_range()),
                    "Member function '{}<...>' not found on type '{}'",
                    name,
                    lhs_inner.display_with(&env.symbols.context)
                );
            };
            let receiver = build_member_receiver_argument(
                env,
                expr,
                &lhs_source,
                lhs_binding.as_ref(),
                lhs,
                &lhs_inner,
                prototype
                    .source_prototype
                    .kind
                    .receiver()
                    .map(|v| v.mode)
                    .unwrap_or(CXReceiverMode::None),
            )?;

            Ok(TypecheckResult::from(build_function_reference(&prototype))
                .with_implicit_parameters(vec![receiver]))
        }

        _ => log_typecheck_error!(
            env,
            Some(expr.token_range()),
            "Invalid right-hand side for access expression, found {:?}",
            rhs
        ),
    }
}

pub(crate) fn build_member_receiver_argument(
    env: &mut TypeEnvironment,
    expr: &CXExpression,
    lhs_source: &MIRExpression,
    lhs_binding: Option<&TypecheckedBinding>,
    lhs: MIRExpression,
    lhs_inner: &MIRType,
    receiver_mode: CXReceiverMode,
) -> CXResult<MIRExpression> {
    match receiver_mode {
        CXReceiverMode::None => {
            unreachable!("member function reference missing receiver mode")
        }
        CXReceiverMode::ByRef => {
            if let Some(binding) = lhs_binding {
                ensure_binding_available(env, Some(expr.token_range().clone()), &binding.root)?;
            }

            Ok(MIRExpression {
                token_range: None,
                kind: MIRExpressionKind::TypeConversion {
                    operand: Box::new(lhs),
                    conversion: MIRCoercion::ReinterpretBits,
                },
                _type: env.symbols.context.mem_ref_to(lhs_inner.clone()),
            })
        }
        CXReceiverMode::ByMove => {
            if let Some(inner_type) = env
                .symbols
                .context
                .mem_ref_inner(&lhs_source._type)
                .cloned()
            {
                let Some(binding) = lhs_binding else {
                    return log_typecheck_error!(
                        env,
                        expr.token_range(),
                        "Consuming member calls currently require a named binding or owned struct rvalue"
                    );
                };

                if binding.kind != BindingPlaceKind::Local {
                    return log_typecheck_error!(
                        env,
                        expr.token_range(),
                        "Consuming member calls on aggregate fields or projections are not implemented"
                    );
                }

                ensure_binding_available(env, Some(expr.token_range().clone()), &binding.root)?;
                if env.symbols.is_nocopy(&inner_type) {
                    mark_binding(env, binding, BindingMoveState::Moved);
                }

                Ok(MIRExpression {
                    token_range: None,
                    _type: inner_type,
                    kind: MIRExpressionKind::RegionMove {
                        source: Box::new(lhs_source.clone()),
                    },
                })
            } else {
                Ok(lhs_source.clone())
            }
        }
    }
}
