use crate::environment::TypeEnvironment;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_thir::thir::expression::{
    THIRCoercion, THIRExpression, THIRExpressionKind, THIRFnContract, THIRPostcondition,
};
use cx_thir::type_context::THIRTypeContext;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum PermissionTier {
    Safe,
    Nonsafe,
    Unsafe,
}

impl PermissionTier {
    pub(crate) fn of_function(safe: bool) -> Self {
        if safe { Self::Safe } else { Self::Nonsafe }
    }
}

/// Checks the permission required by each operation in one fully typechecked body.
///
/// This match is intentionally exhaustive. Adding a THIR expression variant
/// requires choosing its required permission here before the typechecker compiles.
pub(crate) fn check_permissions(
    env: &TypeEnvironment,
    expression: &THIRExpression,
    tier: PermissionTier,
) -> CXResult<()> {
    match &expression.kind {
        THIRExpressionKind::BoolLiteral(_)
        | THIRExpressionKind::IntLiteral(_)
        | THIRExpressionKind::FloatLiteral(_)
        | THIRExpressionKind::StringLiteral { .. }
        | THIRExpressionKind::Unit
        | THIRExpressionKind::SizeOf { .. }
        | THIRExpressionKind::AlignOf { .. }
        | THIRExpressionKind::Variable { .. }
        | THIRExpressionKind::GlobalVariable { .. }
        | THIRExpressionKind::ContractVariable { .. }
        | THIRExpressionKind::Unpack { .. } => Ok(()),

        THIRExpressionKind::Unsafe { expression } => {
            check_permissions(env, expression, PermissionTier::Unsafe)
        }
        THIRExpressionKind::Move { .. } if expression._type.is_unsafe_move() => require(
            env,
            expression,
            tier,
            PermissionTier::Nonsafe,
            "move of a type declared as @unsafe_move".into(),
        ),
        THIRExpressionKind::Move { .. } => Ok(()),

        THIRExpressionKind::Leak { .. } => {
            require(env, expression, tier, PermissionTier::Nonsafe, "@leak".into())
        }
        THIRExpressionKind::FunctionReference { name, debug_name } => {
            if callable_is_safe(env, expression) {
                Ok(())
            } else {
                let name = debug_name.as_ref().unwrap_or(name);
                require(
                    env,
                    expression,
                    tier,
                    PermissionTier::Nonsafe,
                    format!("call to non-safe function '{name}'"),
                )
            }
        }

        THIRExpressionKind::VaStart { list, last } => {
            check_permissions(env, list, tier)?;
            check_permissions(env, last, tier)
        }
        THIRExpressionKind::VaEnd { list } | THIRExpressionKind::VaArg { list, .. } => {
            check_permissions(env, list, tier)
        }

        THIRExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            check_permissions(env, lhs, tier)?;
            check_permissions(env, rhs, tier)
        }
        THIRExpressionKind::AddressOf { operand } if operand._type.is_function() => require(
            env,
            expression,
            tier,
            PermissionTier::Nonsafe,
            "Function address".into(),
        ),
        THIRExpressionKind::UnaryOperation { operand, .. }
        | THIRExpressionKind::Copy { source: operand } => check_permissions(env, operand, tier),
        THIRExpressionKind::AddressOf { operand } => check_permissions(env, operand, tier),

        THIRExpressionKind::CreateLocalVariable { initial_value, .. } => initial_value
            .as_deref()
            .map(|value| check_permissions(env, value, tier))
            .transpose()
            .map(|_| ()),

        THIRExpressionKind::AdoptRegion { initial_value, .. } => {
            check_permissions(env, initial_value, tier)
        }

        THIRExpressionKind::Assign { target, value } => {
            check_permissions(env, target, tier)?;
            check_permissions(env, value, tier)
        }

        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            let operation = match conversion {
                THIRCoercion::PtrToInt { .. } | THIRCoercion::IntToPtr { .. } => {
                    Some("Unsafe type conversion")
                }
                THIRCoercion::Bitcast
                    if operand._type.is_pointer() && expression._type.is_memory_reference() =>
                {
                    Some("Dereferencing a pointer")
                }
                THIRCoercion::Adopt => Some("@adopt"),
                _ => None,
            };
            if let Some(operation) = operation {
                require(env, expression, tier, PermissionTier::Nonsafe, operation.into())?;
            }
            check_permissions(env, operand, tier)
        }

        THIRExpressionKind::MemberAccess { base, .. } => check_permissions(env, base, tier),
        THIRExpressionKind::ArrayAccess { array, index, .. } => {
            check_permissions(env, array, tier)?;
            check_permissions(env, index, tier)
        }
        THIRExpressionKind::PatternIs { lhs, .. } => check_permissions(env, lhs, tier),
        THIRExpressionKind::TaggedUnionTag { value, .. } => check_permissions(env, value, tier),
        THIRExpressionKind::TaggedUnionSet {
            target,
            inner_value,
            ..
        } => {
            check_permissions(env, target, tier)?;
            check_permissions(env, inner_value, tier)
        }
        THIRExpressionKind::TaggedUnionInitializer { value, .. } => {
            check_permissions(env, value, tier)
        }
        THIRExpressionKind::ArrayInitializer { elements, .. } => check_all(env, elements, tier),
        THIRExpressionKind::StructInitializer {
            initializations, ..
        } => {
            for initialization in initializations {
                check_permissions(env, &initialization.value, tier)?;
            }
            Ok(())
        }

        THIRExpressionKind::Break { .. }
        | THIRExpressionKind::Continue { .. }
        | THIRExpressionKind::Unreachable
        | THIRExpressionKind::Goto { .. } => Ok(()),
        THIRExpressionKind::Label { statement, .. } => check_permissions(env, statement, tier),
        THIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            check_permissions(env, condition, tier)?;
            check_permissions(env, then_branch, tier)?;
            else_branch
                .as_deref()
                .map(|branch| check_permissions(env, branch, tier))
                .transpose()
                .map(|_| ())
        }
        THIRExpressionKind::While {
            condition, body, ..
        } => {
            check_permissions(env, condition, tier)?;
            check_permissions(env, body, tier)
        }
        THIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            check_permissions(env, init, tier)?;
            check_permissions(env, condition, tier)?;
            check_permissions(env, increment, tier)?;
            check_permissions(env, body, tier)
        }
        THIRExpressionKind::CSwitch {
            condition,
            cases,
            default,
        } => {
            check_permissions(env, condition, tier)?;
            for (case, body) in cases {
                check_permissions(env, case, tier)?;
                check_permissions(env, body, tier)?;
            }
            default
                .as_deref()
                .map(|branch| check_permissions(env, branch, tier))
                .transpose()
                .map(|_| ())
        }
        THIRExpressionKind::Match {
            condition, arms, ..
        } => {
            check_permissions(env, condition, tier)?;
            for (_, body) in arms {
                check_permissions(env, body, tier)?;
            }
            Ok(())
        }
        THIRExpressionKind::Return {
            postcondition,
            value,
        } => {
            value
                .as_deref()
                .map(|value| check_permissions(env, value, tier))
                .transpose()?;
            if let Some(postcondition) = postcondition {
                check_postcondition(env, postcondition, tier)?;
            }
            Ok(())
        }
        THIRExpressionKind::Yield { value, .. } => {
            value
                .as_deref()
                .map(|value| check_permissions(env, value, tier))
                .transpose()?;
            Ok(())
        }
        THIRExpressionKind::Defer { expression } => check_permissions(env, expression, tier),
        THIRExpressionKind::StagedExpression(staged) => {
            check_permissions(env, staged.expr(), tier)
        }
        THIRExpressionKind::Materialize { expr, with_params } => {
            check_permissions(env, expr, tier)?;
            check_all(env, with_params, tier)
        }
        THIRExpressionKind::Assert {
            condition: inner, ..
        } => check_permissions(env, inner, tier),
        THIRExpressionKind::Block { statements, .. } => check_all(env, statements, tier),
        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => {
            if !matches!(function.kind, THIRExpressionKind::FunctionReference { .. })
                && !callable_is_safe(env, function)
            {
                require(
                    env,
                    function,
                    tier,
                    PermissionTier::Nonsafe,
                    "Non-safe function call".into(),
                )?;
            }
            check_permissions(env, function, tier)?;
            check_all(env, arguments, tier)?;
            check_contract(
                env,
                contract,
                PermissionTier::of_function(callable_is_safe(env, function)),
            )
        }
    }
}

fn callable_is_safe(env: &TypeEnvironment, expression: &THIRExpression) -> bool {
    env.symbols
        .intern_signature(expression.get_type_ref())
        .is_some_and(|signature| signature.contract.safe)
}

fn check_contract(
    env: &TypeEnvironment,
    contract: &THIRFnContract,
    tier: PermissionTier,
) -> CXResult<()> {
    if let Some(precondition) = &contract.precondition {
        check_permissions(env, precondition, tier)?;
    }
    if let Some(postcondition) = &contract.postcondition {
        check_postcondition(env, postcondition, tier)?;
    }
    Ok(())
}

fn check_postcondition(
    env: &TypeEnvironment,
    postcondition: &THIRPostcondition,
    tier: PermissionTier,
) -> CXResult<()> {
    check_permissions(env, &postcondition.condition, tier)
}

fn check_all(
    env: &TypeEnvironment,
    expressions: &[THIRExpression],
    tier: PermissionTier,
) -> CXResult<()> {
    for expression in expressions {
        check_permissions(env, expression, tier)?;
    }
    Ok(())
}

fn require(
    env: &TypeEnvironment,
    expression: &THIRExpression,
    tier: PermissionTier,
    required: PermissionTier,
    operation: String,
) -> CXResult<()> {
    if tier >= required {
        return Ok(());
    }
    env.log_error(
        &expression.token_range,
        &catalogue::UNSAFE_OPERATION,
        operation,
    )
}
