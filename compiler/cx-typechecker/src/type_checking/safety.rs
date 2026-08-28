use crate::environment::TypeEnvironment;
use cx_log::CXResult;
use cx_thir::thir::expression::{
    THIRCoercion, THIRExpression, THIRExpressionKind, THIRFnContract, THIRPostcondition,
};
use cx_thir::type_context::THIRTypeContext;

const UNSAFE_EXPRESSION_MESSAGE: &str =
    "Expression is not supported in safe contexts; wrap it in `@unsafe`";

/// Checks the explicit safe-expression whitelist for one fully typechecked body.
///
/// This match is intentionally exhaustive. Adding a THIR expression variant
/// requires choosing its safe semantics here before the typechecker compiles.
pub(crate) fn validate_safe_expression(
    env: &TypeEnvironment,
    expression: &THIRExpression,
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
        | THIRExpressionKind::Unsafe { .. }
        | THIRExpressionKind::Move { .. }
        | THIRExpressionKind::LifetimeStart { .. }
        | THIRExpressionKind::LifetimeEnd { .. } => Ok(()),

        THIRExpressionKind::Unpack { .. } => Ok(()),
        THIRExpressionKind::LeakLifetime { .. } => reject(env, expression),

        THIRExpressionKind::FunctionReference { .. } => validate_callable(env, expression),

        THIRExpressionKind::VaStart { list, last } => {
            validate_safe_expression(env, list)?;
            validate_safe_expression(env, last)
        }
        THIRExpressionKind::VaEnd { list } | THIRExpressionKind::VaArg { list, .. } => {
            validate_safe_expression(env, list)
        }

        THIRExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            validate_safe_expression(env, lhs)?;
            validate_safe_expression(env, rhs)
        }
        THIRExpressionKind::UnaryOperation { operand, .. }
        | THIRExpressionKind::Copy { source: operand } => validate_safe_expression(env, operand),

        THIRExpressionKind::CreateLocalVariable { initial_value, .. } => initial_value
            .as_deref()
            .map(|value| validate_safe_expression(env, value))
            .transpose()
            .map(|_| ()),

        THIRExpressionKind::Assign { target, value } => {
            validate_safe_expression(env, target)?;
            validate_safe_expression(env, value)
        }

        THIRExpressionKind::Typechange(inner) => {
            if inner._type.is_pointer() {
                reject(env, expression)
            } else {
                validate_safe_expression(env, inner)
            }
        }
        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            if matches!(
                conversion,
                THIRCoercion::PtrToInt { .. }
                    | THIRCoercion::IntToPtr { .. }
                    | THIRCoercion::GetFnPtr
            ) {
                reject(env, expression)
            } else {
                validate_safe_expression(env, operand)
            }
        }

        THIRExpressionKind::MemberAccess { base, .. } => validate_safe_expression(env, base),
        THIRExpressionKind::ArrayAccess { array, index, .. } => {
            validate_safe_expression(env, array)?;
            validate_safe_expression(env, index)
        }
        THIRExpressionKind::PatternIs { lhs, .. } => validate_safe_expression(env, lhs),
        THIRExpressionKind::TaggedUnionTag { value, .. }
        | THIRExpressionKind::TaggedUnionGet { value, .. } => validate_safe_expression(env, value),
        THIRExpressionKind::TaggedUnionSet {
            target,
            inner_value,
            ..
        } => {
            validate_safe_expression(env, target)?;
            validate_safe_expression(env, inner_value)
        }
        THIRExpressionKind::TaggedUnionInitializer { value, .. } => {
            validate_safe_expression(env, value)
        }
        THIRExpressionKind::ArrayInitializer { elements, .. } => validate_all(env, elements),
        THIRExpressionKind::StructInitializer {
            initializations, ..
        } => {
            for initialization in initializations {
                validate_safe_expression(env, &initialization.value)?;
            }
            Ok(())
        }

        THIRExpressionKind::Break { .. }
        | THIRExpressionKind::Continue { .. }
        | THIRExpressionKind::Unreachable
        | THIRExpressionKind::Goto { .. } => Ok(()),
        THIRExpressionKind::Label { statement, .. } => validate_safe_expression(env, statement),
        THIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            validate_safe_expression(env, condition)?;
            validate_safe_expression(env, then_branch)?;
            else_branch
                .as_deref()
                .map(|branch| validate_safe_expression(env, branch))
                .transpose()
                .map(|_| ())
        }
        THIRExpressionKind::While {
            condition, body, ..
        } => {
            validate_safe_expression(env, condition)?;
            validate_safe_expression(env, body)
        }
        THIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            validate_safe_expression(env, init)?;
            validate_safe_expression(env, condition)?;
            validate_safe_expression(env, increment)?;
            validate_safe_expression(env, body)
        }
        THIRExpressionKind::CSwitch {
            condition,
            cases,
            default,
        } => {
            validate_safe_expression(env, condition)?;
            for (case, body) in cases {
                validate_safe_expression(env, case)?;
                validate_safe_expression(env, body)?;
            }
            default
                .as_deref()
                .map(|branch| validate_safe_expression(env, branch))
                .transpose()
                .map(|_| ())
        }
        THIRExpressionKind::Match {
            condition,
            arms,
            default,
            ..
        } => {
            validate_safe_expression(env, condition)?;
            for (_, body) in arms {
                validate_safe_expression(env, body)?;
            }
            default
                .as_deref()
                .map(|branch| validate_safe_expression(env, branch))
                .transpose()
                .map(|_| ())
        }
        THIRExpressionKind::Return {
            postcondition,
            value,
        } => {
            value
                .as_deref()
                .map(|value| validate_safe_expression(env, value))
                .transpose()?;
            if let Some(postcondition) = postcondition {
                validate_postcondition(env, postcondition)?;
            }
            Ok(())
        }
        THIRExpressionKind::Yield { value, .. } => {
            value
                .as_deref()
                .map(|value| validate_safe_expression(env, value))
                .transpose()?;
            Ok(())
        }
        THIRExpressionKind::Defer { expression } => validate_safe_expression(env, expression),
        THIRExpressionKind::StagedExpression(staged) => {
            validate_safe_expression(env, staged.expr())
        }
        THIRExpressionKind::MaterializeStagedExpression { expr, with_params } => {
            validate_safe_expression(env, expr)?;
            validate_all(env, with_params)
        }
        THIRExpressionKind::Assert {
            condition: inner, ..
        } => validate_safe_expression(env, inner),
        THIRExpressionKind::Block { statements, .. } => validate_all(env, statements),
        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract,
        } => {
            validate_callable(env, function)?;
            validate_safe_expression(env, function)?;
            validate_all(env, arguments)?;
            validate_contract(env, contract)
        }
    }
}

fn validate_callable(env: &TypeEnvironment, expression: &THIRExpression) -> CXResult<()> {
    if env
        .symbols
        .intern_signature(expression.get_type_ref())
        .is_some_and(|signature| signature.contract.safe)
    {
        Ok(())
    } else {
        reject(env, expression)
    }
}

fn validate_contract(env: &TypeEnvironment, contract: &THIRFnContract) -> CXResult<()> {
    if let Some(precondition) = &contract.precondition {
        validate_safe_expression(env, precondition)?;
    }
    if let Some(postcondition) = &contract.postcondition {
        validate_postcondition(env, postcondition)?;
    }
    Ok(())
}

fn validate_postcondition(
    env: &TypeEnvironment,
    postcondition: &THIRPostcondition,
) -> CXResult<()> {
    validate_safe_expression(env, &postcondition.condition)
}

fn validate_all(env: &TypeEnvironment, expressions: &[THIRExpression]) -> CXResult<()> {
    for expression in expressions {
        validate_safe_expression(env, expression)?;
    }
    Ok(())
}

fn reject<T>(env: &TypeEnvironment, expression: &THIRExpression) -> CXResult<T> {
    env.log_error(&expression.token_range, UNSAFE_EXPRESSION_MESSAGE)
}
