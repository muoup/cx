use super::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_util::identifier::CXIdent;

pub fn global_reference_symbol(expression: &THIRExpression) -> Option<&CXIdent> {
    match &expression.kind {
        THIRExpressionKind::GlobalVariable { symbol } => Some(symbol),
        THIRExpressionKind::Copy { source }
        | THIRExpressionKind::Typechange(source)
        | THIRExpressionKind::TypeConversion {
            operand: source, ..
        } => global_reference_symbol(source),
        _ => None,
    }
}

pub fn contains_global_reference(expression: &THIRExpression) -> bool {
    match &expression.kind {
        THIRExpressionKind::GlobalVariable { .. } => true,
        THIRExpressionKind::Copy { source }
        | THIRExpressionKind::Typechange(source)
        | THIRExpressionKind::TypeConversion {
            operand: source, ..
        } => contains_global_reference(source),
        THIRExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            contains_global_reference(lhs) || contains_global_reference(rhs)
        }
        _ => false,
    }
}

pub fn contains_null_pointer_conversion(kind: &THIRExpressionKind) -> bool {
    match kind {
        THIRExpressionKind::Typechange(operand) => contains_null_pointer_conversion(&operand.kind),
        THIRExpressionKind::TypeConversion {
            conversion: cx,
            operand,
        } => {
            (matches!(cx, THIRCoercion::IntToPtr { .. })
                && matches!(operand.kind, THIRExpressionKind::IntLiteral(0)))
                || contains_null_pointer_conversion(&operand.kind)
        }
        _ => false,
    }
}

pub fn function_reference_symbol(expression: &THIRExpression) -> Option<&CXIdent> {
    match &expression.kind {
        THIRExpressionKind::FunctionReference { name, .. } => Some(name),
        THIRExpressionKind::Typechange(operand)
        | THIRExpressionKind::TypeConversion { operand, .. } => function_reference_symbol(operand),
        _ => None,
    }
}

pub fn contains_function_reference(kind: &THIRExpressionKind) -> bool {
    match kind {
        THIRExpressionKind::FunctionReference { .. } => true,
        THIRExpressionKind::Typechange(operand)
        | THIRExpressionKind::TypeConversion { operand, .. } => {
            contains_function_reference(&operand.kind)
        }
        _ => false,
    }
}

pub fn contains_string_literal(kind: &THIRExpressionKind) -> bool {
    match kind {
        THIRExpressionKind::StringLiteral { .. } => true,
        THIRExpressionKind::Copy { source }
        | THIRExpressionKind::Typechange(source)
        | THIRExpressionKind::TypeConversion {
            operand: source, ..
        } => contains_string_literal(&source.kind),
        _ => false,
    }
}
