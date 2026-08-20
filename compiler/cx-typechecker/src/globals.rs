use cx_log::CXResult;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::expression_queries::{
    contains_function_reference, contains_global_reference, contains_null_pointer_conversion,
    contains_string_literal,
};

use crate::comptime::evaluate_comptime_expression;
use crate::environment::TypeEnvironment;

pub(crate) fn canonicalize_global_initializer(
    env: &mut TypeEnvironment,
    expression: THIRExpression,
) -> CXResult<Option<THIRExpression>> {
    let token_range = expression.token_range.clone();
    let _type = expression._type.clone();
    let kind = expression.kind;
    let expression = match kind {
        THIRExpressionKind::ArrayInitializer {
            elements,
            element_type,
        } => {
            let elements = elements
                .into_iter()
                .map(|element| canonicalize_global_initializer(env, element))
                .collect::<CXResult<Vec<_>>>()?
                .into_iter()
                .map(|element| element.expect("global aggregate elements are scalar or aggregate"))
                .collect();
            THIRExpression {
                kind: THIRExpressionKind::ArrayInitializer {
                    elements,
                    element_type,
                },
                _type,
                token_range,
            }
        }
        THIRExpressionKind::StructInitializer {
            initializations,
            struct_type,
        } => {
            let initializations = initializations
                .into_iter()
                .map(|initialization| {
                    Ok(cx_thir::thir::expression::StructInitialization {
                        field_index: initialization.field_index,
                        value: canonicalize_global_initializer(env, initialization.value)?
                            .expect("global aggregate fields are scalar or aggregate"),
                    })
                })
                .collect::<CXResult<Vec<_>>>()?;
            THIRExpression {
                kind: THIRExpressionKind::StructInitializer {
                    initializations,
                    struct_type,
                },
                _type,
                token_range,
            }
        }
        kind @ THIRExpressionKind::TypeConversion { .. } if contains_string_literal(&kind) => {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        kind @ THIRExpressionKind::TypeConversion {
            conversion: THIRCoercion::IntToPtr { .. },
            ..
        } => {
            if matches!(
                &kind,
                THIRExpressionKind::TypeConversion { operand, .. }
                    if matches!(operand.kind, THIRExpressionKind::IntLiteral(0))
            ) {
                THIRExpression {
                    kind,
                    _type,
                    token_range,
                }
            } else {
                evaluate_comptime_expression(
                    env,
                    THIRExpression {
                        kind,
                        _type,
                        token_range,
                    },
                )
                .map(|value| value.into_expression())?
            }
        }
        kind @ THIRExpressionKind::TypeConversion { .. }
            if contains_null_pointer_conversion(&kind) =>
        {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        kind @ THIRExpressionKind::TypeConversion {
            conversion: THIRCoercion::ReinterpretBits,
            ..
        } if matches!(
            &kind,
            THIRExpressionKind::TypeConversion { operand, .. }
                if contains_global_reference(operand)
        ) =>
        {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        THIRExpressionKind::TypeConversion {
            conversion,
            operand,
        } if contains_global_reference(&operand) => THIRExpression {
            kind: THIRExpressionKind::TypeConversion {
                conversion,
                operand,
            },
            _type,
            token_range,
        },
        THIRExpressionKind::Typechange(operand)
            if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) =>
        {
            THIRExpression {
                kind: THIRExpressionKind::Typechange(operand),
                _type,
                token_range,
            }
        }
        THIRExpressionKind::GlobalVariable { symbol } => THIRExpression {
            kind: THIRExpressionKind::GlobalVariable { symbol },
            _type,
            token_range,
        },
        THIRExpressionKind::StringLiteral { value } => THIRExpression {
            kind: THIRExpressionKind::StringLiteral { value },
            _type,
            token_range,
        },
        THIRExpressionKind::Copy { source } => {
            if contains_global_reference(&source) {
                THIRExpression {
                    kind: THIRExpressionKind::Copy { source },
                    _type,
                    token_range,
                }
            } else {
                evaluate_comptime_expression(
                    env,
                    THIRExpression {
                        kind: THIRExpressionKind::Copy { source },
                        _type,
                        token_range,
                    },
                )
                .map(|value| value.into_expression())?
            }
        }
        THIRExpressionKind::FunctionReference { .. } => THIRExpression {
            kind,
            _type,
            token_range,
        },
        kind @ THIRExpressionKind::Typechange(_) if contains_function_reference(&kind) => {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        kind @ THIRExpressionKind::TypeConversion { .. } if contains_function_reference(&kind) => {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        kind => evaluate_comptime_expression(
            env,
            THIRExpression {
                kind,
                _type,
                token_range,
            },
        )
        .map(|value| value.into_expression())?,
    };

    Ok(Some(expression))
}
