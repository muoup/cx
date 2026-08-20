use cx_hir::ast::expression::HIRExpression;
use cx_hir::ast::modifiers::{HIR_CONST, HIRSymbolNameScheme};
use cx_hir::ast::types::HIRType;
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::expression_queries::{
    contains_function_reference, contains_global_reference, contains_null_pointer_conversion,
    contains_string_literal,
};
use cx_thir::thir::global::THIRGlobalVariable;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;
use cx_util::namespace::QualifiedName;

use crate::comptime::evaluate_comptime_expression;
use crate::environment::TypeEnvironment;
use crate::symbol::completion::{
    complete_type, completed_symbol_name, ensure_valid_type_component,
};
use crate::type_checking::typechecker::typecheck_expr;

pub(crate) fn lower_global(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    name: CXIdent,
    hir_type: &HIRType,
    linkage: LinkageMode,
    name_scheme: HIRSymbolNameScheme,
    initializer: Option<&HIRExpression>,
) -> CXResult<()> {
    let _type = complete_type(env, &namespace, hir_type)?;
    ensure_valid_type_component(env, hir_type.range(), &_type, "a global variable", true)?;
    println!("Lowering global variable: {}::{} of type {}", namespace, name, _type.display_with(&env.symbols));
    
    let symbol_name = completed_symbol_name(
        env,
        QualifiedName::new(namespace.clone(), name.clone()),
        name_scheme,
    );
    
    let (global_type, comptime_init) = initializer
        .as_ref()
        .map(|init| {
            let expression = typecheck_expr(env, &namespace, init, Some(&_type))
                .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
            let (global_type, expression) = match &expression.kind {
                THIRExpressionKind::TypeConversion {
                    conversion: THIRCoercion::ReinterpretBits,
                    operand,
                } if matches!(operand.kind, THIRExpressionKind::ArrayInitializer { .. }) => {
                    (operand._type.clone(), operand.as_ref().clone())
                }
                THIRExpressionKind::ArrayInitializer { .. } => {
                    (expression._type.clone(), expression)
                }
                THIRExpressionKind::TypeConversion {
                    conversion: THIRCoercion::ReinterpretBits,
                    operand,
                } if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) => {
                    (_type.clone(), expression)
                }
                _ => (_type.clone(), expression),
            };
            Ok((
                global_type,
                canonicalize_global_initializer(env, expression)?,
            ))
        })
        .transpose()?
        .unwrap_or_else(|| (_type.clone(), None));

    if !env.type_eq(&_type, &global_type) {
        let global_value_type = env.symbols.mem_ref_to(global_type.clone());
        env.symbols.insert_value(
            QualifiedName::new(namespace.clone(), name.clone()),
            THIRExpression {
                token_range: cx_tokens::TokenRange::internal(),
                kind: THIRExpressionKind::GlobalVariable {
                    symbol: CXIdent::new(symbol_name.clone()),
                },
                _type: global_value_type,
            },
        );
    }

    let global = THIRGlobalVariable {
        name: CXIdent::new(symbol_name),
        _type: global_type,

        is_mutable: _type.get_specifier(HIR_CONST),
        initializer: comptime_init,

        linkage,
    };

    env.items.push_generated_global(global);
    Ok(())
}

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
