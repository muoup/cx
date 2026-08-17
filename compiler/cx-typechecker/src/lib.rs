use cx_hir::ast::modifiers::HIR_CONST;
use cx_hir::decomposition::{HIRGenerationAST, HIRGenerationStmt};
use cx_log::CXResult;
use cx_thir::EnvironmentNamespace;
use cx_thir::thir::expression::{THIRCoercion, THIRExpression, THIRExpressionKind};
use cx_thir::thir::global::{MIRGlobalVarKind, MIRGlobalVariable};
use cx_util::linkage::LinkageMode;

pub mod environment;
pub mod log;
pub mod symbol;

pub(crate) mod requests;

pub(crate) mod comptime;
mod type_checking;

use crate::comptime::evaluate_comptime_expression;
use crate::requests::fulfill_requests;
use crate::symbol::completion::{complete_prototype, complete_type, completed_symbol_name};
use crate::type_checking::typechecker::typecheck_expr;
use crate::{environment::TypeEnvironment, type_checking::functions::typecheck_function};
use cx_util::{identifier::CXIdent, namespace::QualifiedName};

pub fn typecheck(
    env: &mut TypeEnvironment,
    namespace: &EnvironmentNamespace,
    ast: &HIRGenerationAST,
) -> CXResult<()> {
    for stmt in ast.generation_stmts.iter() {
        match stmt {
            HIRGenerationStmt::Function { prototype, body } => {
                let prototype = complete_prototype(env, namespace, prototype)?;
                typecheck_function(env, namespace, prototype.clone(), body)?;
            }

            HIRGenerationStmt::StringLiteral { name, value } => {
                let global = MIRGlobalVariable {
                    is_mutable: false,
                    linkage: LinkageMode::Static,
                    kind: MIRGlobalVarKind::StringLiteral {
                        name: name.clone(),
                        value: value.clone(),
                    },
                };

                env.items.push_generated_global(global);
            }

            HIRGenerationStmt::AddressableGlobal {
                name,
                _type,
                linkage,
                symbol_naming,
                initializer,
            } => {
                let _type = complete_type(env, namespace, _type)?;
                let symbol_name = completed_symbol_name(
                    env,
                    QualifiedName::new(namespace.clone(), name.clone()),
                    *symbol_naming,
                );
                let (global_type, comptime_init) = initializer
                    .as_ref()
                    .map(|init| {
                        let expression = typecheck_expr(env, namespace, init, Some(&_type))
                            .and_then(|tc| tc.standard_ready_coerce(env, init.token_range()))?;
                        let (global_type, expression) = match &expression.kind {
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(operand.kind, THIRExpressionKind::ArrayInitializer { .. }) =>
                            {
                                (operand._type.clone(), operand.as_ref().clone())
                            }
                            THIRExpressionKind::ArrayInitializer { .. } => {
                                (expression._type.clone(), expression)
                            }
                            THIRExpressionKind::TypeConversion {
                                conversion: THIRCoercion::ReinterpretBits,
                                operand,
                            } if matches!(operand.kind, THIRExpressionKind::GlobalVariable { .. }) =>
                            {
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

                let global = MIRGlobalVariable {
                    is_mutable: _type.get_specifier(HIR_CONST),
                    linkage: *linkage,
                    kind: MIRGlobalVarKind::Variable {
                        name: CXIdent::new(symbol_name),
                        _type: global_type,
                        initializer: comptime_init,
                    },
                };

                env.items.push_generated_global(global);
            }
        }
    }

    fulfill_requests(env)?;

    Ok(())
}

fn canonicalize_global_initializer(
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
        ) => THIRExpression {
            kind,
            _type,
            token_range,
        },
        THIRExpressionKind::TypeConversion { conversion, operand }
            if contains_global_reference(&operand) => THIRExpression {
            kind: THIRExpressionKind::TypeConversion { conversion, operand },
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
        kind @ THIRExpressionKind::Typechange(_)
            if contains_function_reference_kind(&kind) =>
        {
            THIRExpression {
                kind,
                _type,
                token_range,
            }
        }
        kind @ THIRExpressionKind::TypeConversion { .. }
            if contains_function_reference_kind(&kind) =>
        {
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
        .map(|value| value.into_expression())?
    };

    Ok(Some(expression))
}

fn contains_global_reference(expression: &THIRExpression) -> bool {
    match &expression.kind {
        THIRExpressionKind::GlobalVariable { .. } => true,
        THIRExpressionKind::Copy { source } => contains_global_reference(source),
        THIRExpressionKind::Typechange(operand)
        | THIRExpressionKind::TypeConversion { operand, .. } => {
            contains_global_reference(operand)
        }
        THIRExpressionKind::BinaryOperation { lhs, rhs, .. } => {
            contains_global_reference(lhs) || contains_global_reference(rhs)
        }
        _ => false,
    }
}

fn contains_null_pointer_conversion(kind: &THIRExpressionKind) -> bool {
    match kind {
        THIRExpressionKind::Typechange(operand) => {
            contains_null_pointer_conversion(&operand.kind)
        }
        THIRExpressionKind::TypeConversion {
            conversion: THIRCoercion::IntToPtr { .. },
            operand,
        } => matches!(operand.kind, THIRExpressionKind::IntLiteral(0))
            || contains_null_pointer_conversion(&operand.kind),
        THIRExpressionKind::TypeConversion { operand, .. } => {
            contains_null_pointer_conversion(&operand.kind)
        }
        _ => false,
    }
}

fn contains_function_reference_kind(kind: &THIRExpressionKind) -> bool {
    match kind {
        THIRExpressionKind::FunctionReference { .. } => true,
        THIRExpressionKind::Typechange(operand)
        | THIRExpressionKind::TypeConversion { operand, .. } => {
            contains_function_reference_kind(&operand.kind)
        }
        _ => false,
    }
}
