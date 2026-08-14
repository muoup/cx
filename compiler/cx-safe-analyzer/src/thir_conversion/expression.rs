use crate::{log::AnalysisDiagnosticSource, thir_conversion::factories::*};
use cx_log::CXResult;
use cx_safe_ir::{
    ast::{CVMOperation, FMIRNode, FMIRNodeBody, FMIRType, FRc, MemoryLocation},
    intrinsic::FMIRIntrinsicKind,
};
use cx_thir::thir::{
    data::{THIRType, THIRTypeKind},
    expression::{THIRExpression, THIRExpressionKind, THIRUnOp},
};
use cx_tokens::TokenRange;

use crate::thir_conversion::environment::FMIREnvironment;

pub fn convert_expression(
    env: &mut FMIREnvironment,
    thir_expr: &THIRExpression,
) -> CXResult<FMIRNode> {
    let node = match &thir_expr.kind {
        THIRExpressionKind::BoolLiteral(value) => Ok(FMIRNode {
            token_range: TokenRange::internal(),
            body: FMIRNodeBody::BooleanLiteral(*value),
            _type: FMIRType::Pure {
                thir_type: THIRType::bool(),
            },
        }),

        THIRExpressionKind::IntLiteral(value) => {
            let THIRTypeKind::Integer {
                _type: itype,
                signed,
            } = &thir_expr._type.kind
            else {
                unreachable!("FMIR conversion expected integer type in integer literal expression")
            };

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::IntegerLiteral(*value),
                _type: FMIRType::Pure {
                    thir_type: THIRType::from(THIRTypeKind::Integer {
                        _type: *itype,
                        signed: *signed,
                    }),
                },
            })
        }

        THIRExpressionKind::FloatLiteral(value) => {
            let THIRTypeKind::Float { _type: ftype } = &thir_expr._type.kind else {
                unreachable!("FMIR conversion expected float type in float literal expression")
            };

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::FloatLiteral(value.into()),
                _type: FMIRType::Pure {
                    thir_type: THIRTypeKind::Float { _type: *ftype }.into(),
                },
            })
        }
        THIRExpressionKind::Unit => Ok(FMIRNode::unit()),

        THIRExpressionKind::Block { statements } => {
            env.push_scope();
            let nodes = statements
                .iter()
                .map(|statement| convert_expression(env, statement))
                .collect::<CXResult<Vec<_>>>()?;
            env.pop_scope();

            Ok(chain_statements(nodes))
        }

        THIRExpressionKind::RegionCreate {
            _type,
            initial_value,
        } => {
            let initial_node = initial_value
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?;

            let operation = CVMOperation::Unsafe;

            let allocation = FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::Alloca,
                _type: FMIRType::CMonad {
                    inner: Box::new(FMIRType::pure(thir_expr.get_type())),
                    operation: operation.clone(),
                },
            };

            let Some(initial_value) = initial_node else {
                return Ok(allocation);
            };

            let store = FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::Store {
                    pointer: FRc::new(allocation.clone()),
                    value: FRc::new(initial_value.clone()),
                },
                _type: monad_unit(operation),
            };

            Ok(then_node(allocation, store))
        }

        THIRExpressionKind::BindRegion {
            name,
            _type,
            initial_region,
            ..
        } => {
            let initial_node = convert_expression(env, initial_region)?;
            let location = MemoryLocation::Stack {
                name: name.as_string(),
                depth: env.query_variable(name).map(|v| v.depth + 1).unwrap_or(0),
            };
            let known_value = if initial_node._type.get_operation().is_none() {
                Some(initial_node.clone())
            } else {
                None
            };

            env.insert_variable(
                name.clone(),
                FMIRType::pure(_type.clone()),
                location.clone(),
                known_value,
            );

            let bind_effect = FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::Unit,
                _type: monad_unit(CVMOperation::Access {
                    reads: vec![],
                    writes: vec![location],
                }),
            };

            Ok(then_node(initial_node, bind_effect))
        }

        THIRExpressionKind::If {
            condition,
            then_branch,
            else_branch,
        } => {
            let condition_node = convert_expression(env, condition)?;
            let then_node_value = convert_expression(env, then_branch)?;
            let else_node_value = else_branch
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?
                .unwrap_or_else(FMIRNode::unit);

            let effect = condition_node
                ._type
                .union(&then_node_value._type)
                .union(&else_node_value._type);

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(then_node_value._type.inner_type().clone()),
                body: FMIRNodeBody::If {
                    condition: FRc::new(condition_node),
                    then_branch: FRc::new(then_node_value),
                    else_branch: FRc::new(else_node_value),
                },
            })
        }

        THIRExpressionKind::Match {
            condition,
            arms,
            default,
            ..
        } => {
            let condition_node = convert_expression(env, condition)?;
            let default_node = default
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?
                .unwrap_or(FMIRNode::unit());

            let mut effect = condition_node._type.union(&default_node._type);
            let mut arm_nodes = Vec::new();

            for (_, arm_expr) in arms {
                let arm_node = convert_expression(env, arm_expr)?;
                effect = effect.union(&arm_node._type);
                arm_nodes.push((FRc::new(FMIRNode::unit()), FRc::new(arm_node)));
            }

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(default_node._type.inner_type().clone()),
                body: FMIRNodeBody::Match {
                    condition: FRc::new(condition_node),
                    arms: arm_nodes,
                    default: FRc::new(default_node),
                },
            })
        }

        THIRExpressionKind::While {
            pre_eval,
            condition,
            body,
        } => {
            let condition_node = convert_expression(env, condition)?;
            let body_node = convert_expression(env, body)?;

            let loop_effect = condition_node._type.union(&body_node._type);
            let loop_node = FMIRNode {
                token_range: TokenRange::internal(),
                _type: loop_effect.clone().apply(FMIRType::pure(THIRType::unit())),
                body: FMIRNodeBody::CLoop {
                    condition: FRc::new(condition_node.clone()),
                    body: FRc::new(body_node),
                },
            };

            if *pre_eval {
                Ok(then_node(condition_node, loop_node))
            } else {
                Ok(loop_node)
            }
        }

        THIRExpressionKind::For {
            init,
            condition,
            increment,
            body,
        } => {
            let init_node = convert_expression(env, init)?;
            let condition_node = convert_expression(env, condition)?;
            let increment_node = convert_expression(env, increment)?;
            let body_node = convert_expression(env, body)?;

            let loop_body = then_node(body_node, increment_node);
            let loop_node = FMIRNode {
                token_range: TokenRange::internal(),
                _type: condition_node
                    ._type
                    .union(&loop_body._type)
                    .apply(FMIRType::pure(THIRType::unit())),
                body: FMIRNodeBody::CLoop {
                    condition: FRc::new(condition_node),
                    body: FRc::new(loop_body),
                },
            };

            Ok(then_node(init_node, loop_node))
        }

        THIRExpressionKind::Return {
            value,
            postcondition,
            cleanups: _,
        } => {
            let return_value = value
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?
                .unwrap_or_else(FMIRNode::unit);

            let return_node = FMIRNode {
                token_range: TokenRange::internal(),
                _type: FMIRType::unsafe_effect(FMIRType::pure(THIRType::unit())),
                body: FMIRNodeBody::CReturn {
                    value: FRc::new(return_value.clone()),
                },
            };

            env.push_scope();
            let postcondition_node = postcondition
                .as_ref()
                .map(|postcondition| {
                    if let Some(binding) = &postcondition.binding {
                        env.insert_variable(
                            binding.clone(),
                            return_value._type.clone(),
                            MemoryLocation::Stack {
                                name: binding.as_string(),
                                depth: env.current_depth(),
                            },
                            Some(return_value.clone()),
                        );
                    }

                    convert_expression(env, &postcondition.condition)
                })
                .transpose()?
                .unwrap_or(FMIRNode::unit());
            env.pop_scope();

            let assert_node = FMIRNode {
                token_range: thir_expr.token_range.clone(),
                _type: FMIRType::unsafe_effect(FMIRType::pure(THIRType::unit())),
                body: FMIRNodeBody::CompilerAssert {
                    condition: FRc::new(postcondition_node),
                    message: "postcondition failed".to_string(),
                },
            };

            Ok(then_node(assert_node, return_node))
        }

        THIRExpressionKind::Variable { name, .. }
        | THIRExpressionKind::ContractVariable { name, .. } => {
            // TODO: Force param

            if !thir_expr._type.is_memory_reference()
                && let Some(known) = env.query_known_value(name)
            {
                return Ok(known);
            }

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::VariableAlias {
                    name: name.as_string(),
                },
                _type: FMIRType::pure(thir_expr.get_type()),
            })
        }

        THIRExpressionKind::FunctionReference { name, debug_name } => {
            let THIRTypeKind::Function { signature } = &thir_expr._type.kind else {
                unreachable!(
                    "FMIR conversion expected function type in function reference expression"
                )
            };

            let display_name = debug_name.as_ref().unwrap_or(name);

            if !signature.contract.safe {
                return env.log_error(
                    thir_expr,
                    format!(
                        "References to unsafe function `{}` may not be used in safe contexts",
                        display_name
                    ),
                );
            }

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                body: FMIRNodeBody::VariableAlias {
                    name: name.as_string(),
                },
                _type: FMIRType::pure(thir_expr.get_type()),
            })
        }

        THIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            let lhs_node = convert_expression(env, lhs)?;
            let rhs_node = convert_expression(env, rhs)?;
            let result = app2(
                FMIRIntrinsicKind::Binary(binary_op_intrinsic(op)),
                lhs_node.clone(),
                rhs_node.clone(),
                &thir_expr._type,
            );
            let effect = lhs_node._type.union(&rhs_node._type);

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(FMIRType::pure(thir_expr.get_type())),
                body: result.body,
            })
        }

        THIRExpressionKind::UnaryOperation { operand, op } => {
            match op {
                THIRUnOp::PreIncrement(amount) => {
                    return Ok(with_expression_range(
                        convert_increment(env, operand, *amount, true)?,
                        thir_expr,
                    ));
                }
                THIRUnOp::PostIncrement(amount) => {
                    return Ok(with_expression_range(
                        convert_increment(env, operand, *amount, false)?,
                        thir_expr,
                    ));
                }
                _ => {}
            }

            let operand_node = convert_expression(env, operand)?;
            let intrinsic = unary_op_intrinsic(op);
            let result = app1(
                FMIRIntrinsicKind::Unary(intrinsic),
                operand_node.clone(),
                &thir_expr._type,
            );
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(FMIRType::pure(thir_expr.get_type())),
                body: result.body,
            })
        }

        THIRExpressionKind::TaggedUnionGet {
            value,
            variant_type,
        } => {
            // FIXME: THIR should output invariant assertions including tagged union accesses, so we are currently flakily
            // assuming that accesses are valid here.

            let value_node = convert_expression(env, value)?;
            let variant_ref_type = thir_expr.get_type();

            Ok(FMIRNode {
                _type: FMIRType::pure(variant_ref_type),
                body: FMIRNodeBody::Transmute {
                    value: FRc::new(value_node),
                    target_type: FMIRType::pure(variant_type.clone()),
                },
                token_range: thir_expr.token_range.clone(),
            })
        }

        THIRExpressionKind::StructInitializer {
            initializations,
            struct_type,
        } => {
            let fields = initializations
                .iter()
                .map(|init| {
                    let field_node = convert_expression(env, &init.value)?;
                    Ok((init.field_index, FRc::new(field_node)))
                })
                .collect::<CXResult<Vec<_>>>()?;

            Ok(FMIRNode {
                token_range: thir_expr.token_range.clone(),
                _type: FMIRType::pure(struct_type.clone()),
                body: FMIRNodeBody::AggregateInitialization { fields },
            })
        }

        THIRExpressionKind::RegionWrite { target, value } => {
            let target_node = convert_expression(env, target)?;
            let value_node = convert_expression(env, value)?;
            if let Some(name) = source_variable_name(target) {
                if value_node._type.get_operation().is_none() {
                    env.set_known_value(name, Some(value_node.clone()));
                } else {
                    env.set_known_value(name, None);
                }
            }

            Ok(store_node(
                target_node,
                value_node,
                write_operation_for_expr(env, target),
            ))
        }

        THIRExpressionKind::MemberAccess { base, .. } => {
            let base_node = convert_expression(env, base)?;

            Ok(FMIRNode {
                token_range: thir_expr.token_range.clone(),
                _type: base_node
                    ._type
                    .identity()
                    .apply(FMIRType::pure(thir_expr.get_type())),
                body: FMIRNodeBody::Transmute {
                    value: FRc::new(base_node),
                    target_type: FMIRType::pure(thir_expr.get_type()),
                },
            })
        }

        THIRExpressionKind::RegionMove { source } => convert_expression(env, source),

        THIRExpressionKind::Typechange(inner) => {
            if inner._type.is_pointer() {
                return env.log_error(
                    thir_expr,
                    "Dereferencing raw pointers is not allowed in safe contexts".to_string(),
                );
            }

            convert_expression(env, inner)
        }

        THIRExpressionKind::CallFunction {
            function,
            arguments,
            contract: _,
        } => {
            let function_node = convert_expression(env, function)?;
            let argument_nodes = arguments
                .iter()
                .map(|argument| convert_expression(env, argument))
                .collect::<CXResult<Vec<_>>>()?;

            let mut application = function_node.clone();
            let mut effect = function_node._type.identity();

            for argument in argument_nodes {
                effect = effect.union(&argument._type);
                application = FMIRNode {
                    token_range: TokenRange::internal(),
                    _type: FMIRType::pure(THIRType::internal_function()),
                    body: FMIRNodeBody::Application {
                        function: FRc::new(application),
                        argument: FRc::new(argument),
                    },
                };
            }

            effect = effect.union(&FMIRType::unsafe_effect(FMIRType::pure(
                thir_expr.get_type(),
            )));

            // TODO: Contract enforcement

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(FMIRType::pure(thir_expr.get_type())),
                body: application.body,
            })
        }

        THIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            let operand_node = convert_expression(env, operand)?;
            let converted = app1(
                FMIRIntrinsicKind::Cast(coercion_intrinsic(env, operand, conversion)?),
                operand_node.clone(),
                &thir_expr._type,
            );
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                token_range: TokenRange::internal(),
                _type: effect.apply(FMIRType::pure(thir_expr.get_type())),
                body: converted.body,
            })
        }

        THIRExpressionKind::Break { .. } | THIRExpressionKind::Continue { .. } => {
            unsupported_expression_error(env, thir_expr)
        }

        THIRExpressionKind::LeakLifetime { .. } => Ok(FMIRNode {
            token_range: thir_expr.token_range.clone(),
            _type: FMIRType::unsafe_effect(FMIRType::pure(THIRType::unit())),
            body: FMIRNodeBody::UnsafeBlock,
        }),

        THIRExpressionKind::Unsafe { .. } => Ok(FMIRNode {
            token_range: thir_expr.token_range.clone(),
            _type: FMIRType::unsafe_effect(FMIRType::pure(thir_expr.get_type())),
            body: FMIRNodeBody::UnsafeBlock,
        }),

        _ => unsupported_expression_error(env, thir_expr),
    }?;

    Ok(with_expression_range(node, thir_expr))
}
