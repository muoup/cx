use cx_mir::mir::{
    expression::{
        MIRExpression, MIRExpressionKind, MIRUnOp,
    },
    types::{MIRType, MIRTypeKind},
};
use cx_safe_ir::{ast::{
    CVMOperation, FMIRNode, FMIRNodeBody, FMIRSourceRange, FMIRType, FRc, MemoryLocation,
}, intrinsic::FMIRIntrinsicKind};
use crate::{log_analysis_error, mir_conversion::factories::*};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use crate::mir_conversion::environment::FMIREnvironment;

pub fn convert_expression(
    env: &mut FMIREnvironment,
    mir_expr: &MIRExpression,
) -> CXResult<FMIRNode> {
    let node = match &mir_expr.kind {
        MIRExpressionKind::BoolLiteral(value) => Ok(FMIRNode {
            source_range: None,
            body: FMIRNodeBody::BooleanLiteral(*value),
            _type: FMIRType::Pure {
                mir_type: MIRType::bool(),
            },
        }),

        MIRExpressionKind::IntLiteral(value, itype, signed) => Ok(FMIRNode {
            source_range: None,
            body: FMIRNodeBody::IntegerLiteral(*value),
            _type: FMIRType::Pure {
                mir_type: MIRType::from(MIRTypeKind::Integer {
                    _type: *itype,
                    signed: *signed,
                }),
            },
        }),

        MIRExpressionKind::FloatLiteral(value, ftype) => Ok(FMIRNode {
            source_range: None,
            body: FMIRNodeBody::FloatLiteral(value.into()),
            _type: FMIRType::Pure {
                mir_type: MIRTypeKind::Float { _type: *ftype }.into(),
            },
        }),
        MIRExpressionKind::Unit => Ok(FMIRNode::unit()),

        MIRExpressionKind::Block { statements } => {
            env.push_scope();
            let nodes = statements
                .iter()
                .map(|statement| convert_expression(env, statement))
                .collect::<CXResult<Vec<_>>>()?;
            env.pop_scope();

            Ok(chain_statements(nodes))
        }

        MIRExpressionKind::CreateStackVariable {
            name,
            _type,
            initial_value,
        } => {
            let initial_node = initial_value
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?;
            
            if _type.is_pointer() {
                return log_analysis_error!(
                    env,
                    mir_expr,
                    "Pointer types may not be used in safe contexts"
                )
            }

            let mut operation = CVMOperation::Unsafe;
            if let Some(name) = name {
                let location = MemoryLocation::Stack {
                    name: name.as_string(),
                    depth: env.query_variable(name).map(|v| v.depth + 1).unwrap_or(0),
                };

                let known_value = initial_node.as_ref().and_then(|node| {
                    if node._type.get_operation().is_none() {
                        Some(node.clone())
                    } else {
                        None
                    }
                });

                env.insert_variable(
                    name.clone(),
                    FMIRType::pure(_type.clone()),
                    location.clone(),
                    known_value,
                );
                operation = CVMOperation::Access {
                    reads: vec![],
                    writes: vec![location],
                };
            }

            let allocation = FMIRNode {
                source_range: None,
                body: FMIRNodeBody::Alloca,
                _type: FMIRType::CMonad {
                    inner: Box::new(FMIRType::pure(mir_expr._type.clone())),
                    operation: operation.clone(),
                },
            };

            let Some(initial_value) = initial_node else {
                return Ok(allocation);
            };

            let pointer = if let Some(name) = name {
                pointer_alias(name, mir_expr._type.clone())
            } else {
                allocation.clone()
            };

            let store = FMIRNode {
                source_range: None,
                body: FMIRNodeBody::Store {
                    pointer: FRc::new(pointer),
                    value: FRc::new(initial_value.clone()),
                },
                _type: monad_unit(operation),
            };

            Ok(then_node(allocation, store))
        }

        MIRExpressionKind::If {
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
                source_range: None,
                _type: effect.apply(then_node_value._type.inner_type().clone()),
                body: FMIRNodeBody::If {
                    condition: FRc::new(condition_node),
                    then_branch: FRc::new(then_node_value),
                    else_branch: FRc::new(else_node_value),
                },
            })
        }

        MIRExpressionKind::While {
            pre_eval,
            condition,
            body,
        } => {
            let condition_node = convert_expression(env, condition)?;
            let body_node = convert_expression(env, body)?;

            let loop_effect = condition_node._type.union(&body_node._type);
            let loop_node = FMIRNode {
                source_range: None,
                _type: loop_effect.clone().apply(FMIRType::pure(MIRType::unit())),
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

        MIRExpressionKind::For {
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
                source_range: None,
                _type: condition_node
                    ._type
                    .union(&loop_body._type)
                    .apply(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CLoop {
                    condition: FRc::new(condition_node),
                    body: FRc::new(loop_body),
                },
            };

            Ok(then_node(init_node, loop_node))
        }

        MIRExpressionKind::Return { value } => {
            let return_value = value
                .as_ref()
                .map(|expr| convert_expression(env, expr))
                .transpose()?
                .unwrap_or_else(FMIRNode::unit);

            let return_node = FMIRNode {
                source_range: None,
                _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CReturn {
                    value: FRc::new(return_value.clone()),
                },
            };

            let postcondition = env.current_mir_prototype().contract.postcondition.clone();
            let Some((ret_name, postcondition_expr)) = postcondition else {
                return Ok(return_node);
            };

            env.push_scope();
            let ret_binding = ret_name.unwrap_or_else(|| CXIdent::from("ret"));
            env.insert_variable(
                ret_binding.clone(),
                return_value._type.inner_type().clone(),
                MemoryLocation::Stack {
                    name: ret_binding.as_string(),
                    depth: 0,
                },
                Some(FMIRNode {
                    source_range: None,
                    _type: return_value._type.inner_type().clone(),
                    body: return_value.body.clone(),
                }),
            );
            let postcondition_node = convert_expression(env, postcondition_expr.as_ref())?;
            env.pop_scope();

            let assert_node = FMIRNode {
                source_range: mir_expr.source_range.as_ref().map(FMIRSourceRange::from),
                _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CompilerAssert {
                    condition: FRc::new(postcondition_node),
                    message: format!("postcondition failed:{}", ret_binding),
                },
            };

            Ok(then_node(assert_node, return_node))
        }

        MIRExpressionKind::Variable(name) | MIRExpressionKind::ContractVariable { name, .. } => {
            if !mir_expr._type.is_memory_reference()
                && let Some(known) = env.query_known_value(name)
            {
                return Ok(known);
            }

            Ok(FMIRNode {
                source_range: None,
                body: FMIRNodeBody::VariableAlias {
                    name: name.as_string(),
                },
                _type: FMIRType::pure(mir_expr._type.clone()),
            })
        }

        MIRExpressionKind::FunctionReference { .. } => {
            let MIRTypeKind::Function { prototype } = &mir_expr._type.kind else {
                unreachable!("FMIR conversion expected function type in function reference expression")
            };
            
            if !prototype.contract.safe {
                return log_analysis_error!(
                    env,
                    mir_expr,
                    "References to unsafe functions may not be used in safe contexts"
                );
            }
            
            let Some(function_name) = mir_expr._type.get_fn_name() else {
                return CXError::create_result(format!(
                    "FMIR conversion expected function reference type in function '{}'",
                    env.current_mir_prototype().name
                ));
            };

            Ok(FMIRNode {
                source_range: None,
                body: FMIRNodeBody::VariableAlias {
                    name: function_name.as_string(),
                },
                _type: FMIRType::pure(mir_expr._type.clone()),
            })
        }

        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            let lhs_node = convert_expression(env, lhs)?;
            let rhs_node = convert_expression(env, rhs)?;
            let result = app2(
                FMIRIntrinsicKind::Binary(binary_op_intrinsic(op)),
                lhs_node.clone(),
                rhs_node.clone(),
                &mir_expr._type,
            );
            let effect = lhs_node._type.union(&rhs_node._type);

            Ok(FMIRNode {
                source_range: None,
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: result.body,
            })
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            match op {
                MIRUnOp::PreIncrement(amount) => return Ok(with_expression_range(
                    convert_increment(env, operand, *amount, true)?,
                    mir_expr,
                )),
                MIRUnOp::PostIncrement(amount) => return Ok(with_expression_range(
                    convert_increment(env, operand, *amount, false)?,
                    mir_expr,
                )),
                _ => {}
            }

            let operand_node = convert_expression(env, operand)?;
            let intrinsic = unary_op_intrinsic(op);
            let result = app1(
                FMIRIntrinsicKind::Unary(intrinsic),
                operand_node.clone(),
                &mir_expr._type,
            );
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                source_range: None,
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: result.body,
            })
        }

        MIRExpressionKind::MemoryRead { source } => {
            if let MIRExpressionKind::Variable(name)
            | MIRExpressionKind::ContractVariable { name, .. } = &source.kind
                && let Some(mut known) = env.query_known_value(name)
            {
                if let Some(location) = env.query_memory_location(name) {
                    let access =
                        FMIRType::access(known._type.inner_type().clone(), vec![location], vec![]);
                    known._type = known
                        ._type
                        .union(&access)
                        .apply(known._type.inner_type().clone());
                }

                return Ok(known);
            }

            let source_node = convert_expression(env, source)?;
            Ok(load_node(
                source_node,
                &mir_expr._type,
                read_operation_for_expr(env, source),
            ))
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
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

        MIRExpressionKind::Typechange(inner) => convert_expression(env, inner),

        MIRExpressionKind::CallFunction {
            function,
            arguments,
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
                    source_range: None,
                    _type: FMIRType::pure(MIRType::internal_function()),
                    body: FMIRNodeBody::Application {
                        function: FRc::new(application),
                        argument: FRc::new(argument),
                    },
                };
            }

            effect = effect.union(&FMIRType::unsafe_effect(FMIRType::pure(
                mir_expr._type.clone(),
            )));

            Ok(FMIRNode {
                source_range: None,
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: application.body,
            })
        }

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            let operand_node = convert_expression(env, operand)?;
            let converted = app1(
                FMIRIntrinsicKind::Cast(coercion_intrinsic(conversion)),
                operand_node.clone(),
                &mir_expr._type,
            );
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                source_range: None,
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: converted.body,
            })
        }

        MIRExpressionKind::Break { .. } | MIRExpressionKind::Continue { .. } => {
            unsupported_expression_error(env, mir_expr)
        }

        MIRExpressionKind::LeakLifetime { .. } => Ok(FMIRNode {
            source_range: mir_expr.source_range.as_ref().map(FMIRSourceRange::from),
            _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
            body: FMIRNodeBody::UnsafeBlock,
        }),

        MIRExpressionKind::Unsafe { .. } => Ok(FMIRNode {
            source_range: mir_expr.source_range.as_ref().map(FMIRSourceRange::from),
            _type: FMIRType::unsafe_effect(FMIRType::pure(mir_expr._type.clone())),
            body: FMIRNodeBody::UnsafeBlock,
        }),

        _ => unsupported_expression_error(env, mir_expr),
    }?;

    Ok(with_expression_range(node, mir_expr))
}
