use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp,
        MIRPtrBinOp, MIRPtrDiffBinOp, MIRUnOp,
    },
    types::{MIRType, MIRTypeKind},
};
use cx_safe_ir::ast::{CVMOperation, FMIRNode, FMIRNodeBody, FMIRType, FRc, MemoryLocation};
use cx_util::{identifier::CXIdent, CXError, CXResult};

use crate::mir_conversion::environment::FMIREnvironment;

fn monad_unit(operation: CVMOperation) -> FMIRType {
    FMIRType::CMonad {
        inner: Box::new(FMIRType::pure(MIRType::unit())),
        operation,
    }
}

fn pointer_alias(name: &CXIdent, pointer_type: MIRType) -> FMIRNode {
    FMIRNode {
        body: FMIRNodeBody::VariableAlias {
            name: name.as_string(),
        },
        _type: FMIRType::pure(pointer_type),
    }
}

fn operator_alias(name: String) -> FMIRNode {
    FMIRNode {
        body: FMIRNodeBody::VariableAlias { name },
        _type: FMIRType::pure(MIRType::internal_function()),
    }
}

fn then_node(first: FMIRNode, second: FMIRNode) -> FMIRNode {
    let combined = first._type.union(&second._type);
    FMIRNode {
        _type: combined.apply(second._type.inner_type().clone()),
        body: FMIRNodeBody::Then {
            first: FRc::new(first),
            second: FRc::new(second),
        },
    }
}

fn chain_statements(statements: Vec<FMIRNode>) -> FMIRNode {
    let mut iter = statements.into_iter();
    let Some(first) = iter.next() else {
        return FMIRNode::unit();
    };

    iter.fold(first, then_node)
}

fn unary_op_name(op: &MIRUnOp) -> String {
    match op {
        MIRUnOp::NEG => "__op_neg".to_string(),
        MIRUnOp::INEG => "__op_ineg".to_string(),
        MIRUnOp::FNEG => "__op_fneg".to_string(),
        MIRUnOp::BNOT => "__op_bnot".to_string(),
        MIRUnOp::LNOT => "__op_lnot".to_string(),
        MIRUnOp::PreIncrement(amount) => format!("__op_preinc_{}", amount),
        MIRUnOp::PostIncrement(amount) => format!("__op_postinc_{}", amount),
    }
}

fn int_binop_name(op: &MIRIntegerBinOp) -> &'static str {
    match op {
        MIRIntegerBinOp::ADD => "add",
        MIRIntegerBinOp::SUB => "sub",
        MIRIntegerBinOp::MUL => "mul",
        MIRIntegerBinOp::DIV => "div",
        MIRIntegerBinOp::MOD => "mod",
        MIRIntegerBinOp::IMUL => "imul",
        MIRIntegerBinOp::IDIV => "idiv",
        MIRIntegerBinOp::IMOD => "imod",
        MIRIntegerBinOp::EQ => "eq",
        MIRIntegerBinOp::NE => "ne",
        MIRIntegerBinOp::LT => "lt",
        MIRIntegerBinOp::LE => "le",
        MIRIntegerBinOp::GT => "gt",
        MIRIntegerBinOp::GE => "ge",
        MIRIntegerBinOp::ILT => "ilt",
        MIRIntegerBinOp::ILE => "ile",
        MIRIntegerBinOp::IGT => "igt",
        MIRIntegerBinOp::IGE => "ige",
        MIRIntegerBinOp::LAND => "land",
        MIRIntegerBinOp::LOR => "lor",
        MIRIntegerBinOp::BAND => "band",
        MIRIntegerBinOp::BOR => "bor",
        MIRIntegerBinOp::BXOR => "bxor",
    }
}

fn float_binop_name(op: &MIRFloatBinOp) -> &'static str {
    match op {
        MIRFloatBinOp::FADD => "add",
        MIRFloatBinOp::FSUB => "sub",
        MIRFloatBinOp::FMUL => "mul",
        MIRFloatBinOp::FDIV => "div",
        MIRFloatBinOp::EQ => "eq",
        MIRFloatBinOp::NEQ => "ne",
        MIRFloatBinOp::FLT => "lt",
        MIRFloatBinOp::FLE => "le",
        MIRFloatBinOp::FGT => "gt",
        MIRFloatBinOp::FGE => "ge",
    }
}

fn ptrdiff_binop_name(op: &MIRPtrDiffBinOp) -> &'static str {
    match op {
        MIRPtrDiffBinOp::ADD => "add",
        MIRPtrDiffBinOp::SUB => "sub",
    }
}

fn ptr_binop_name(op: &MIRPtrBinOp) -> &'static str {
    match op {
        MIRPtrBinOp::EQ => "eq",
        MIRPtrBinOp::NE => "ne",
        MIRPtrBinOp::LT => "lt",
        MIRPtrBinOp::GT => "gt",
        MIRPtrBinOp::LE => "le",
        MIRPtrBinOp::GE => "ge",
    }
}

fn binary_op_name(op: &MIRBinOp) -> String {
    match op {
        MIRBinOp::Integer { itype, op } => {
            format!("__op_i{}_{}", itype.bytes() * 8, int_binop_name(op))
        }
        MIRBinOp::Float { ftype, op } => {
            format!("__op_f{}_{}", ftype.bytes() * 8, float_binop_name(op))
        }
        MIRBinOp::PtrDiff { op, .. } => {
            format!("__op_ptrdiff_{}", ptrdiff_binop_name(op))
        }
        MIRBinOp::Pointer { op } => {
            format!("__op_ptr_{}", ptr_binop_name(op))
        }
    }
}

fn coercion_name(coercion: &MIRCoercion) -> String {
    match coercion {
        MIRCoercion::Integral { sextend, to_type } => {
            format!(
                "__cast_integral_i{}_sx{}",
                to_type.bytes() * 8,
                if *sextend { 1 } else { 0 }
            )
        }
        MIRCoercion::FloatCast { to_type } => {
            format!("__cast_float_f{}", to_type.bytes() * 8)
        }
        MIRCoercion::PtrToInt { to_type } => {
            format!("__cast_ptr_to_i{}", to_type.bytes() * 8)
        }
        MIRCoercion::IntToPtr { sextend } => {
            format!("__cast_i_to_ptr_sx{}", if *sextend { 1 } else { 0 })
        }
        MIRCoercion::IntToFloat { to_type, sextend } => {
            format!(
                "__cast_i_to_f{}_sx{}",
                to_type.bytes() * 8,
                if *sextend { 1 } else { 0 }
            )
        }
        MIRCoercion::FloatToInt { to_type, sextend } => {
            format!(
                "__cast_f_to_i{}_sx{}",
                to_type.bytes() * 8,
                if *sextend { 1 } else { 0 }
            )
        }
        MIRCoercion::IntToBool => "__cast_i_to_bool".to_string(),
        MIRCoercion::ReinterpretBits => "__cast_reinterpret_bits".to_string(),
    }
}

fn app1(op_name: String, arg: FMIRNode, output_type: &MIRType) -> FMIRNode {
    FMIRNode {
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(operator_alias(op_name)),
            argument: FRc::new(arg),
        },
    }
}

fn app2(op_name: String, lhs: FMIRNode, rhs: FMIRNode, output_type: &MIRType) -> FMIRNode {
    FMIRNode {
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(FMIRNode {
                _type: FMIRType::pure(MIRType::internal_function()),
                body: FMIRNodeBody::Application {
                    function: FRc::new(operator_alias(op_name)),
                    argument: FRc::new(lhs),
                },
            }),
            argument: FRc::new(rhs),
        },
    }
}

fn unsupported_expression_error(env: &FMIREnvironment, expr: &MIRExpression) -> CXResult<FMIRNode> {
    CXError::create_result(format!(
        "FMIR conversion does not currently support expression '{}' in function '{}'",
        expr,
        env.current_mir_prototype().name,
    ))
}

pub fn convert_expression(env: &mut FMIREnvironment, mir_expr: &MIRExpression) -> CXResult<FMIRNode> {
    match &mir_expr.kind {
        MIRExpressionKind::BoolLiteral(value) => Ok(FMIRNode {
            body: FMIRNodeBody::BooleanLiteral(*value),
            _type: FMIRType::pure(MIRType::bool()),
        }),

        MIRExpressionKind::IntLiteral(value, itype, signed) => Ok(FMIRNode {
            body: FMIRNodeBody::IntegerLiteral(*value),
            _type: FMIRType::Pure {
                mir_type: MIRType::from(MIRTypeKind::Integer {
                    _type: *itype,
                    signed: *signed,
                }),
            },
        }),

        MIRExpressionKind::FloatLiteral(value, ftype) => Ok(FMIRNode {
            body: FMIRNodeBody::FloatLiteral(value.into()),
            _type: FMIRType::Pure {
                mir_type: MIRTypeKind::Float { _type: *ftype }.into(),
            },
        }),

        MIRExpressionKind::Null => Ok(FMIRNode {
            body: FMIRNodeBody::IntegerLiteral(0),
            _type: FMIRType::pure(mir_expr._type.clone()),
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
                    _type: return_value._type.inner_type().clone(),
                    body: return_value.body.clone(),
                }),
            );
            let postcondition_node = convert_expression(env, postcondition_expr.as_ref())?;
            env.pop_scope();

            let assert_node = FMIRNode {
                _type: FMIRType::unsafe_effect(FMIRType::pure(MIRType::unit())),
                body: FMIRNodeBody::CompilerAssert {
                    condition: FRc::new(postcondition_node),
                    message: "postcondition failed".to_string(),
                },
            };

            Ok(then_node(assert_node, return_node))
        }

        MIRExpressionKind::Variable(name) | MIRExpressionKind::ContractVariable { name, .. } => {
            if !mir_expr._type.is_memory_reference() {
                if let Some(known) = env.query_known_value(name) {
                    return Ok(known);
                }
            }

            Ok(FMIRNode {
                body: FMIRNodeBody::VariableAlias {
                    name: name.as_string(),
                },
                _type: FMIRType::pure(mir_expr._type.clone()),
            })
        }

        MIRExpressionKind::FunctionReference { .. } => {
            let Some(function_name) = mir_expr._type.get_fn_name() else {
                return CXError::create_result(format!(
                    "FMIR conversion expected function reference type in function '{}'",
                    env.current_mir_prototype().name
                ));
            };

            Ok(FMIRNode {
                body: FMIRNodeBody::VariableAlias {
                    name: function_name.as_string(),
                },
                _type: FMIRType::pure(mir_expr._type.clone()),
            })
        }

        MIRExpressionKind::BinaryOperation { lhs, rhs, op } => {
            let lhs_node = convert_expression(env, lhs)?;
            let rhs_node = convert_expression(env, rhs)?;
            let result = app2(binary_op_name(op), lhs_node.clone(), rhs_node.clone(), &mir_expr._type);
            let effect = lhs_node._type.union(&rhs_node._type);

            Ok(FMIRNode {
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: result.body,
            })
        }

        MIRExpressionKind::UnaryOperation { operand, op } => {
            let operand_node = convert_expression(env, operand)?;
            let result = app1(unary_op_name(op), operand_node.clone(), &mir_expr._type);
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: result.body,
            })
        }

        MIRExpressionKind::MemoryRead { source } => {
            if let MIRExpressionKind::Variable(name)
            | MIRExpressionKind::ContractVariable { name, .. } = &source.kind
            {
                if let Some(mut known) = env.query_known_value(name) {
                    if let Some(location) = env.query_memory_location(name) {
                        let access = FMIRType::access(
                            known._type.inner_type().clone(),
                            vec![location],
                            vec![],
                        );
                        known._type = known._type.union(&access).apply(known._type.inner_type().clone());
                    }

                    return Ok(known);
                }
            }

            let source_node = convert_expression(env, source)?;

            let operation = if let MIRExpressionKind::Variable(name)
            | MIRExpressionKind::ContractVariable { name, .. } = &source.kind
            {
                env.query_memory_location(name)
                    .map(|location| CVMOperation::Access {
                        reads: vec![location],
                        writes: vec![],
                    })
                    .unwrap_or(CVMOperation::Unsafe)
            } else {
                CVMOperation::Unsafe
            };

            let read_effect = FMIRType::CMonad {
                inner: Box::new(FMIRType::pure(mir_expr._type.clone())),
                operation,
            };
            let combined = source_node
                ._type
                .union(&read_effect)
                .apply(FMIRType::pure(mir_expr._type.clone()));

            Ok(FMIRNode {
                _type: combined,
                body: FMIRNodeBody::Load {
                    pointer: FRc::new(source_node),
                },
            })
        }

        MIRExpressionKind::MemoryWrite { target, value } => {
            let target_node = convert_expression(env, target)?;
            let value_node = convert_expression(env, value)?;

            let operation = if let MIRExpressionKind::Variable(name)
            | MIRExpressionKind::ContractVariable { name, .. } = &target.kind
            {
                let location = env.query_memory_location(name);
                if value_node._type.get_operation().is_none() {
                    env.set_known_value(name, Some(value_node.clone()));
                } else {
                    env.set_known_value(name, None);
                }

                location
                    .map(|location| CVMOperation::Access {
                        reads: vec![],
                        writes: vec![location],
                    })
                    .unwrap_or(CVMOperation::Unsafe)
            } else {
                CVMOperation::Unsafe
            };

            let write_effect = monad_unit(operation);
            let combined = target_node
                ._type
                .union(&value_node._type)
                .union(&write_effect)
                .apply(FMIRType::pure(MIRType::unit()));

            Ok(FMIRNode {
                _type: combined,
                body: FMIRNodeBody::Store {
                    pointer: FRc::new(target_node),
                    value: FRc::new(value_node),
                },
            })
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
                    _type: FMIRType::pure(MIRType::internal_function()),
                    body: FMIRNodeBody::Application {
                        function: FRc::new(application),
                        argument: FRc::new(argument),
                    },
                };
            }

            effect = effect.union(&FMIRType::unsafe_effect(FMIRType::pure(mir_expr._type.clone())));

            Ok(FMIRNode {
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: application.body,
            })
        }

        MIRExpressionKind::TypeConversion {
            operand,
            conversion,
        } => {
            let operand_node = convert_expression(env, operand)?;
            let converted = app1(coercion_name(conversion), operand_node.clone(), &mir_expr._type);
            let effect = operand_node._type.identity();

            Ok(FMIRNode {
                _type: effect.apply(FMIRType::pure(mir_expr._type.clone())),
                body: converted.body,
            })
        }

        MIRExpressionKind::Break { .. } | MIRExpressionKind::Continue { .. } => {
            unsupported_expression_error(env, mir_expr)
        }

        _ => unsupported_expression_error(env, mir_expr),
    }
}
