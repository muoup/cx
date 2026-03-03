use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp,
        MIRPtrBinOp, MIRPtrDiffBinOp, MIRUnOp,
    },
    types::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_safe_ir::ast::{
    CVMOperation, FMIRBinaryIntrinsic, FMIRCastIntrinsic, FMIRFloatBinaryIntrinsicOp,
    FMIRIntegerBinaryIntrinsicOp, FMIRIntrinsicFunction, FMIRIntrinsicKind, FMIRNode,
    FMIRNodeBody, FMIRPointerBinaryIntrinsicOp, FMIRPointerDiffBinaryIntrinsicOp, FMIRSourceRange,
    FMIRType, FMIRUnaryIntrinsic, FRc, MemoryLocation,
};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use crate::mir_conversion::environment::FMIREnvironment;

fn monad_unit(operation: CVMOperation) -> FMIRType {
    FMIRType::CMonad {
        inner: Box::new(FMIRType::pure(MIRType::unit())),
        operation,
    }
}

fn pointer_alias(name: &CXIdent, pointer_type: MIRType) -> FMIRNode {
    FMIRNode {
        source_range: None,
        body: FMIRNodeBody::VariableAlias {
            name: name.as_string(),
        },
        _type: FMIRType::pure(pointer_type),
    }
}

fn intrinsic_alias(intrinsic: FMIRIntrinsicKind) -> FMIRNode {
    FMIRNode {
        source_range: None,
        body: FMIRNodeBody::IntrinsicFunction(FMIRIntrinsicFunction { kind: intrinsic }),
        _type: FMIRType::pure(MIRType::internal_function()),
    }
}

fn then_node(first: FMIRNode, second: FMIRNode) -> FMIRNode {
    let combined = first._type.union(&second._type);
    FMIRNode {
        source_range: None,
        _type: combined.apply(second._type.inner_type().clone()),
        body: FMIRNodeBody::Then {
            first: FRc::new(first),
            second: FRc::new(second),
        },
    }
}

fn bind_node(monad: FMIRNode, capture: CXIdent, function: FMIRNode) -> FMIRNode {
    let combined = monad._type.union(&function._type);
    FMIRNode {
        source_range: None,
        _type: combined.apply(function._type.inner_type().clone()),
        body: FMIRNodeBody::Bind {
            monad: FRc::new(monad),
            capture,
            function: FRc::new(function),
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

fn unary_op_intrinsic(op: &MIRUnOp) -> Option<FMIRUnaryIntrinsic> {
    Some(match op {
        MIRUnOp::NEG => FMIRUnaryIntrinsic::Neg,
        MIRUnOp::INEG => FMIRUnaryIntrinsic::INeg,
        MIRUnOp::FNEG => FMIRUnaryIntrinsic::FNeg,
        MIRUnOp::BNOT => FMIRUnaryIntrinsic::BNot,
        MIRUnOp::LNOT => FMIRUnaryIntrinsic::LNot,
        MIRUnOp::PreIncrement(_) | MIRUnOp::PostIncrement(_) => return None,
    })
}

fn int_binop_intrinsic(op: &MIRIntegerBinOp) -> FMIRIntegerBinaryIntrinsicOp {
    match op {
        MIRIntegerBinOp::ADD => FMIRIntegerBinaryIntrinsicOp::Add,
        MIRIntegerBinOp::SUB => FMIRIntegerBinaryIntrinsicOp::Sub,
        MIRIntegerBinOp::MUL => FMIRIntegerBinaryIntrinsicOp::Mul,
        MIRIntegerBinOp::DIV => FMIRIntegerBinaryIntrinsicOp::Div,
        MIRIntegerBinOp::MOD => FMIRIntegerBinaryIntrinsicOp::Mod,
        MIRIntegerBinOp::IMUL => FMIRIntegerBinaryIntrinsicOp::IMul,
        MIRIntegerBinOp::IDIV => FMIRIntegerBinaryIntrinsicOp::IDiv,
        MIRIntegerBinOp::IMOD => FMIRIntegerBinaryIntrinsicOp::IMod,
        MIRIntegerBinOp::EQ => FMIRIntegerBinaryIntrinsicOp::Eq,
        MIRIntegerBinOp::NE => FMIRIntegerBinaryIntrinsicOp::Ne,
        MIRIntegerBinOp::LT => FMIRIntegerBinaryIntrinsicOp::Lt,
        MIRIntegerBinOp::LE => FMIRIntegerBinaryIntrinsicOp::Le,
        MIRIntegerBinOp::GT => FMIRIntegerBinaryIntrinsicOp::Gt,
        MIRIntegerBinOp::GE => FMIRIntegerBinaryIntrinsicOp::Ge,
        MIRIntegerBinOp::ILT => FMIRIntegerBinaryIntrinsicOp::ILt,
        MIRIntegerBinOp::ILE => FMIRIntegerBinaryIntrinsicOp::ILe,
        MIRIntegerBinOp::IGT => FMIRIntegerBinaryIntrinsicOp::IGt,
        MIRIntegerBinOp::IGE => FMIRIntegerBinaryIntrinsicOp::IGe,
        MIRIntegerBinOp::LAND => FMIRIntegerBinaryIntrinsicOp::LAnd,
        MIRIntegerBinOp::LOR => FMIRIntegerBinaryIntrinsicOp::LOr,
        MIRIntegerBinOp::BAND => FMIRIntegerBinaryIntrinsicOp::BAnd,
        MIRIntegerBinOp::BOR => FMIRIntegerBinaryIntrinsicOp::BOr,
        MIRIntegerBinOp::BXOR => FMIRIntegerBinaryIntrinsicOp::BXor,
    }
}

fn float_binop_intrinsic(op: &MIRFloatBinOp) -> FMIRFloatBinaryIntrinsicOp {
    match op {
        MIRFloatBinOp::FADD => FMIRFloatBinaryIntrinsicOp::Add,
        MIRFloatBinOp::FSUB => FMIRFloatBinaryIntrinsicOp::Sub,
        MIRFloatBinOp::FMUL => FMIRFloatBinaryIntrinsicOp::Mul,
        MIRFloatBinOp::FDIV => FMIRFloatBinaryIntrinsicOp::Div,
        MIRFloatBinOp::EQ => FMIRFloatBinaryIntrinsicOp::Eq,
        MIRFloatBinOp::NEQ => FMIRFloatBinaryIntrinsicOp::Ne,
        MIRFloatBinOp::FLT => FMIRFloatBinaryIntrinsicOp::Lt,
        MIRFloatBinOp::FLE => FMIRFloatBinaryIntrinsicOp::Le,
        MIRFloatBinOp::FGT => FMIRFloatBinaryIntrinsicOp::Gt,
        MIRFloatBinOp::FGE => FMIRFloatBinaryIntrinsicOp::Ge,
    }
}

fn ptrdiff_binop_intrinsic(op: &MIRPtrDiffBinOp) -> FMIRPointerDiffBinaryIntrinsicOp {
    match op {
        MIRPtrDiffBinOp::ADD => FMIRPointerDiffBinaryIntrinsicOp::Add,
        MIRPtrDiffBinOp::SUB => FMIRPointerDiffBinaryIntrinsicOp::Sub,
    }
}

fn ptr_binop_intrinsic(op: &MIRPtrBinOp) -> FMIRPointerBinaryIntrinsicOp {
    match op {
        MIRPtrBinOp::EQ => FMIRPointerBinaryIntrinsicOp::Eq,
        MIRPtrBinOp::NE => FMIRPointerBinaryIntrinsicOp::Ne,
        MIRPtrBinOp::LT => FMIRPointerBinaryIntrinsicOp::Lt,
        MIRPtrBinOp::GT => FMIRPointerBinaryIntrinsicOp::Gt,
        MIRPtrBinOp::LE => FMIRPointerBinaryIntrinsicOp::Le,
        MIRPtrBinOp::GE => FMIRPointerBinaryIntrinsicOp::Ge,
    }
}

fn binary_op_intrinsic(op: &MIRBinOp) -> FMIRBinaryIntrinsic {
    match op {
        MIRBinOp::Integer { itype, op } => FMIRBinaryIntrinsic::Integer {
            bits: itype.bytes() * 8,
            op: int_binop_intrinsic(op),
        },
        MIRBinOp::Float { ftype, op } => FMIRBinaryIntrinsic::Float {
            bits: ftype.bytes() * 8,
            op: float_binop_intrinsic(op),
        },
        MIRBinOp::PtrDiff { op, .. } => FMIRBinaryIntrinsic::PointerDiff {
            op: ptrdiff_binop_intrinsic(op),
        },
        MIRBinOp::Pointer { op } => FMIRBinaryIntrinsic::Pointer {
            op: ptr_binop_intrinsic(op),
        },
    }
}

fn coercion_intrinsic(coercion: &MIRCoercion) -> FMIRCastIntrinsic {
    match coercion {
        MIRCoercion::Integral { sextend, to_type } => FMIRCastIntrinsic::Integral {
            sextend: *sextend,
            to_bits: to_type.bytes() * 8,
        },
        MIRCoercion::FloatCast { to_type } => FMIRCastIntrinsic::FloatCast {
            to_bits: to_type.bytes() * 8,
        },
        MIRCoercion::PtrToInt { to_type } => FMIRCastIntrinsic::PtrToInt {
            to_bits: to_type.bytes() * 8,
        },
        MIRCoercion::IntToPtr { sextend } => FMIRCastIntrinsic::IntToPtr {
            sextend: *sextend,
        },
        MIRCoercion::IntToFloat { to_type, sextend } => FMIRCastIntrinsic::IntToFloat {
            to_bits: to_type.bytes() * 8,
            sextend: *sextend,
        },
        MIRCoercion::FloatToInt { to_type, sextend } => FMIRCastIntrinsic::FloatToInt {
            to_bits: to_type.bytes() * 8,
            sextend: *sextend,
        },
        MIRCoercion::IntToBool => FMIRCastIntrinsic::IntToBool,
        MIRCoercion::ReinterpretBits => FMIRCastIntrinsic::ReinterpretBits,
    }
}

fn app1(intrinsic: FMIRIntrinsicKind, arg: FMIRNode, output_type: &MIRType) -> FMIRNode {
    FMIRNode {
        source_range: None,
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(intrinsic_alias(intrinsic)),
            argument: FRc::new(arg),
        },
    }
}

fn app2(intrinsic: FMIRIntrinsicKind, lhs: FMIRNode, rhs: FMIRNode, output_type: &MIRType) -> FMIRNode {
    FMIRNode {
        source_range: None,
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(FMIRNode {
                source_range: None,
                _type: FMIRType::pure(MIRType::internal_function()),
                body: FMIRNodeBody::Application {
                    function: FRc::new(intrinsic_alias(intrinsic)),
                    argument: FRc::new(lhs),
                },
            }),
            argument: FRc::new(rhs),
        },
    }
}

fn source_variable_name(expr: &MIRExpression) -> Option<&CXIdent> {
    match &expr.kind {
        MIRExpressionKind::Variable(name) | MIRExpressionKind::ContractVariable { name, .. } => {
            Some(name)
        }
        _ => None,
    }
}

fn read_operation_for_expr(env: &FMIREnvironment, source: &MIRExpression) -> CVMOperation {
    source_variable_name(source)
        .and_then(|name| env.query_memory_location(name))
        .map(|location| CVMOperation::Access {
            reads: vec![location],
            writes: vec![],
        })
        .unwrap_or(CVMOperation::Unsafe)
}

fn write_operation_for_expr(env: &FMIREnvironment, target: &MIRExpression) -> CVMOperation {
    source_variable_name(target)
        .and_then(|name| env.query_memory_location(name))
        .map(|location| CVMOperation::Access {
            reads: vec![],
            writes: vec![location],
        })
        .unwrap_or(CVMOperation::Unsafe)
}

fn invalidate_known_value_for_expr(env: &mut FMIREnvironment, target: &MIRExpression) {
    if let Some(name) = source_variable_name(target) {
        env.set_known_value(name, None);
    }
}

fn load_node(pointer: FMIRNode, value_type: &MIRType, operation: CVMOperation) -> FMIRNode {
    let read_effect = FMIRType::CMonad {
        inner: Box::new(FMIRType::pure(value_type.clone())),
        operation,
    };
    let combined = pointer
        ._type
        .union(&read_effect)
        .apply(FMIRType::pure(value_type.clone()));

    FMIRNode {
        source_range: None,
        _type: combined,
        body: FMIRNodeBody::Load {
            pointer: FRc::new(pointer),
        },
    }
}

fn store_node(pointer: FMIRNode, value: FMIRNode, operation: CVMOperation) -> FMIRNode {
    let write_effect = monad_unit(operation);
    let combined = pointer
        ._type
        .union(&value._type)
        .union(&write_effect)
        .apply(FMIRType::pure(MIRType::unit()));

    FMIRNode {
        source_range: None,
        _type: combined,
        body: FMIRNodeBody::Store {
            pointer: FRc::new(pointer),
            value: FRc::new(value),
        },
    }
}

fn increment_amount_node(value: i64, mir_type: &MIRType) -> CXResult<FMIRNode> {
    let MIRTypeKind::Integer { _type, signed } = &mir_type.kind else {
        return CXError::create_result(format!(
            "FMIR increment desugaring expected integer type, found '{}'",
            mir_type
        ));
    };

    Ok(FMIRNode {
        source_range: None,
        body: FMIRNodeBody::IntegerLiteral(value),
        _type: FMIRType::pure(MIRType::from(MIRTypeKind::Integer {
            _type: *_type,
            signed: *signed,
        })),
    })
}

fn convert_increment(
    env: &mut FMIREnvironment,
    operand_expr: &MIRExpression,
    amount: i8,
    is_pre: bool,
) -> CXResult<FMIRNode> {
    let pointer_node = convert_expression(env, operand_expr)?;
    let Some(value_type) = operand_expr._type.mem_ref_inner().cloned() else {
        return CXError::create_result(format!(
            "FMIR increment desugaring expected memory reference operand, found '{}'",
            operand_expr._type
        ));
    };

    let old_value_load = load_node(
        pointer_node.clone(),
        &value_type,
        read_operation_for_expr(env, operand_expr),
    );

    let old_capture = CXIdent::from("__inc_old");
    let old_alias = FMIRNode {
        source_range: None,
        body: FMIRNodeBody::VariableAlias {
            name: old_capture.as_string(),
        },
        _type: FMIRType::pure(value_type.clone()),
    };

    let (add_intrinsic, delta_node) = match &value_type.kind {
        MIRTypeKind::Integer { _type, .. } => (
            FMIRIntrinsicKind::Binary(FMIRBinaryIntrinsic::Integer {
                bits: _type.bytes() * 8,
                op: FMIRIntegerBinaryIntrinsicOp::Add,
            }),
            increment_amount_node(i64::from(amount), &value_type)?,
        ),
        MIRTypeKind::PointerTo { .. } => {
            let op = if amount < 0 {
                FMIRPointerDiffBinaryIntrinsicOp::Sub
            } else {
                FMIRPointerDiffBinaryIntrinsicOp::Add
            };
            let delta_type = MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I64,
                signed: true,
            });
            (
                FMIRIntrinsicKind::Binary(FMIRBinaryIntrinsic::PointerDiff { op }),
                increment_amount_node(i64::from(amount).abs(), &delta_type)?,
            )
        }
        _ => {
            return CXError::create_result(format!(
                "FMIR increment desugaring requires integer or pointer inner type, found '{}'",
                value_type
            ));
        }
    };

    let new_value = app2(add_intrinsic, old_alias.clone(), delta_node, &value_type);

    invalidate_known_value_for_expr(env, operand_expr);
    let store = store_node(
        pointer_node,
        new_value.clone(),
        write_operation_for_expr(env, operand_expr),
    );

    let result_value = if is_pre { new_value } else { old_alias };
    Ok(bind_node(
        old_value_load,
        old_capture,
        then_node(store, result_value),
    ))
}

fn unsupported_expression_error(env: &FMIREnvironment, expr: &MIRExpression) -> CXResult<FMIRNode> {
    CXError::create_result(format!(
        "FMIR conversion does not currently support expression '{}' in function '{}'",
        expr,
        env.current_mir_prototype().name,
    ))
}

fn with_expression_range(mut node: FMIRNode, mir_expr: &MIRExpression) -> FMIRNode {
    if node.source_range.is_none() {
        node.source_range = mir_expr.source_range.as_ref().map(FMIRSourceRange::from);
    }

    node
}

pub fn convert_expression(
    env: &mut FMIREnvironment,
    mir_expr: &MIRExpression,
) -> CXResult<FMIRNode> {
    let node = match &mir_expr.kind {
        MIRExpressionKind::BoolLiteral(value) => Ok(FMIRNode {
            source_range: None,
            body: FMIRNodeBody::BooleanLiteral(*value),
            _type: FMIRType::pure(MIRType::bool()),
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
            if !mir_expr._type.is_memory_reference() {
                if let Some(known) = env.query_known_value(name) {
                    return Ok(known);
                }
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
            if let MIRUnOp::PreIncrement(amount) = op {
                return Ok(with_expression_range(
                    convert_increment(env, operand, *amount, true)?,
                    mir_expr,
                ));
            }
            if let MIRUnOp::PostIncrement(amount) = op {
                return Ok(with_expression_range(
                    convert_increment(env, operand, *amount, false)?,
                    mir_expr,
                ));
            }

            let operand_node = convert_expression(env, operand)?;
            let Some(intrinsic) = unary_op_intrinsic(op) else {
                return unsupported_expression_error(env, mir_expr);
            };
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
            {
                if let Some(mut known) = env.query_known_value(name) {
                    if let Some(location) = env.query_memory_location(name) {
                        let access = FMIRType::access(
                            known._type.inner_type().clone(),
                            vec![location],
                            vec![],
                        );
                        known._type = known
                            ._type
                            .union(&access)
                            .apply(known._type.inner_type().clone());
                    }

                    return Ok(known);
                }
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

        _ => unsupported_expression_error(env, mir_expr),
    }?;

    Ok(with_expression_range(node, mir_expr))
}
