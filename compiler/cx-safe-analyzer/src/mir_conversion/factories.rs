use cx_mir::mir::{
    expression::{MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp, MIRPtrBinOp, MIRPtrDiffBinOp, MIRUnOp},
    types::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_safe_ir::{ast::*, intrinsic::*};
use cx_util::{CXError, CXResult, identifier::CXIdent};

use crate::mir_conversion::{environment::FMIREnvironment, expression::convert_expression};

pub(crate) fn monad_unit(operation: CVMOperation) -> FMIRType {
    FMIRType::CMonad {
        inner: Box::new(FMIRType::pure(MIRType::unit())),
        operation,
    }
}

pub(crate) fn pointer_alias(name: &CXIdent, pointer_type: MIRType) -> FMIRNode {
    FMIRNode {
        source_range: None,
        body: FMIRNodeBody::VariableAlias {
            name: name.as_string(),
        },
        _type: FMIRType::pure(pointer_type),
    }
}

pub(crate) fn intrinsic_alias(intrinsic: FMIRIntrinsicKind) -> FMIRNode {
    FMIRNode {
        source_range: None,
        body: FMIRNodeBody::IntrinsicFunction(FMIRIntrinsicFunction { kind: intrinsic }),
        _type: FMIRType::pure(MIRType::internal_function()),
    }
}

pub(crate) fn then_node(first: FMIRNode, second: FMIRNode) -> FMIRNode {
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

pub(crate) fn bind_node(monad: FMIRNode, capture: CXIdent, function: FMIRNode) -> FMIRNode {
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

pub(crate) fn chain_statements(statements: Vec<FMIRNode>) -> FMIRNode {
    let mut iter = statements.into_iter();
    let Some(first) = iter.next() else {
        return FMIRNode::unit();
    };

    iter.fold(first, then_node)
}

pub(crate) fn unary_op_intrinsic(op: &MIRUnOp) -> Option<FMIRUnaryIntrinsic> {
    Some(match op {
        MIRUnOp::NEG => FMIRUnaryIntrinsic::Neg,
        MIRUnOp::INEG => FMIRUnaryIntrinsic::INeg,
        MIRUnOp::FNEG => FMIRUnaryIntrinsic::FNeg,
        MIRUnOp::BNOT => FMIRUnaryIntrinsic::BNot,
        MIRUnOp::LNOT => FMIRUnaryIntrinsic::LNot,
        MIRUnOp::PreIncrement(_) | MIRUnOp::PostIncrement(_) => return None,
    })
}

pub(crate) fn int_binop_intrinsic(op: &MIRIntegerBinOp) -> FMIRIntrinsicIBinOp {
    match op {
        MIRIntegerBinOp::ADD => FMIRIntrinsicIBinOp::Add,
        MIRIntegerBinOp::SUB => FMIRIntrinsicIBinOp::Sub,
        MIRIntegerBinOp::MUL => FMIRIntrinsicIBinOp::Mul,
        MIRIntegerBinOp::DIV => FMIRIntrinsicIBinOp::Div,
        MIRIntegerBinOp::MOD => FMIRIntrinsicIBinOp::Mod,
        MIRIntegerBinOp::IMUL => FMIRIntrinsicIBinOp::IMul,
        MIRIntegerBinOp::IDIV => FMIRIntrinsicIBinOp::IDiv,
        MIRIntegerBinOp::IMOD => FMIRIntrinsicIBinOp::IMod,
        MIRIntegerBinOp::EQ => FMIRIntrinsicIBinOp::Eq,
        MIRIntegerBinOp::NE => FMIRIntrinsicIBinOp::Ne,
        MIRIntegerBinOp::LT => FMIRIntrinsicIBinOp::Lt,
        MIRIntegerBinOp::LE => FMIRIntrinsicIBinOp::Le,
        MIRIntegerBinOp::GT => FMIRIntrinsicIBinOp::Gt,
        MIRIntegerBinOp::GE => FMIRIntrinsicIBinOp::Ge,
        MIRIntegerBinOp::ILT => FMIRIntrinsicIBinOp::ILt,
        MIRIntegerBinOp::ILE => FMIRIntrinsicIBinOp::ILe,
        MIRIntegerBinOp::IGT => FMIRIntrinsicIBinOp::IGt,
        MIRIntegerBinOp::IGE => FMIRIntrinsicIBinOp::IGe,
        MIRIntegerBinOp::LAND => FMIRIntrinsicIBinOp::LAnd,
        MIRIntegerBinOp::LOR => FMIRIntrinsicIBinOp::LOr,
        MIRIntegerBinOp::BAND => FMIRIntrinsicIBinOp::BAnd,
        MIRIntegerBinOp::BOR => FMIRIntrinsicIBinOp::BOr,
        MIRIntegerBinOp::BXOR => FMIRIntrinsicIBinOp::BXor,
    }
}

pub(crate) fn float_binop_intrinsic(op: &MIRFloatBinOp) -> FMIRIntrinsicFBinOp {
    use MIRFloatBinOp as MIR;
    use FMIRIntrinsicFBinOp as FMIR;
    
    match op {
        MIR::FADD    => FMIR::Add,
        MIR::FSUB    => FMIR::Sub,
        MIR::FMUL    => FMIR::Mul,
        MIR::FDIV    => FMIR::Div,
        MIR::EQ      => FMIR::Eq,
        MIR::NEQ     => FMIR::Ne,
        MIR::FLT     => FMIR::Lt,
        MIR::FLE     => FMIR::Le,
        MIR::FGT     => FMIR::Gt,
        MIR::FGE     => FMIR::Ge,
    }
}

pub(crate) fn ptrdiff_binop_intrinsic(op: &MIRPtrDiffBinOp) -> FMIRPointerDiffBinaryIntrinsicOp {
    match op {
        MIRPtrDiffBinOp::ADD => FMIRPointerDiffBinaryIntrinsicOp::Add,
        MIRPtrDiffBinOp::SUB => FMIRPointerDiffBinaryIntrinsicOp::Sub,
    }
}

pub(crate) fn ptr_binop_intrinsic(op: &MIRPtrBinOp) -> FMIRPointerBinaryIntrinsicOp {
    match op {
        MIRPtrBinOp::EQ => FMIRPointerBinaryIntrinsicOp::Eq,
        MIRPtrBinOp::NE => FMIRPointerBinaryIntrinsicOp::Ne,
        MIRPtrBinOp::LT => FMIRPointerBinaryIntrinsicOp::Lt,
        MIRPtrBinOp::GT => FMIRPointerBinaryIntrinsicOp::Gt,
        MIRPtrBinOp::LE => FMIRPointerBinaryIntrinsicOp::Le,
        MIRPtrBinOp::GE => FMIRPointerBinaryIntrinsicOp::Ge,
    }
}

pub(crate) fn binary_op_intrinsic(op: &MIRBinOp) -> FMIRBinaryIntrinsic {
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

pub(crate) fn coercion_intrinsic(coercion: &MIRCoercion) -> FMIRCastIntrinsic {
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
        MIRCoercion::IntToPtr { sextend } => FMIRCastIntrinsic::IntToPtr { sextend: *sextend },
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

pub(crate) fn app1(intrinsic: FMIRIntrinsicKind, arg: FMIRNode, output_type: &MIRType) -> FMIRNode {
    FMIRNode {
        source_range: None,
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(intrinsic_alias(intrinsic)),
            argument: FRc::new(arg),
        },
    }
}

pub(crate) fn app2(
    intrinsic: FMIRIntrinsicKind,
    lhs: FMIRNode,
    rhs: FMIRNode,
    output_type: &MIRType,
) -> FMIRNode {
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

pub(crate) fn source_variable_name(expr: &MIRExpression) -> Option<&CXIdent> {
    match &expr.kind {
        MIRExpressionKind::Variable(name) | MIRExpressionKind::ContractVariable { name, .. } => {
            Some(name)
        }
        _ => None,
    }
}

pub(crate) fn read_operation_for_expr(
    env: &FMIREnvironment,
    source: &MIRExpression,
) -> CVMOperation {
    source_variable_name(source)
        .and_then(|name| env.query_memory_location(name))
        .map(|location| CVMOperation::Access {
            reads: vec![location],
            writes: vec![],
        })
        .unwrap_or(CVMOperation::Unsafe)
}

pub(crate) fn write_operation_for_expr(
    env: &FMIREnvironment,
    target: &MIRExpression,
) -> CVMOperation {
    source_variable_name(target)
        .and_then(|name| env.query_memory_location(name))
        .map(|location| CVMOperation::Access {
            reads: vec![],
            writes: vec![location],
        })
        .unwrap_or(CVMOperation::Unsafe)
}

pub(crate) fn invalidate_known_value_for_expr(env: &mut FMIREnvironment, target: &MIRExpression) {
    if let Some(name) = source_variable_name(target) {
        env.set_known_value(name, None);
    }
}

pub(crate) fn load_node(
    pointer: FMIRNode,
    value_type: &MIRType,
    operation: CVMOperation,
) -> FMIRNode {
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

pub(crate) fn store_node(pointer: FMIRNode, value: FMIRNode, operation: CVMOperation) -> FMIRNode {
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

pub(crate) fn increment_amount_node(value: i64, mir_type: &MIRType) -> CXResult<FMIRNode> {
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

pub(crate) fn convert_increment(
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
                op: FMIRIntrinsicIBinOp::Add,
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

pub(crate) fn unsupported_expression_error(
    env: &FMIREnvironment,
    expr: &MIRExpression,
) -> CXResult<FMIRNode> {
    CXError::create_result(format!(
        "FMIR conversion does not currently support expression '{}' in function '{}'",
        expr,
        env.current_mir_prototype().name,
    ))
}

pub(crate) fn with_expression_range(mut node: FMIRNode, mir_expr: &MIRExpression) -> FMIRNode {
    if node.source_range.is_none() {
        node.source_range = mir_expr.source_range.as_ref().map(FMIRSourceRange::from);
    }

    node
}
