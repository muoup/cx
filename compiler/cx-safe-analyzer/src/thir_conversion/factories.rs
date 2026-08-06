use cx_log::CXResult;
use cx_thir::{
    thir::{
        data::{THIRIntType, THIRType, THIRTypeKind},
        expression::{
            THIRBinOp, THIRCoercion, THIRExpression, THIRExpressionKind, THIRFloatBinOp,
            THIRIntBinOp, THIRPtrBinOp, THIRPtrDiffBinOp, THIRUnOp,
        },
    },
    type_context::THIRTypeContext,
};
use cx_safe_ir::{ast::*, intrinsic::*};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    log::AnalysisDiagnosticSource,
    thir_conversion::{environment::FMIREnvironment, expression::convert_expression},
};

pub(crate) fn monad_unit(operation: CVMOperation) -> FMIRType {
    FMIRType::CMonad {
        inner: Box::new(FMIRType::pure(THIRType::unit())),
        operation,
    }
}

pub(crate) fn intrinsic_alias(intrinsic: FMIRIntrinsicKind) -> FMIRNode {
    FMIRNode {
        token_range: TokenRange::internal(),
        body: FMIRNodeBody::IntrinsicFunction(FMIRIntrinsicFunction { kind: intrinsic }),
        _type: FMIRType::pure(THIRType::internal_function()),
    }
}

pub(crate) fn then_node(first: FMIRNode, second: FMIRNode) -> FMIRNode {
    let combined = first._type.union(&second._type);
    FMIRNode {
        token_range: TokenRange::internal(),
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
        token_range: TokenRange::internal(),
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

pub fn unary_op_intrinsic(op: &THIRUnOp) -> FMIRUnaryIntrinsic {
    use FMIRUnaryIntrinsic as FMIR;
    use THIRUnOp as MIR;

    match op {
        MIR::NEG => FMIR::NEG,
        MIR::INEG => FMIR::INEG,
        MIR::FNEG => FMIR::FNEG,
        MIR::BNOT => FMIR::BNOT,
        MIR::LNOT => FMIR::LNOT,
        MIR::PreIncrement(_) | MIR::PostIncrement(_) => {
            unreachable!("increments are desugared before unary_op_intrinsic is called")
        }
    }
}

pub fn int_binop_intrinsic(op: &THIRIntBinOp) -> FMIRIntrinsicIBinOp {
    use FMIRIntrinsicIBinOp as FMIR;
    use THIRIntBinOp as MIR;

    match op {
        MIR::ADD => FMIR::ADD,
        MIR::SUB => FMIR::SUB,
        MIR::MUL => FMIR::MUL,
        MIR::DIV => FMIR::DIV,
        MIR::MOD => FMIR::MOD,
        MIR::IMUL => FMIR::IMUL,
        MIR::IDIV => FMIR::IDIV,
        MIR::IMOD => FMIR::IMOD,
        MIR::EQ => FMIR::EQ,
        MIR::NE => FMIR::NE,
        MIR::LT => FMIR::LT,
        MIR::LE => FMIR::LE,
        MIR::GT => FMIR::GT,
        MIR::GE => FMIR::GE,
        MIR::ILT => FMIR::ILT,
        MIR::ILE => FMIR::ILE,
        MIR::IGT => FMIR::IGT,
        MIR::IGE => FMIR::IGE,
        MIR::LAND => FMIR::LAND,
        MIR::LOR => FMIR::LOR,
        MIR::BAND => FMIR::BAND,
        MIR::BOR => FMIR::BOR,
        MIR::BXOR => FMIR::BXOR,
        MIR::SHL => FMIR::SHL,
        MIR::ASHR => FMIR::ASHR,
        MIR::LSHR => FMIR::LSHR,
    }
}

pub fn float_binop_intrinsic(op: &THIRFloatBinOp) -> FMIRIntrinsicFBinOp {
    use FMIRIntrinsicFBinOp as FMIR;
    use THIRFloatBinOp as MIR;

    match op {
        MIR::FADD => FMIR::FADD,
        MIR::FSUB => FMIR::FSUB,
        MIR::FMUL => FMIR::FMUL,
        MIR::FDIV => FMIR::FDIV,
        MIR::FEQ => FMIR::FEQ,
        MIR::FNE => FMIR::FNE,
        MIR::FLT => FMIR::FLT,
        MIR::FLE => FMIR::FLE,
        MIR::FGT => FMIR::FGT,
        MIR::FGE => FMIR::FGE,
    }
}

pub fn ptrdiff_binop_intrinsic(op: &THIRPtrDiffBinOp) -> FMIRIntrinsicPtrDiffBinop {
    use FMIRIntrinsicPtrDiffBinop as FMIR;
    use THIRPtrDiffBinOp as MIR;

    match op {
        MIR::ADD => FMIR::ADD,
        MIR::SUB => FMIR::SUB,
    }
}

pub fn ptr_binop_intrinsic(op: &THIRPtrBinOp) -> FMIRPointerBinaryIntrinsicOp {
    use FMIRPointerBinaryIntrinsicOp as FMIR;
    use THIRPtrBinOp as MIR;

    match op {
        MIR::EQ => FMIR::EQ,
        MIR::NE => FMIR::NE,
        MIR::LT => FMIR::LT,
        MIR::GT => FMIR::GT,
        MIR::LE => FMIR::LE,
        MIR::GE => FMIR::GE,
    }
}

pub fn binary_op_intrinsic(op: &THIRBinOp) -> FMIRBinaryIntrinsic {
    match op {
        THIRBinOp::Integer { itype, op } => FMIRBinaryIntrinsic::Integer {
            bits: itype.bytes() * 8,
            op: int_binop_intrinsic(op),
        },
        THIRBinOp::Float { ftype, op } => FMIRBinaryIntrinsic::Float {
            bits: ftype.bytes() * 8,
            op: float_binop_intrinsic(op),
        },
        THIRBinOp::PtrDiff { op, .. } => FMIRBinaryIntrinsic::PointerDiff {
            op: ptrdiff_binop_intrinsic(op),
        },
        THIRBinOp::Pointer { op } => FMIRBinaryIntrinsic::Pointer {
            op: ptr_binop_intrinsic(op),
        },
    }
}

pub fn coercion_intrinsic(
    _env: &FMIREnvironment,
    _expr: &THIRExpression,
    coercion: &THIRCoercion,
) -> CXResult<FMIRCastIntrinsic> {
    Ok(match coercion {
        THIRCoercion::Integral {
            sextend, to_type, ..
        } => FMIRCastIntrinsic::Integral {
            sextend: *sextend,
            to_bits: to_type.bytes() * 8,
        },
        THIRCoercion::FloatCast { to_type } => FMIRCastIntrinsic::FloatCast {
            to_bits: to_type.bytes() * 8,
        },
        THIRCoercion::PtrToInt { to_type } => FMIRCastIntrinsic::PtrToInt {
            to_bits: to_type.bytes() * 8,
        },
        THIRCoercion::IntToPtr { sextend } => FMIRCastIntrinsic::IntToPtr { sextend: *sextend },
        THIRCoercion::IntToFloat { to_type, sextend } => FMIRCastIntrinsic::IntToFloat {
            to_bits: to_type.bytes() * 8,
            sextend: *sextend,
        },
        THIRCoercion::FloatToInt { to_type, sextend } => FMIRCastIntrinsic::FloatToInt {
            to_bits: to_type.bytes() * 8,
            sextend: *sextend,
        },
        THIRCoercion::Typechange | THIRCoercion::ReinterpretBits => {
            FMIRCastIntrinsic::ReinterpretBits
        }
        THIRCoercion::GetFnPtr => {
            return _env.log_error(
                _expr,
                "Function pointer decay is not supported in safe analysis yet".to_string(),
            );
        }
    })
}

pub(crate) fn app1(intrinsic: FMIRIntrinsicKind, arg: FMIRNode, output_type: &THIRType) -> FMIRNode {
    FMIRNode {
        token_range: TokenRange::internal(),
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
    output_type: &THIRType,
) -> FMIRNode {
    FMIRNode {
        token_range: TokenRange::internal(),
        _type: FMIRType::pure(output_type.clone()),
        body: FMIRNodeBody::Application {
            function: FRc::new(FMIRNode {
                token_range: TokenRange::internal(),
                _type: FMIRType::pure(THIRType::internal_function()),
                body: FMIRNodeBody::Application {
                    function: FRc::new(intrinsic_alias(intrinsic)),
                    argument: FRc::new(lhs),
                },
            }),
            argument: FRc::new(rhs),
        },
    }
}

pub(crate) fn source_variable_name(expr: &THIRExpression) -> Option<&CXIdent> {
    match &expr.kind {
        THIRExpressionKind::Variable { name, .. }
        | THIRExpressionKind::ContractVariable { name, .. } => Some(name),
        THIRExpressionKind::MemberAccess { base, .. } => source_variable_name(base),
        _ => None,
    }
}

pub(crate) fn read_operation_for_expr(
    env: &FMIREnvironment,
    source: &THIRExpression,
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
    target: &THIRExpression,
) -> CVMOperation {
    source_variable_name(target)
        .and_then(|name| env.query_memory_location(name))
        .map(|location| CVMOperation::Access {
            reads: vec![],
            writes: vec![location],
        })
        .unwrap_or(CVMOperation::Unsafe)
}

pub(crate) fn invalidate_known_value_for_expr(env: &mut FMIREnvironment, target: &THIRExpression) {
    if let Some(name) = source_variable_name(target) {
        env.set_known_value(name, None);
    }
}

pub(crate) fn load_node(
    pointer: FMIRNode,
    value_type: &THIRType,
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
        token_range: TokenRange::internal(),
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
        .apply(FMIRType::pure(THIRType::unit()));

    FMIRNode {
        token_range: TokenRange::internal(),
        _type: combined,
        body: FMIRNodeBody::Store {
            pointer: FRc::new(pointer),
            value: FRc::new(value),
        },
    }
}

pub(crate) fn increment_amount_node(
    env: &FMIREnvironment,
    value: i64,
    mir_type: &THIRType,
) -> CXResult<FMIRNode> {
    let THIRTypeKind::Integer { _type, signed } = &mir_type.kind else {
        return crate::log::internal_analysis_error(format!(
            "FMIR increment desugaring expected integer type, found '{}'",
            mir_type.display_with(env.type_definitions)
        ));
    };

    Ok(FMIRNode {
        token_range: TokenRange::internal(),
        body: FMIRNodeBody::IntegerLiteral(value),
        _type: FMIRType::pure(THIRType::from(THIRTypeKind::Integer {
            _type: *_type,
            signed: *signed,
        })),
    })
}

pub(crate) fn convert_increment(
    env: &mut FMIREnvironment,
    operand_expr: &THIRExpression,
    amount: i8,
    is_pre: bool,
) -> CXResult<FMIRNode> {
    let pointer_node = convert_expression(env, operand_expr)?;
    let Some(value_type) = env
        .type_definitions
        .mem_ref_inner(&operand_expr._type)
        .cloned()
    else {
        return crate::log::internal_analysis_error(format!(
            "FMIR increment desugaring expected memory reference operand, found '{}'",
            operand_expr._type.display_with(env.type_definitions)
        ));
    };

    let old_value_load = load_node(
        pointer_node.clone(),
        &value_type,
        read_operation_for_expr(env, operand_expr),
    );

    let old_capture = CXIdent::from("__inc_old");
    let old_alias = FMIRNode {
        token_range: TokenRange::internal(),
        body: FMIRNodeBody::VariableAlias {
            name: old_capture.as_string(),
        },
        _type: FMIRType::pure(value_type.clone()),
    };

    let (add_intrinsic, delta_node) = match &value_type.kind {
        THIRTypeKind::Integer { _type, .. } => (
            FMIRIntrinsicKind::Binary(FMIRBinaryIntrinsic::Integer {
                bits: _type.bytes() * 8,
                op: FMIRIntrinsicIBinOp::ADD,
            }),
            increment_amount_node(env, i64::from(amount), &value_type)?,
        ),
        THIRTypeKind::PointerTo { .. } => {
            let op = if amount < 0 {
                FMIRIntrinsicPtrDiffBinop::SUB
            } else {
                FMIRIntrinsicPtrDiffBinop::ADD
            };
            let delta_type = THIRType::from(THIRTypeKind::Integer {
                _type: THIRIntType::I64,
                signed: true,
            });
            (
                FMIRIntrinsicKind::Binary(FMIRBinaryIntrinsic::PointerDiff { op }),
                increment_amount_node(env, i64::from(amount).abs(), &delta_type)?,
            )
        }
        _ => {
            return crate::log::internal_analysis_error(format!(
                "FMIR increment desugaring requires integer or pointer inner type, found '{}'",
                value_type.display_with(env.type_definitions)
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
    expr: &THIRExpression,
) -> CXResult<FMIRNode> {
    env.log_error(expr, "Expression is not supported in safe context, use `unsafe` block if no safe alternative is available".to_string())
}

pub(crate) fn with_expression_range(mut node: FMIRNode, thir_expr: &THIRExpression) -> FMIRNode {
    node.token_range = thir_expr.token_range.clone();
    node
}
