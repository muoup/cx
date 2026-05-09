use cx_ast::ast::CXBinOp;
use cx_mir::mir::{
    expression::{
        MIRBinOp, MIRCoercion, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp,
        MIRPtrBinOp, MIRPtrDiffBinOp,
    },
    r#type::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::TypecheckResult,
    },
};

pub(crate) mod access;
pub(crate) mod assign;
pub(crate) mod calls;
pub(crate) mod is;
pub(crate) mod scoped_calls;

pub(crate) fn dispatch(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    match &op {
        CXBinOp::LOr | CXBinOp::LAnd => resolve_logical(env, op, lhs, rhs),

        _ => resolve_std_arithmetic(env, op, lhs, rhs),
    }
}

pub(crate) fn resolve_logical(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    lhs = std_rval_promotion(env, lhs)?;
    rhs = std_rval_promotion(env, rhs)?;

    let valid_logical_operand = |expr: &MIRExpression| {
        expr._type.is_integer() || expr._type.is_float() || expr._type.is_pointer()
    };

    if !valid_logical_operand(&lhs) || !valid_logical_operand(&rhs) {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Invalid operands to logical operation {:?}, {} and {}",
            op,
            lhs._type.display_with(&env.symbols.context),
            rhs._type.display_with(&env.symbols.context)
        );
    }

    let lhs = coerce_scalar_to_bool(env, lhs)?;
    let rhs = coerce_scalar_to_bool(env, rhs)?;

    let operator = MIRBinOp::Integer {
        itype: MIRIntegerType::I1,
        op: match op {
            CXBinOp::LAnd => MIRIntegerBinOp::LAND,
            CXBinOp::LOr => MIRIntegerBinOp::LOR,
            _ => unreachable!(),
        },
    };

    Ok(TypecheckResult::new_base(
        MIRType::bool(),
        MIRExpressionKind::BinaryOperation {
            op: operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn coerce_scalar_to_bool(
    env: &mut TypeEnvironment,
    expr: MIRExpression,
) -> CXResult<MIRExpression> {
    if expr._type.is_integer() {
        return implicit_cast(env, expr, &MIRType::bool());
    }

    match &expr._type.kind {
        MIRTypeKind::Float { _type } => {
            let zero = MIRExpression {
                token_range: expr.token_range.clone(),
                kind: MIRExpressionKind::FloatLiteral(0.0.into(), *_type),
                _type: expr._type.clone(),
            };

            Ok(MIRExpression {
                token_range: expr.token_range.clone(),
                kind: MIRExpressionKind::BinaryOperation {
                    op: MIRBinOp::Float {
                        ftype: *_type,
                        op: MIRFloatBinOp::FNE,
                    },
                    lhs: Box::new(expr),
                    rhs: Box::new(zero),
                },
                _type: MIRType::bool(),
            })
        }
        MIRTypeKind::PointerTo { .. } => {
            let zero = MIRExpression {
                token_range: expr.token_range.clone(),
                kind: MIRExpressionKind::TypeConversion {
                    conversion: MIRCoercion::IntToPtr { sextend: false },
                    operand: Box::new(MIRExpression {
                        token_range: expr.token_range.clone(),
                        kind: MIRExpressionKind::IntLiteral(0, MIRIntegerType::I64, false),
                        _type: MIRTypeKind::Integer {
                            _type: MIRIntegerType::I64,
                            signed: false,
                        }
                        .into(),
                    }),
                },
                _type: expr._type.clone(),
            };

            Ok(MIRExpression {
                token_range: expr.token_range.clone(),
                kind: MIRExpressionKind::BinaryOperation {
                    op: MIRBinOp::Pointer {
                        op: MIRPtrBinOp::NE,
                    },
                    lhs: Box::new(expr),
                    rhs: Box::new(zero),
                },
                _type: MIRType::bool(),
            })
        }
        _ => unreachable!("logical operands should already be scalar"),
    }
}

pub(crate) fn resolve_std_arithmetic(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    lhs = std_rval_promotion(env, lhs)?;
    rhs = std_rval_promotion(env, rhs)?;

    if lhs._type.is_float() || rhs._type.is_float() {
        coerce_float_binop(env, op, lhs, rhs)
    } else if lhs._type.is_pointer() || rhs._type.is_pointer() {
        coerce_pointer_binop(env, op, lhs, rhs)
    } else if lhs._type.is_integer() && rhs._type.is_integer() {
        coerce_integral_binop(env, op, lhs, rhs)
    } else {
        log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Invalid binary operation {op} for types {} and {}",
            lhs.get_type().display_with(&env.symbols.context),
            rhs.get_type().display_with(&env.symbols.context)
        )
    }
}

fn coerce_float_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    if let MIRTypeKind::Float { _type: lftype } = lhs._type.kind
        && let MIRTypeKind::Float { _type: rftype } = rhs._type.kind
        && lftype != rftype {
            let common_ftype = if lftype.bytes() > rftype.bytes() {
                lhs._type.clone()
            } else {
                rhs._type.clone()
            };

            rhs = implicit_cast(env, rhs, &common_ftype)?;
        }

    if !rhs._type.is_float() {
        rhs = implicit_cast(env, rhs, &lhs._type)?;
    } else {
        lhs = implicit_cast(env, lhs, &rhs._type)?;
    }

    let (op, return_type) = match op {
        CXBinOp::Add => (MIRFloatBinOp::FADD, lhs._type.clone()),
        CXBinOp::Subtract => (MIRFloatBinOp::FSUB, lhs._type.clone()),
        CXBinOp::Multiply => (MIRFloatBinOp::FMUL, lhs._type.clone()),
        CXBinOp::Divide => (MIRFloatBinOp::FDIV, lhs._type.clone()),

        CXBinOp::Equal => (MIRFloatBinOp::FEQ, MIRType::bool()),
        CXBinOp::NotEqual => (MIRFloatBinOp::FNE, MIRType::bool()),
        CXBinOp::Less => (MIRFloatBinOp::FLT, MIRType::bool()),
        CXBinOp::Greater => (MIRFloatBinOp::FGT, MIRType::bool()),
        CXBinOp::LessEqual => (MIRFloatBinOp::FLE, MIRType::bool()),
        CXBinOp::GreaterEqual => (MIRFloatBinOp::FGE, MIRType::bool()),

        _ => {
            return log_typecheck_error!(
                env,
                lhs.token_range.as_ref(),
                "Invalid float binary operation {op} for types {} and {}",
                lhs.get_type().display_with(&env.symbols.context),
                rhs.get_type().display_with(&env.symbols.context)
            );
        }
    };

    Ok(TypecheckResult::new_base(
        return_type,
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Float {
                ftype: match lhs._type.kind {
                    MIRTypeKind::Float { _type } => _type,
                    _ => unreachable!(),
                },
                op,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn coerce_pointer_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    if lhs._type.is_pointer() && rhs._type.is_pointer() {
        let (return_type, op) = match op {
            CXBinOp::LessEqual => (MIRType::bool(), MIRPtrBinOp::LE),
            CXBinOp::GreaterEqual => (MIRType::bool(), MIRPtrBinOp::GE),
            CXBinOp::Less => (MIRType::bool(), MIRPtrBinOp::LT),
            CXBinOp::Greater => (MIRType::bool(), MIRPtrBinOp::GT),
            CXBinOp::Equal => (MIRType::bool(), MIRPtrBinOp::EQ),
            CXBinOp::NotEqual => (MIRType::bool(), MIRPtrBinOp::NE),

            _ => {
                return log_typecheck_error!(
                    env,
                    lhs.token_range.as_ref(),
                    "Invalid binary operation {op} for pointer types"
                );
            }
        };

        return Ok(TypecheckResult::new_base(
            return_type,
            MIRExpressionKind::BinaryOperation {
                op: MIRBinOp::Pointer { op },
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        ));
    }

    let (pointer, non_pointer) = if lhs._type.is_pointer() {
        (&mut lhs, &mut rhs)
    } else {
        (&mut rhs, &mut lhs)
    };

    let intptr = MIRTypeKind::Integer {
        _type: MIRIntegerType::I64,
        signed: true,
    };

    *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &intptr.into())?;

    let ptr_type = pointer._type.clone();
    let ptr_inner = Box::new(env.symbols.context.ptr_inner(&ptr_type).cloned().unwrap());

    let (return_type, op) = match op {
        CXBinOp::Add => (
            ptr_type,
            MIRBinOp::PtrDiff {
                op: MIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        CXBinOp::ArrayIndex => (
            env.symbols.context.mem_ref_to(ptr_inner.as_ref().clone()),
            MIRBinOp::PtrDiff {
                op: MIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        CXBinOp::Subtract => (
            ptr_type,
            MIRBinOp::PtrDiff {
                op: MIRPtrDiffBinOp::SUB,
                ptr_inner,
            },
        ),

        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => (
            MIRType::bool(),
            MIRBinOp::Pointer {
                op: match op {
                    CXBinOp::LessEqual => MIRPtrBinOp::LE,
                    CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
                    CXBinOp::Less => MIRPtrBinOp::LT,
                    CXBinOp::Greater => MIRPtrBinOp::GT,
                    CXBinOp::Equal => MIRPtrBinOp::EQ,
                    CXBinOp::NotEqual => MIRPtrBinOp::NE,
                    _ => unreachable!(),
                },
            },
        ),

        _ => {
            return log_typecheck_error!(
                env,
                lhs.token_range.as_ref(),
                "Invalid binary operation {op} for pointer and non-pointer types"
            );
        }
    };

    Ok(TypecheckResult::new_base(
        return_type,
        MIRExpressionKind::BinaryOperation {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn coerce_integral_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    let MIRTypeKind::Integer { _type: litype, .. } = lhs._type.kind else {
        unreachable!("Expected integer type for lhs of integral binary operation");
    };
    let MIRTypeKind::Integer { _type: ritype, .. } = rhs._type.kind else {
        unreachable!("Expected integer type for rhs of integral binary operation");
    };

    if litype.rank() < ritype.rank() {
        lhs = implicit_cast(env, lhs, &rhs._type)?;
    } else if ritype.rank() < litype.rank() {
        rhs = implicit_cast(env, rhs, &lhs._type)?;
    }

    let return_type = match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus
        | CXBinOp::BitAnd
        | CXBinOp::BitOr
        | CXBinOp::BitXor => lhs._type.clone(),

        CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => MIRType::bool(),

        _ => {
            return log_typecheck_error!(
                env,
                lhs.token_range.as_ref(),
                "Invalid integer binary operation {op} for types {} and {}",
                lhs.get_type().display_with(&env.symbols.context),
                rhs.get_type().display_with(&env.symbols.context)
            );
        }
    };

    let Some(op) = lower_int_binop(op, true) else {
        return log_typecheck_error!(
            env,
            lhs.token_range.as_ref(),
            "Invalid integer binary operation {op} for types {} and {}",
            lhs.get_type().display_with(&env.symbols.context),
            rhs.get_type().display_with(&env.symbols.context)
        );
    };

    Ok(TypecheckResult::new_base(
        return_type,
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Integer {
                itype: match lhs._type.kind {
                    MIRTypeKind::Integer { _type, .. } => _type,
                    _ => unreachable!(),
                },
                op,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn lower_int_binop(op: &CXBinOp, signed: bool) -> Option<MIRIntegerBinOp> {
    Some(match op {
        CXBinOp::Add => MIRIntegerBinOp::ADD,
        CXBinOp::Subtract => MIRIntegerBinOp::SUB,
        CXBinOp::Multiply => MIRIntegerBinOp::MUL,
        CXBinOp::Divide => MIRIntegerBinOp::DIV,
        CXBinOp::Modulus => MIRIntegerBinOp::MOD,

        CXBinOp::Less if !signed => MIRIntegerBinOp::LT,
        CXBinOp::Less if signed => MIRIntegerBinOp::ILT,

        CXBinOp::Greater if !signed => MIRIntegerBinOp::GT,
        CXBinOp::Greater if signed => MIRIntegerBinOp::IGT,

        CXBinOp::LessEqual if !signed => MIRIntegerBinOp::LE,
        CXBinOp::LessEqual if signed => MIRIntegerBinOp::ILE,

        CXBinOp::GreaterEqual if !signed => MIRIntegerBinOp::GE,
        CXBinOp::GreaterEqual if signed => MIRIntegerBinOp::IGE,

        CXBinOp::Equal => MIRIntegerBinOp::EQ,
        CXBinOp::NotEqual => MIRIntegerBinOp::NE,

        CXBinOp::BitAnd => MIRIntegerBinOp::BAND,
        CXBinOp::BitOr => MIRIntegerBinOp::BOR,
        CXBinOp::BitXor => MIRIntegerBinOp::BXOR,

        _ => return None,
    })
}
