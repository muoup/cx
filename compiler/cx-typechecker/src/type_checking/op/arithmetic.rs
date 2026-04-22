use cx_ast::ast::{CXBinOp, CXExpr};
use cx_mir::mir::{
    expression::{MIRBinOp, MIRExpression, MIRExpressionKind, MIRFloatBinOp, MIRIntegerBinOp},
    r#type::{MIRIntegerType, MIRType, MIRTypeKind},
};
use cx_util::CXResult;

use crate::{
    environment::TypeEnvironment,
    log_typecheck_error,
    type_checking::{casting::implicit_cast, coercion::implicit::integer, result::TypecheckResult},
};

pub(crate) fn typecheck_logical_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
) -> CXResult<TypecheckResult> {
    let lhs = implicit_cast(env, lhs, &MIRType::bool())?;
    let rhs = implicit_cast(env, rhs, &MIRType::bool())?;

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

pub(crate) fn typecheck_float_float_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let lhs_type = lhs.get_type();
    let rhs_type = rhs.get_type();

    let ftype = match (&lhs_type.kind, &rhs_type.kind) {
        (MIRTypeKind::Float { _type: lhs_ftype }, MIRTypeKind::Float { _type: rhs_ftype }) => {
            if rhs_ftype.bytes() > lhs_ftype.bytes() {
                *rhs_ftype
            } else {
                *lhs_ftype
            }
        }

        (MIRTypeKind::Float { _type }, MIRTypeKind::Integer { .. }) => *_type,
        (MIRTypeKind::Integer { .. }, MIRTypeKind::Float { _type }) => *_type,

        _ => unreachable!("Expected arithmetic types for floating-point binop"),
    };
    let common_type = MIRType::from(MIRTypeKind::Float { _type: ftype });

    lhs = implicit_cast(env, lhs, &common_type)?;
    rhs = implicit_cast(env, rhs, &common_type)?;

    let (result_type, fp_op) = match op {
        CXBinOp::Add => (common_type.clone(), MIRFloatBinOp::FADD),
        CXBinOp::Subtract => (common_type.clone(), MIRFloatBinOp::FSUB),
        CXBinOp::Multiply => (common_type.clone(), MIRFloatBinOp::FMUL),
        CXBinOp::Divide => (common_type.clone(), MIRFloatBinOp::FDIV),

        CXBinOp::Equal => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FEQ,
        ),
        CXBinOp::NotEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FNE,
        ),
        CXBinOp::Less => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FLT,
        ),
        CXBinOp::Greater => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FGT,
        ),
        CXBinOp::LessEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FLE,
        ),
        CXBinOp::GreaterEqual => (
            MIRType::from(MIRTypeKind::Integer {
                _type: MIRIntegerType::I1,
                signed: false,
            }),
            MIRFloatBinOp::FGE,
        ),

        _ => {
            return log_typecheck_error!(
                env,
                Some(expr.token_range()),
                "Invalid float binary operation {op} for types {} and {}",
                lhs.get_type(),
                rhs.get_type()
            );
        }
    };

    Ok(TypecheckResult::new_base(
        result_type,
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Float { ftype, op: fp_op },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn lower_int_binop(
    op: CXBinOp,
    signed: bool
) -> CXResult<MIRIntegerBinOp, ()> {
    match op {
        CXBinOp::Add => Ok(MIRIntegerBinOp::ADD),
        CXBinOp::Subtract => Ok(MIRIntegerBinOp::SUB),
        CXBinOp::Multiply => Ok(MIRIntegerBinOp::MUL),
        CXBinOp::Divide => Ok(MIRIntegerBinOp::DIV),
        CXBinOp::Modulus => Ok(MIRIntegerBinOp::MOD),

        CXBinOp::Less if !signed => Ok(MIRIntegerBinOp::LT),
        CXBinOp::Less if signed => Ok(MIRIntegerBinOp::ILT),

        CXBinOp::Greater if !signed => Ok(MIRIntegerBinOp::GT),
        CXBinOp::Greater if signed => Ok(MIRIntegerBinOp::IGT),

        CXBinOp::LessEqual if !signed => Ok(MIRIntegerBinOp::LE),
        CXBinOp::LessEqual if signed => Ok(MIRIntegerBinOp::ILE),

        CXBinOp::GreaterEqual if !signed => Ok(MIRIntegerBinOp::GE),
        CXBinOp::GreaterEqual if signed => Ok(MIRIntegerBinOp::IGE),

        CXBinOp::Equal => Ok(MIRIntegerBinOp::EQ),
        CXBinOp::NotEqual => Ok(MIRIntegerBinOp::NE),

        CXBinOp::BitAnd => Ok(MIRIntegerBinOp::BAND),
        CXBinOp::BitOr => Ok(MIRIntegerBinOp::BOR),
        CXBinOp::BitXor => Ok(MIRIntegerBinOp::BXOR),

        _ => log_typecheck_error!(
            env,
            None,
            "Invalid integer binary operation {op} for signed={signed}"
        ),
    }
}

pub(crate) fn typecheck_int_int_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    mut lhs: MIRExpression,
    mut rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    lhs = integer::try_promotion(env, lhs)
        .as_err(|expr, msg| log_typecheck_error!(env, expr.token_range, "{}", msg))?;
    rhs = integer::try_promotion(env, rhs)
        .as_err(|expr, msg| log_typecheck_error!(env, expr.token_range, "{}", msg))?;

    let lhs_type = lhs.get_type();
    let rhs_type = rhs.get_type();
    
    lhs = implicit_cast(env, lhs, &result_type)?;
    rhs = implicit_cast(env, rhs, &result_type)?;

    let MIRTypeKind::Integer {
        _type: itype,
        signed: result_signed,
    } = result_type.kind
    else {
        unreachable!("Expected integer result type");
    };

    let operator = lower_int_binop(op, result_signed)?;

    let result_type = match op {
        CXBinOp::Add
        | CXBinOp::Subtract
        | CXBinOp::Multiply
        | CXBinOp::Divide
        | CXBinOp::Modulus
        | CXBinOp::BitAnd
        | CXBinOp::BitOr
        | CXBinOp::BitXor => result_type.clone().into(),

        CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => MIRTypeKind::Integer {
            _type: MIRIntegerType::I1,
            signed: false,
        }
        .into(),

        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Invalid integer binary operation {op} for types {} and {}",
                lhs.get_type(),
                rhs.get_type()
            );
        }
    };

    Ok(TypecheckResult::new_base(
        result_type,
        MIRExpressionKind::BinaryOperation {
            op: operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub(crate) fn typecheck_int_ptr_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    pointer_inner: &MIRType,
    non_pointer: MIRExpression,
    pointer: MIRExpression,
) -> CXResult<TypecheckResult> {
    if op == CXBinOp::Subtract {
        return log_typecheck_error!(
            env,
            &TokenRange::default(),
            "Invalid operation [integer] - [pointer] for types {} and {}",
            non_pointer.get_type(),
            pointer.get_type()
        );
    }

    typecheck_ptr_int_binop(env, op, pointer_inner, pointer, non_pointer)
}

pub(crate) fn typecheck_ptr_int_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    pointer_inner: &MIRType,
    pointer: MIRExpression,
    integer: MIRExpression,
) -> CXResult<TypecheckResult> {
    match op {
        // Requires one pointer and one integer
        CXBinOp::Add | CXBinOp::Subtract | CXBinOp::ArrayIndex => {
            let rhs_type = integer.get_type();
            let int_sized_pointer = MIRTypeKind::Integer {
                _type: MIRIntegerType::I64,
                signed: true,
            }
            .into();

            let coerced_integer =
                implicit_cast(env, &CXExpr::default(), integer, &int_sized_pointer)?;

            let MIRTypeKind::Integer { _type, .. } = &rhs_type.kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            let operation = match op {
                CXBinOp::Add | CXBinOp::ArrayIndex => MIRBinOp::PtrDiff {
                    op: MIRPtrDiffBinOp::ADD,
                    ptr_inner: Box::new(pointer_inner.clone()),
                },

                CXBinOp::Subtract => MIRBinOp::PtrDiff {
                    op: MIRPtrDiffBinOp::SUB,
                    ptr_inner: Box::new(pointer_inner.clone()),
                },

                _ => unreachable!(),
            };

            let return_type = match op {
                CXBinOp::ArrayIndex => env.type_context.mem_ref_to(pointer_inner.clone()),

                _ => env.type_context.pointer_to(pointer_inner.clone()),
            };

            Ok(TypecheckResult::new_base(
                return_type.clone(),
                MIRExpressionKind::BinaryOperation {
                    op: operation,
                    lhs: Box::new(pointer),
                    rhs: Box::new(coerced_integer),
                },
            ))
        }

        // Requires two pointers
        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => {
            let MIRTypeKind::Integer { signed, _type } = integer.get_type().kind else {
                unreachable!("Expected integer type for pointer-integer binary operation");
            };

            let mir_op = match op {
                CXBinOp::LessEqual => MIRPtrBinOp::LE,
                CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
                CXBinOp::Less => MIRPtrBinOp::LT,
                CXBinOp::Greater => MIRPtrBinOp::GT,
                CXBinOp::Equal => MIRPtrBinOp::EQ,
                CXBinOp::NotEqual => MIRPtrBinOp::NE,
                _ => unreachable!(),
            };

            let coerced_val = implicit_cast(
                env,
                &CXExpr::default(),
                integer,
                &MIRTypeKind::Integer {
                    _type: MIRIntegerType::I64,
                    signed,
                }
                .into(),
            )?;

            Ok(TypecheckResult::new_base(
                MIRTypeKind::Integer {
                    _type: MIRIntegerType::I1,
                    signed: false,
                }
                .into(),
                MIRExpressionKind::BinaryOperation {
                    op: MIRBinOp::Pointer { op: mir_op },
                    lhs: Box::new(pointer),
                    rhs: Box::new(coerced_val),
                },
            ))
        }

        _ => panic!("Invalid binary operation {op} for pointer type"),
    }
}

pub(crate) fn typecheck_ptr_ptr_binop(
    env: &mut TypeEnvironment,
    op: CXBinOp,
    lhs: MIRExpression,
    rhs: MIRExpression,
    expr: &CXExpr,
) -> CXResult<TypecheckResult> {
    let operator = match op {
        CXBinOp::LessEqual => MIRPtrBinOp::LE,
        CXBinOp::GreaterEqual => MIRPtrBinOp::GE,
        CXBinOp::Less => MIRPtrBinOp::LT,
        CXBinOp::Greater => MIRPtrBinOp::GT,
        CXBinOp::Equal => MIRPtrBinOp::EQ,
        CXBinOp::NotEqual => MIRPtrBinOp::NE,

        _ => {
            return log_typecheck_error!(
                env,
                expr.token_range(),
                "Invalid binary operation {op} for pointer types",
            );
        }
    };

    Ok(TypecheckResult::new_base(
        MIRTypeKind::Integer {
            _type: MIRIntegerType::I1,
            signed: false,
        }
        .into(),
        MIRExpressionKind::BinaryOperation {
            op: MIRBinOp::Pointer { op: operator },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}
