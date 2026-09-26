use cx_hir::ast::expression::HIRBinOp;
use cx_log::CXResult;
use cx_log::catalogue::typecheck as catalogue;
use cx_thir::{
    thir::{
        expression::{
            THIRBinOp, THIRBlockKind, THIRExpression, THIRExpressionKind, THIRFloatBinOp, THIRIntBinOp,
            THIRPtrBinOp, THIRPtrDiffBinOp,
        },
        r#type::{THIRIntType, THIRType, THIRTypeKind},
    },
    type_context::THIRTypeContext,
};

use crate::{
    environment::TypeEnvironment,
    type_checking::{
        coercion::implicit::{implicit_cast, promotion::std_rval_promotion},
        result::TypecheckResult,
    },
};

pub(crate) mod access;
pub(crate) mod assign;
pub(crate) mod calls;
pub(crate) mod is;

pub(crate) fn dispatch(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    lhs: THIRExpression,
    rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    match &op {
        HIRBinOp::LOr | HIRBinOp::LAnd => resolve_logical(env, op, lhs, rhs),
        HIRBinOp::Comma => resolve_comma(lhs, rhs),

        _ => resolve_std_arithmetic(env, op, lhs, rhs),
    }
}

fn resolve_comma(lhs: THIRExpression, rhs: THIRExpression) -> CXResult<TypecheckResult> {
    Ok(TypecheckResult::new(
        rhs.ty.clone(),
        THIRExpressionKind::Block {
            statements: vec![lhs, rhs],
            kind: THIRBlockKind::Sequence,
            yields: false,
        },
    ))
}

pub(crate) fn resolve_logical(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    lhs = std_rval_promotion(env, lhs)?;
    rhs = std_rval_promotion(env, rhs)?;

    let valid_logical_operand =
        |expr: &THIRExpression| expr.ty.is_integer() || expr.ty.is_float() || expr.ty.is_pointer();

    if !valid_logical_operand(&lhs) || !valid_logical_operand(&rhs) {
        return env.log_error(
            &lhs.token_range,
            &catalogue::INVALID_BINARY_OPERANDS,
            (
                format!("{:?}", op),
                format!("{}", lhs.ty.display_with(&env.symbols)),
                format!("{}", rhs.ty.display_with(&env.symbols)),
            ),
        );
    }

    let lhs = implicit_cast(env, lhs, &THIRType::bool())?;
    let rhs = implicit_cast(env, rhs, &THIRType::bool())?;

    let operator = THIRBinOp::Integer {
        itype: THIRIntType::I1,
        op: match op {
            HIRBinOp::LAnd => THIRIntBinOp::LAND,
            HIRBinOp::LOr => THIRIntBinOp::LOR,
            _ => unreachable!(),
        },
    };

    Ok(TypecheckResult::new(
        THIRType::bool(),
        THIRExpressionKind::BinaryOperation {
            op: operator,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

pub(crate) fn resolve_std_arithmetic(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    lhs = std_rval_promotion(env, lhs)?;
    rhs = std_rval_promotion(env, rhs)?;

    if lhs.ty.is_float() || rhs.ty.is_float() {
        coerce_float_binop(env, op, lhs, rhs)
    } else if lhs.ty.is_pointer() || rhs.ty.is_pointer() {
        coerce_pointer_binop(env, op, lhs, rhs)
    } else if lhs.ty.is_integer() && rhs.ty.is_integer() {
        coerce_integral_binop(env, op, lhs, rhs)
    } else {
        env.log_error(
            &lhs.token_range,
            &catalogue::INVALID_BINARY_OPERANDS,
            (
                format!("{}", op),
                format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                format!("{}", rhs.ty.clone().display_with(&env.symbols)),
            ),
        )
    }
}

fn coerce_float_binop(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    if let THIRTypeKind::Float { ty: lftype } = lhs.ty.kind
        && let THIRTypeKind::Float { ty: rftype } = rhs.ty.kind
        && lftype != rftype
    {
        let common_ftype = if lftype.bytes() > rftype.bytes() {
            lhs.ty.clone()
        } else {
            rhs.ty.clone()
        };

        rhs = implicit_cast(env, rhs, &common_ftype)?;
    }

    if !rhs.ty.is_float() {
        rhs = implicit_cast(env, rhs, &lhs.ty)?;
    } else {
        lhs = implicit_cast(env, lhs, &rhs.ty)?;
    }

    let (op, return_type) = match op {
        HIRBinOp::Add => (THIRFloatBinOp::FADD, lhs.ty.clone()),
        HIRBinOp::Subtract => (THIRFloatBinOp::FSUB, lhs.ty.clone()),
        HIRBinOp::Multiply => (THIRFloatBinOp::FMUL, lhs.ty.clone()),
        HIRBinOp::Divide => (THIRFloatBinOp::FDIV, lhs.ty.clone()),

        HIRBinOp::Equal => (THIRFloatBinOp::FEQ, THIRType::bool()),
        HIRBinOp::NotEqual => (THIRFloatBinOp::FNE, THIRType::bool()),
        HIRBinOp::Less => (THIRFloatBinOp::FLT, THIRType::bool()),
        HIRBinOp::Greater => (THIRFloatBinOp::FGT, THIRType::bool()),
        HIRBinOp::LessEqual => (THIRFloatBinOp::FLE, THIRType::bool()),
        HIRBinOp::GreaterEqual => (THIRFloatBinOp::FGE, THIRType::bool()),

        _ => {
            return env.log_error(
                &lhs.token_range,
                &catalogue::INVALID_BINARY_OPERANDS,
                (
                    format!("{}", op),
                    format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                    format!("{}", rhs.ty.clone().display_with(&env.symbols)),
                ),
            );
        }
    };

    Ok(TypecheckResult::new(
        return_type,
        THIRExpressionKind::BinaryOperation {
            op: THIRBinOp::Float {
                ftype: match lhs.ty.kind {
                    THIRTypeKind::Float { ty } => ty,
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
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    if lhs.ty.is_pointer() && rhs.ty.is_pointer() {
        if *op == HIRBinOp::Subtract {
            let pointer_integer = env.symbols.pointer_integer_type();
            let integer_type: THIRType = THIRTypeKind::Integer {
                ty: pointer_integer,
                signed: true,
            }
            .into();
            let pointee = env.symbols.ptr_inner(&lhs.ty).cloned().unwrap();
            let element_ty = env.symbols.generate_type_id(pointee);
            return Ok(TypecheckResult::new(
                integer_type,
                THIRExpressionKind::BinaryOperation {
                    op: THIRBinOp::PtrDifference { element_ty },
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                },
            ));
        }

        let (return_type, op) = match op {
            HIRBinOp::LessEqual => (THIRType::bool(), THIRPtrBinOp::LE),
            HIRBinOp::GreaterEqual => (THIRType::bool(), THIRPtrBinOp::GE),
            HIRBinOp::Less => (THIRType::bool(), THIRPtrBinOp::LT),
            HIRBinOp::Greater => (THIRType::bool(), THIRPtrBinOp::GT),
            HIRBinOp::Equal => (THIRType::bool(), THIRPtrBinOp::EQ),
            HIRBinOp::NotEqual => (THIRType::bool(), THIRPtrBinOp::NE),

            _ => {
                return env.log_error(
                    &lhs.token_range,
                    &catalogue::INVALID_BINARY_OPERANDS,
                    (
                        format!("{}", op),
                        format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                        format!("{}", rhs.ty.clone().display_with(&env.symbols)),
                    )
                );
            }
        };

        return Ok(TypecheckResult::new(
            return_type,
            THIRExpressionKind::BinaryOperation {
                op: THIRBinOp::Pointer { op },
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
            },
        ));
    }

    if matches!(op, HIRBinOp::Subtract) && !lhs.ty.is_pointer() {
        return env.log_error(
            &lhs.token_range,
            &catalogue::INVALID_BINARY_OPERANDS,
            (
                format!("{}", op),
                format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                format!("{}", rhs.ty.clone().display_with(&env.symbols)),
            ),
        );
    }

    let (pointer, non_pointer) = if lhs.ty.is_pointer() {
        (&mut lhs, &mut rhs)
    } else {
        (&mut rhs, &mut lhs)
    };

    let ptr_type = pointer.ty.clone();
    if matches!(op, HIRBinOp::Equal | HIRBinOp::NotEqual)
        && matches!(non_pointer.kind, THIRExpressionKind::IntLiteral(0))
    {
        *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &ptr_type)?;
    } else {
        let intptr = THIRTypeKind::Integer {
            ty: env.symbols.pointer_integer_type(),
            signed: true,
        };
        *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &intptr.into())?;
    }

    let ptr_inner = env.symbols.ptr_inner(&ptr_type).cloned().unwrap();
    let ptr_inner_id = env.symbols.generate_type_id(ptr_inner.clone());

    let (return_type, op) = match op {
        HIRBinOp::Add => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner: ptr_inner_id,
            },
        ),

        HIRBinOp::ArrayIndex => (
            env.symbols.mem_ref_to(ptr_inner),
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner: ptr_inner_id,
            },
        ),

        HIRBinOp::Subtract => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::SUB,
                ptr_inner: ptr_inner_id,
            },
        ),

        HIRBinOp::LessEqual
        | HIRBinOp::GreaterEqual
        | HIRBinOp::Less
        | HIRBinOp::Greater
        | HIRBinOp::Equal
        | HIRBinOp::NotEqual => (
            THIRType::bool(),
            THIRBinOp::Pointer {
                op: match op {
                    HIRBinOp::LessEqual => THIRPtrBinOp::LE,
                    HIRBinOp::GreaterEqual => THIRPtrBinOp::GE,
                    HIRBinOp::Less => THIRPtrBinOp::LT,
                    HIRBinOp::Greater => THIRPtrBinOp::GT,
                    HIRBinOp::Equal => THIRPtrBinOp::EQ,
                    HIRBinOp::NotEqual => THIRPtrBinOp::NE,
                    _ => unreachable!(),
                },
            },
        ),

        _ => {
            return env.log_error(
                &lhs.token_range,
                &catalogue::INVALID_BINARY_OPERANDS,
                (
                    format!("{}", op),
                    format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                    format!("{}", rhs.ty.clone().display_with(&env.symbols)),
                )
            );
        }
    };

    if matches!(op, THIRBinOp::PtrDiff { .. }) && rhs.ty.is_pointer() {
        std::mem::swap(&mut lhs, &mut rhs);
    }

    Ok(TypecheckResult::new(
        return_type,
        THIRExpressionKind::BinaryOperation {
            op,
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn coerce_integral_binop(
    env: &mut TypeEnvironment,
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    let THIRTypeKind::Integer { ty: litype, .. } = lhs.ty.kind else {
        unreachable!("Expected integer type for lhs of integral binary operation");
    };
    let THIRTypeKind::Integer { ty: ritype, .. } = rhs.ty.kind else {
        unreachable!("Expected integer type for rhs of integral binary operation");
    };

    if litype.rank() < ritype.rank() {
        lhs = implicit_cast(env, lhs, &rhs.ty)?;
    } else if ritype.rank() < litype.rank() {
        rhs = implicit_cast(env, rhs, &lhs.ty)?;
    }

    let return_type = match op {
        HIRBinOp::Add
        | HIRBinOp::Subtract
        | HIRBinOp::Multiply
        | HIRBinOp::Divide
        | HIRBinOp::Modulus
        | HIRBinOp::BitAnd
        | HIRBinOp::BitOr
        | HIRBinOp::BitXor
        | HIRBinOp::LShift
        | HIRBinOp::RShift => lhs.ty.clone(),

        HIRBinOp::Less
        | HIRBinOp::Greater
        | HIRBinOp::LessEqual
        | HIRBinOp::GreaterEqual
        | HIRBinOp::Equal
        | HIRBinOp::NotEqual => THIRType::bool(),

        _ => {
            return env.log_error(
                &lhs.token_range,
                &catalogue::INVALID_BINARY_OPERANDS,
                (
                    format!("{}", op),
                    format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                    format!("{}", rhs.ty.clone().display_with(&env.symbols)),
                ),
            );
        }
    };

    let signed = match lhs.ty.kind {
        THIRTypeKind::Integer { signed, .. } => signed,
        _ => unreachable!(),
    };

    let Some(op) = lower_int_binop(op, signed) else {
        return env.log_error(
            &lhs.token_range,
            &catalogue::INVALID_BINARY_OPERANDS,
            (
                format!("{}", op),
                format!("{}", lhs.ty.clone().display_with(&env.symbols)),
                format!("{}", rhs.ty.clone().display_with(&env.symbols)),
            ),
        );
    };

    Ok(TypecheckResult::new(
        return_type,
        THIRExpressionKind::BinaryOperation {
            op: THIRBinOp::Integer {
                itype: match lhs.ty.kind {
                    THIRTypeKind::Integer { ty, .. } => ty,
                    _ => unreachable!(),
                },
                op,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn lower_int_binop(op: &HIRBinOp, signed: bool) -> Option<THIRIntBinOp> {
    Some(match op {
        HIRBinOp::Add => THIRIntBinOp::ADD,
        HIRBinOp::Subtract => THIRIntBinOp::SUB,
        HIRBinOp::Multiply if signed => THIRIntBinOp::IMUL,
        HIRBinOp::Multiply => THIRIntBinOp::MUL,
        HIRBinOp::Divide if signed => THIRIntBinOp::IDIV,
        HIRBinOp::Divide => THIRIntBinOp::DIV,
        HIRBinOp::Modulus if signed => THIRIntBinOp::IMOD,
        HIRBinOp::Modulus => THIRIntBinOp::MOD,

        HIRBinOp::Less if !signed => THIRIntBinOp::LT,
        HIRBinOp::Less if signed => THIRIntBinOp::ILT,

        HIRBinOp::Greater if !signed => THIRIntBinOp::GT,
        HIRBinOp::Greater if signed => THIRIntBinOp::IGT,

        HIRBinOp::LessEqual if !signed => THIRIntBinOp::LE,
        HIRBinOp::LessEqual if signed => THIRIntBinOp::ILE,

        HIRBinOp::GreaterEqual if !signed => THIRIntBinOp::GE,
        HIRBinOp::GreaterEqual if signed => THIRIntBinOp::IGE,

        HIRBinOp::Equal => THIRIntBinOp::EQ,
        HIRBinOp::NotEqual => THIRIntBinOp::NE,

        HIRBinOp::BitAnd => THIRIntBinOp::BAND,
        HIRBinOp::BitOr => THIRIntBinOp::BOR,
        HIRBinOp::BitXor => THIRIntBinOp::BXOR,
        HIRBinOp::LShift => THIRIntBinOp::SHL,
        HIRBinOp::RShift if signed => THIRIntBinOp::ASHR,
        HIRBinOp::RShift => THIRIntBinOp::LSHR,

        _ => return None,
    })
}
