use cx_ast::ast::expression::CXBinOp;
use cx_log::CXResult;
use cx_thir::{
    thir::{
        expression::{
            THIRBinOp, THIRExpression, THIRExpressionKind, THIRFloatBinOp, THIRIntBinOp,
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
    op: &CXBinOp,
    lhs: THIRExpression,
    rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    match &op {
        CXBinOp::LOr | CXBinOp::LAnd => resolve_logical(env, op, lhs, rhs),

        _ => resolve_std_arithmetic(env, op, lhs, rhs),
    }
}

pub(crate) fn resolve_logical(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    lhs = std_rval_promotion(env, lhs)?;
    rhs = std_rval_promotion(env, rhs)?;

    let valid_logical_operand = |expr: &THIRExpression| {
        expr._type.is_integer() || expr._type.is_float() || expr._type.is_pointer()
    };

    if !valid_logical_operand(&lhs) || !valid_logical_operand(&rhs) {
        return env.log_error(
            &lhs.token_range,
            format!(
                "Invalid operands to logical operation {:?}, {} and {}",
                op,
                lhs._type.display_with(&env.symbols),
                rhs._type.display_with(&env.symbols)
            ),
        );
    }

    let lhs = implicit_cast(env, lhs, &THIRType::bool())?;
    let rhs = implicit_cast(env, rhs, &THIRType::bool())?;

    let operator = THIRBinOp::Integer {
        itype: THIRIntType::I1,
        op: match op {
            CXBinOp::LAnd => THIRIntBinOp::LAND,
            CXBinOp::LOr => THIRIntBinOp::LOR,
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
    op: &CXBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
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
        env.log_error(
            &lhs.token_range,
            format!(
                "Invalid binary operation {op} for types {} and {}",
                lhs.get_type().display_with(&env.symbols),
                rhs.get_type().display_with(&env.symbols)
            ),
        )
    }
}

fn coerce_float_binop(
    env: &mut TypeEnvironment,
    op: &CXBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    if let THIRTypeKind::Float { _type: lftype } = lhs._type.kind
        && let THIRTypeKind::Float { _type: rftype } = rhs._type.kind
        && lftype != rftype
    {
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
        CXBinOp::Add => (THIRFloatBinOp::FADD, lhs._type.clone()),
        CXBinOp::Subtract => (THIRFloatBinOp::FSUB, lhs._type.clone()),
        CXBinOp::Multiply => (THIRFloatBinOp::FMUL, lhs._type.clone()),
        CXBinOp::Divide => (THIRFloatBinOp::FDIV, lhs._type.clone()),

        CXBinOp::Equal => (THIRFloatBinOp::FEQ, THIRType::bool()),
        CXBinOp::NotEqual => (THIRFloatBinOp::FNE, THIRType::bool()),
        CXBinOp::Less => (THIRFloatBinOp::FLT, THIRType::bool()),
        CXBinOp::Greater => (THIRFloatBinOp::FGT, THIRType::bool()),
        CXBinOp::LessEqual => (THIRFloatBinOp::FLE, THIRType::bool()),
        CXBinOp::GreaterEqual => (THIRFloatBinOp::FGE, THIRType::bool()),

        _ => {
            return env.log_error(
                &lhs.token_range,
                format!(
                    "Invalid float binary operation {op} for types {} and {}",
                    lhs.get_type().display_with(&env.symbols),
                    rhs.get_type().display_with(&env.symbols)
                ),
            );
        }
    };

    Ok(TypecheckResult::new(
        return_type,
        THIRExpressionKind::BinaryOperation {
            op: THIRBinOp::Float {
                ftype: match lhs._type.kind {
                    THIRTypeKind::Float { _type } => _type,
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
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    if lhs._type.is_pointer() && rhs._type.is_pointer() {
        let (return_type, op) = match op {
            CXBinOp::LessEqual => (THIRType::bool(), THIRPtrBinOp::LE),
            CXBinOp::GreaterEqual => (THIRType::bool(), THIRPtrBinOp::GE),
            CXBinOp::Less => (THIRType::bool(), THIRPtrBinOp::LT),
            CXBinOp::Greater => (THIRType::bool(), THIRPtrBinOp::GT),
            CXBinOp::Equal => (THIRType::bool(), THIRPtrBinOp::EQ),
            CXBinOp::NotEqual => (THIRType::bool(), THIRPtrBinOp::NE),

            _ => {
                return env.log_error(
                    &lhs.token_range,
                    format!("Invalid binary operation {op} for pointer types"),
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

    let (pointer, non_pointer) = if lhs._type.is_pointer() {
        (&mut lhs, &mut rhs)
    } else {
        (&mut rhs, &mut lhs)
    };

    let intptr = THIRTypeKind::Integer {
        _type: env.symbols.pointer_integer_type(),
        signed: true,
    };

    *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &intptr.into())?;

    let ptr_type = pointer._type.clone();
    let ptr_inner = Box::new(env.symbols.ptr_inner(&ptr_type).cloned().unwrap());

    let (return_type, op) = match op {
        CXBinOp::Add => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        CXBinOp::ArrayIndex => (
            env.symbols.mem_ref_to(ptr_inner.as_ref().clone()),
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        CXBinOp::Subtract => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::SUB,
                ptr_inner,
            },
        ),

        CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::Equal
        | CXBinOp::NotEqual => (
            THIRType::bool(),
            THIRBinOp::Pointer {
                op: match op {
                    CXBinOp::LessEqual => THIRPtrBinOp::LE,
                    CXBinOp::GreaterEqual => THIRPtrBinOp::GE,
                    CXBinOp::Less => THIRPtrBinOp::LT,
                    CXBinOp::Greater => THIRPtrBinOp::GT,
                    CXBinOp::Equal => THIRPtrBinOp::EQ,
                    CXBinOp::NotEqual => THIRPtrBinOp::NE,
                    _ => unreachable!(),
                },
            },
        ),

        _ => {
            return env.log_error(
                &lhs.token_range,
                format!("Invalid binary operation {op} for pointer and non-pointer types"),
            );
        }
    };

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
    op: &CXBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    let THIRTypeKind::Integer { _type: litype, .. } = lhs._type.kind else {
        unreachable!("Expected integer type for lhs of integral binary operation");
    };
    let THIRTypeKind::Integer { _type: ritype, .. } = rhs._type.kind else {
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
        | CXBinOp::BitXor
        | CXBinOp::LShift
        | CXBinOp::RShift => lhs._type.clone(),

        CXBinOp::Less
        | CXBinOp::Greater
        | CXBinOp::LessEqual
        | CXBinOp::GreaterEqual
        | CXBinOp::Equal
        | CXBinOp::NotEqual => THIRType::bool(),

        _ => {
            return env.log_error(
                &lhs.token_range,
                format!(
                    "Invalid integer binary operation {op} for types {} and {}",
                    lhs.get_type().display_with(&env.symbols),
                    rhs.get_type().display_with(&env.symbols)
                ),
            );
        }
    };

    let signed = match lhs._type.kind {
        THIRTypeKind::Integer { signed, .. } => signed,
        _ => unreachable!(),
    };
    
    let Some(op) = lower_int_binop(op, signed) else {
        return env.log_error(
            &lhs.token_range,
            format!(
                "Invalid integer binary operation {op} for types {} and {}",
                lhs.get_type().display_with(&env.symbols),
                rhs.get_type().display_with(&env.symbols)
            ),
        );
    };

    Ok(TypecheckResult::new(
        return_type,
        THIRExpressionKind::BinaryOperation {
            op: THIRBinOp::Integer {
                itype: match lhs._type.kind {
                    THIRTypeKind::Integer { _type, .. } => _type,
                    _ => unreachable!(),
                },
                op,
            },
            lhs: Box::new(lhs),
            rhs: Box::new(rhs),
        },
    ))
}

fn lower_int_binop(op: &CXBinOp, signed: bool) -> Option<THIRIntBinOp> {
    Some(match op {
        CXBinOp::Add => THIRIntBinOp::ADD,
        CXBinOp::Subtract => THIRIntBinOp::SUB,
        CXBinOp::Multiply => THIRIntBinOp::MUL,
        CXBinOp::Divide => THIRIntBinOp::DIV,
        CXBinOp::Modulus => THIRIntBinOp::MOD,

        CXBinOp::Less if !signed => THIRIntBinOp::LT,
        CXBinOp::Less if signed => THIRIntBinOp::ILT,

        CXBinOp::Greater if !signed => THIRIntBinOp::GT,
        CXBinOp::Greater if signed => THIRIntBinOp::IGT,

        CXBinOp::LessEqual if !signed => THIRIntBinOp::LE,
        CXBinOp::LessEqual if signed => THIRIntBinOp::ILE,

        CXBinOp::GreaterEqual if !signed => THIRIntBinOp::GE,
        CXBinOp::GreaterEqual if signed => THIRIntBinOp::IGE,

        CXBinOp::Equal => THIRIntBinOp::EQ,
        CXBinOp::NotEqual => THIRIntBinOp::NE,

        CXBinOp::BitAnd => THIRIntBinOp::BAND,
        CXBinOp::BitOr => THIRIntBinOp::BOR,
        CXBinOp::BitXor => THIRIntBinOp::BXOR,
        CXBinOp::LShift => THIRIntBinOp::SHL,
        CXBinOp::RShift if signed => THIRIntBinOp::ASHR,
        CXBinOp::RShift => THIRIntBinOp::LSHR,

        _ => return None,
    })
}
