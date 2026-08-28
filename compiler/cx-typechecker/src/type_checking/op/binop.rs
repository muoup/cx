use cx_hir::ast::expression::HIRBinOp;
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
        rhs._type.clone(),
        THIRExpressionKind::Block {
            statements: vec![lhs, rhs],
            creates_scope: false,
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
    op: &HIRBinOp,
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
        HIRBinOp::Add => (THIRFloatBinOp::FADD, lhs._type.clone()),
        HIRBinOp::Subtract => (THIRFloatBinOp::FSUB, lhs._type.clone()),
        HIRBinOp::Multiply => (THIRFloatBinOp::FMUL, lhs._type.clone()),
        HIRBinOp::Divide => (THIRFloatBinOp::FDIV, lhs._type.clone()),

        HIRBinOp::Equal => (THIRFloatBinOp::FEQ, THIRType::bool()),
        HIRBinOp::NotEqual => (THIRFloatBinOp::FNE, THIRType::bool()),
        HIRBinOp::Less => (THIRFloatBinOp::FLT, THIRType::bool()),
        HIRBinOp::Greater => (THIRFloatBinOp::FGT, THIRType::bool()),
        HIRBinOp::LessEqual => (THIRFloatBinOp::FLE, THIRType::bool()),
        HIRBinOp::GreaterEqual => (THIRFloatBinOp::FGE, THIRType::bool()),

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
    op: &HIRBinOp,
    mut lhs: THIRExpression,
    mut rhs: THIRExpression,
) -> CXResult<TypecheckResult> {
    if lhs._type.is_pointer() && rhs._type.is_pointer() {
        if *op == HIRBinOp::Subtract {
            let pointer_integer = env.symbols.pointer_integer_type();
            let integer_type: THIRType = THIRTypeKind::Integer {
                _type: pointer_integer,
                signed: true,
            }
            .into();
            let pointee = env.symbols.ptr_inner(&lhs._type).cloned().unwrap();
            let difference_range = lhs.token_range.clone();
            let pointer_to_integer = |operand: THIRExpression| THIRExpression {
                token_range: operand.token_range.clone(),
                kind: THIRExpressionKind::TypeConversion {
                    operand: Box::new(operand),
                    conversion: cx_thir::thir::expression::THIRCoercion::PtrToInt {
                        to_type: pointer_integer,
                    },
                },
                _type: integer_type.clone(),
            };
            let difference = THIRExpression {
                token_range: difference_range.clone(),
                kind: THIRExpressionKind::BinaryOperation {
                    op: THIRBinOp::Integer {
                        itype: pointer_integer,
                        op: THIRIntBinOp::IDIV,
                    },
                    lhs: Box::new(THIRExpression {
                        token_range: difference_range.clone(),
                        kind: THIRExpressionKind::BinaryOperation {
                            op: THIRBinOp::Integer {
                                itype: pointer_integer,
                                op: THIRIntBinOp::SUB,
                            },
                            lhs: Box::new(pointer_to_integer(lhs)),
                            rhs: Box::new(pointer_to_integer(rhs)),
                        },
                        _type: integer_type.clone(),
                    }),
                    rhs: Box::new(THIRExpression {
                        token_range: difference_range.clone(),
                        kind: THIRExpressionKind::SizeOf { _type: pointee },
                        _type: integer_type.clone(),
                    }),
                },
                _type: integer_type.clone(),
            };

            return Ok(TypecheckResult::from(difference));
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

    let ptr_type = pointer._type.clone();
    if matches!(op, HIRBinOp::Equal | HIRBinOp::NotEqual)
        && matches!(non_pointer.kind, THIRExpressionKind::IntLiteral(0))
    {
        *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &ptr_type)?;
    } else {
        let intptr = THIRTypeKind::Integer {
            _type: env.symbols.pointer_integer_type(),
            signed: true,
        };
        *non_pointer = implicit_cast(env, std::mem::take(non_pointer), &intptr.into())?;
    }

    let ptr_inner = Box::new(env.symbols.ptr_inner(&ptr_type).cloned().unwrap());

    let (return_type, op) = match op {
        HIRBinOp::Add => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        HIRBinOp::ArrayIndex => (
            env.symbols.mem_ref_to(ptr_inner.as_ref().clone()),
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::ADD,
                ptr_inner,
            },
        ),

        HIRBinOp::Subtract => (
            ptr_type,
            THIRBinOp::PtrDiff {
                op: THIRPtrDiffBinOp::SUB,
                ptr_inner,
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
    op: &HIRBinOp,
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
        HIRBinOp::Add
        | HIRBinOp::Subtract
        | HIRBinOp::Multiply
        | HIRBinOp::Divide
        | HIRBinOp::Modulus
        | HIRBinOp::BitAnd
        | HIRBinOp::BitOr
        | HIRBinOp::BitXor
        | HIRBinOp::LShift
        | HIRBinOp::RShift => lhs._type.clone(),

        HIRBinOp::Less
        | HIRBinOp::Greater
        | HIRBinOp::LessEqual
        | HIRBinOp::GreaterEqual
        | HIRBinOp::Equal
        | HIRBinOp::NotEqual => THIRType::bool(),

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
