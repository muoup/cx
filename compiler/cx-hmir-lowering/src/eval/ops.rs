use cx_hmir::{HMIRBinaryOp, HMIRExprID, HMIRIntWidth, HMIRUnaryOp};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, eval, eval_global_type},
    program::Program,
    staging_error,
    ty::{TypeID, TypeKind},
    value::{
        FloatResult, StaticValue, arithmetic_type, float_value, fold_float, fold_int,
        is_comparison, is_logical, normalize_int,
    },
};

pub(crate) fn fold_binary(
    cx: &mut Program<'_>,
    op: HMIRBinaryOp,
    lhs: StaticValue,
    rhs: StaticValue,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let unsupported = |lhs: &StaticValue, rhs: &StaticValue| {
        staging_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            format!("'{}' on {} and {}", op.path(), lhs.describe(), rhs.describe()),
        )
    };
    match (&lhs, &rhs) {
        (
            StaticValue::Int {
                value: left,
                ty: left_ty,
            },
            StaticValue::Int {
                value: right,
                ty: right_ty,
            },
        ) => {
            if is_logical(op) {
                let result = fold_int(op, *left, *right, false).unwrap_or_default();
                return Ok(StaticValue::bool(result != 0, cx.types_mut()));
            }
            let ty = match op {
                HMIRBinaryOp::LShift | HMIRBinaryOp::RShift => {
                    arithmetic_type(cx.types_mut(), *left_ty, *left_ty)
                }
                _ => arithmetic_type(cx.types_mut(), *left_ty, *right_ty),
            }
            .unwrap_or(*left_ty);
            let signed = cx.types().is_signed(ty);
            let left = normalize_int(*left, ty, cx.types());
            let right = normalize_int(*right, ty, cx.types());
            let result = fold_int(op, left, right, signed)
                .ok_or_else(|| {
                    staging_error(span, &mir::COMPTIME_UNDEFINED_ARITHMETIC, op.path().into())
                })?;
            if is_comparison(op) {
                return Ok(StaticValue::bool(result != 0, cx.types_mut()));
            }
            Ok(StaticValue::int(normalize_int(result, ty, cx.types()), ty))
        }
        (StaticValue::Float { .. } | StaticValue::Int { .. }, StaticValue::Float { .. })
        | (StaticValue::Float { .. }, StaticValue::Int { .. }) => {
            let left_ty = lhs.simple_type(cx.types_mut()).expect("numeric");
            let right_ty = rhs.simple_type(cx.types_mut()).expect("numeric");
            let ty = arithmetic_type(cx.types_mut(), left_ty, right_ty)
                .ok_or_else(|| unsupported(&lhs, &rhs))?;
            let (Some(left), Some(right)) = (as_f64(&lhs), as_f64(&rhs)) else {
                return Err(unsupported(&lhs, &rhs));
            };
            match fold_float(op, left, right) {
                Some(FloatResult::Float(value)) => Ok(float_value(value, ty)),
                Some(FloatResult::Bool(value)) => Ok(StaticValue::bool(value, cx.types_mut())),
                None => Err(unsupported(&lhs, &rhs)),
            }
        }
        (StaticValue::Type(left), StaticValue::Type(right))
            if matches!(op, HMIRBinaryOp::Eq | HMIRBinaryOp::Neq) =>
        {
            let equal = left == right;
            Ok(StaticValue::bool(
                equal == (op == HMIRBinaryOp::Eq),
                cx.types_mut(),
            ))
        }
        (StaticValue::GlobalAddress { def, offset, ty }, StaticValue::Int { value, .. })
            if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
        {
            let element = cx
                .types()
                .pointer_inner(*ty)
                .ok_or_else(|| unsupported(&lhs, &rhs))?;
            let size = cx.types_mut().size_of(element, span)? as i64;
            let delta = size * *value as i64;
            Ok(StaticValue::GlobalAddress {
                def: *def,
                offset: if op == HMIRBinaryOp::Add {
                    offset + delta
                } else {
                    offset - delta
                },
                ty: *ty,
            })
        }
        (
            StaticValue::GlobalAddress {
                def: left_def,
                offset: left,
                ty,
            },
            StaticValue::GlobalAddress {
                def: right_def,
                offset: right,
                ..
            },
        ) if op == HMIRBinaryOp::Sub && left_def == right_def => {
            let element = cx
                .types()
                .pointer_inner(*ty)
                .ok_or_else(|| unsupported(&lhs, &rhs))?;
            let size = cx.types_mut().size_of(element, span)?.max(1) as i64;
            let ty = cx.types_mut().int(HMIRIntWidth::I64, true);
            Ok(StaticValue::int(((left - right) / size) as i128, ty))
        }
        _ => match (lhs.is_truthy(), rhs.is_truthy(), op) {
            (Some(left), Some(right), HMIRBinaryOp::Eq | HMIRBinaryOp::Neq)
                if (matches!(lhs, StaticValue::Null(_)) || matches!(rhs, StaticValue::Null(_))) =>
            {
                let equal = left == right;
                Ok(StaticValue::bool(
                    equal == (op == HMIRBinaryOp::Eq),
                    cx.types_mut(),
                ))
            }
            _ => Err(unsupported(&lhs, &rhs)),
        },
    }
}

pub(crate) fn exec_unary(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    op: HMIRUnaryOp,
    operand: HMIRExprID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    match op {
        HMIRUnaryOp::Neg | HMIRUnaryOp::BNot | HMIRUnaryOp::LNot => {
            let value = eval(cx, frame, operand, None)?;
            fold_unary(cx, op, value, span)
        }
        HMIRUnaryOp::PreIncrement
        | HMIRUnaryOp::PreDecrement
        | HMIRUnaryOp::PostIncrement
        | HMIRUnaryOp::PostDecrement => {
            let Some(local) = frame.as_local(operand) else {
                return Err(staging_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "incrementing a value that is not a local".into(),
                ));
            };
            let Some(StaticValue::Int { value, ty }) = frame.local(local).cloned() else {
                return Err(staging_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "incrementing a value that is not an integer".into(),
                ));
            };
            let delta = match op {
                HMIRUnaryOp::PreIncrement | HMIRUnaryOp::PostIncrement => 1,
                _ => -1,
            };
            let updated = StaticValue::int(normalize_int(value + delta, ty, cx.types()), ty);
            frame.bind(local, updated.clone());
            Ok(match op {
                HMIRUnaryOp::PreIncrement | HMIRUnaryOp::PreDecrement => updated,
                _ => StaticValue::int(value, ty),
            })
        }
    }
}

pub(crate) fn coerce_static(
    cx: &mut Program<'_>,
    value: StaticValue,
    ty: TypeID,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let kind = cx.types().kind(ty).clone();
    Ok(match (value, kind) {
        (StaticValue::Quote(quote), TypeKind::Expr { params, result }) => {
            let given = quote.get().params().len();
            if given != params.len() {
                return Err(staging_error(
                    span,
                    &typecheck::ARGUMENT_COUNT,
                    ("staged expression".into(), params.len(), given, false),
                ));
            }
            StaticValue::Quote(quote.with_result(result, cx.types().is_void(result)))
        }
        (value @ StaticValue::Type(_), TypeKind::Type) => value,
        (_, TypeKind::Void) => StaticValue::Unit,
        (StaticValue::Int { value, .. }, TypeKind::Int { width, .. }) => {
            if width == HMIRIntWidth::I1 {
                StaticValue::int((value != 0) as i128, ty)
            } else {
                StaticValue::int(normalize_int(value, ty, cx.types()), ty)
            }
        }
        (StaticValue::Int { value, .. }, TypeKind::Float { .. }) => float_value(value as f64, ty),
        (StaticValue::Float { value, .. }, TypeKind::Int { width, .. }) => {
            let value = f64::from(&value);
            if width == HMIRIntWidth::I1 {
                StaticValue::int((value != 0.0) as i128, ty)
            } else {
                StaticValue::int(normalize_int(value as i128, ty, cx.types()), ty)
            }
        }
        (StaticValue::Float { value, .. }, TypeKind::Float { .. }) => {
            StaticValue::Float { value, ty }
        }
        (StaticValue::Int { value: 0, .. } | StaticValue::Null(_), TypeKind::Pointer(_)) => {
            StaticValue::Null(ty)
        }
        (
            StaticValue::GlobalAddress { def, offset, .. },
            TypeKind::Pointer(_) | TypeKind::Reference(_),
        ) => StaticValue::GlobalAddress { def, offset, ty },
        (StaticValue::Global(def), TypeKind::Pointer(element)) => {
            let global = eval_global_type(cx, def, span)?;
            if cx.types().is_array(global) || global == element {
                StaticValue::GlobalAddress { def, offset: 0, ty }
            } else {
                return Err(staging_error(
                    span,
                    &typecheck::INVALID_CONVERSION,
                    ("a global".into(), cx.types().display(ty)),
                ));
            }
        }
        (
            value @ StaticValue::Str(_),
            TypeKind::Pointer(_) | TypeKind::Str | TypeKind::Array { .. },
        ) => value,
        (value @ StaticValue::Function { .. }, TypeKind::Pointer(_) | TypeKind::Function(_)) => {
            value
        }
        (value @ StaticValue::Aggregate { ty: source, .. }, TypeKind::Nominal(_))
            if cx.types_mut().same_unqualified(source, ty) =>
        {
            value
        }
        (StaticValue::Aggregate { ty: source, fields }, TypeKind::Array { element, length })
            if matches!(cx.types().kind(source), TypeKind::Array { element: from, length: count }
                if *from == element && (length == *count || length.is_none() || count.is_none())) =>
        {
            StaticValue::Aggregate { ty, fields }
        }
        (value, _) if value.simple_type(cx.types_mut()) == Some(ty) => value,
        (value, _) => {
            return Err(staging_error(
                span,
                &typecheck::INVALID_CONVERSION,
                (value.describe().into(), cx.types().display(ty)),
            ));
        }
    })
}

fn as_f64(value: &StaticValue) -> Option<f64> {
    match value {
        StaticValue::Int { value, .. } => Some(*value as f64),
        StaticValue::Float { value, .. } => Some(f64::from(value)),
        _ => None,
    }
}

pub(crate) fn fold_unary(
    cx: &mut Program<'_>,
    op: HMIRUnaryOp,
    value: StaticValue,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    if op == HMIRUnaryOp::LNot {
        let truthy = value
            .is_truthy()
            .ok_or_else(|| {
                staging_error(span, &mir::COMPTIME_NO_TRUTH_VALUE, value.describe().into())
            })?;
        return Ok(StaticValue::bool(!truthy, cx.types_mut()));
    }
    match value {
        StaticValue::Int { value, ty } if matches!(op, HMIRUnaryOp::Neg | HMIRUnaryOp::BNot) => {
            let ty = arithmetic_type(cx.types_mut(), ty, ty).unwrap_or(ty);
            let result = if op == HMIRUnaryOp::Neg {
                -value
            } else {
                !value
            };
            Ok(StaticValue::int(normalize_int(result, ty, cx.types()), ty))
        }
        StaticValue::Float { value, ty } if op == HMIRUnaryOp::Neg => {
            Ok(float_value(-f64::from(&value), ty))
        }
        other => Err(staging_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            format!("'{}' on {}", op.path(), other.describe()),
        )),
    }
}
