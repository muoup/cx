use cx_hmir::{HMIRBinaryOp, HMIRTypeOp, HMIRUnaryOp};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    program::Program,
    staging_error,
    ty::{TypeID, TypeKind, TypeTable},
    value::{StaticValue, arithmetic_type, is_comparison, is_logical},
};

pub(super) fn value_type(types: &TypeTable, ty: TypeID) -> TypeID {
    match types.kind(ty) {
        TypeKind::Reference(inner) => *inner,
        _ => ty,
    }
}

pub(super) fn decay(types: &mut TypeTable, ty: TypeID) -> TypeID {
    let ty = value_type(types, ty);
    match types.kind(ty).clone() {
        TypeKind::Array { element, .. } => types.pointer(element),
        TypeKind::Function(_) => types.pointer(ty),
        TypeKind::Str => types.char_pointer(),
        _ => ty,
    }
}

pub(super) fn exec_relation(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    op: &HMIRTypeOp,
    span: &TokenRange,
) -> CXResult<StaticValue> {
    let ty = match op {
        HMIRTypeOp::PointerInner(operand) | HMIRTypeOp::ReferenceInner(operand) => {
            let ty = program.eval_type(frame, *operand)?;
            match (op, program.types().kind(ty)) {
                (HMIRTypeOp::PointerInner(_), TypeKind::Pointer(inner))
                | (HMIRTypeOp::ReferenceInner(_), TypeKind::Reference(inner)) => *inner,
                _ => {
                    return Err(staging_error(
                        span,
                        format!(
                            "{} is invalid for '{}'",
                            op.path(),
                            program.types().display(ty)
                        ),
                    ));
                }
            }
        }
        HMIRTypeOp::Decay(operand) => {
            let ty = program.eval_type(frame, *operand)?;
            decay(program.types_mut(), ty)
        }
        _ => unreachable!("type relation expected"),
    };
    Ok(StaticValue::Type(ty))
}

pub(super) fn binary(
    program: &mut Program<'_>,
    op: HMIRBinaryOp,
    lhs: TypeID,
    rhs: TypeID,
    span: &TokenRange,
) -> CXResult<TypeID> {
    let lhs = decay(program.types_mut(), lhs);
    let rhs = decay(program.types_mut(), rhs);
    let ty = if is_comparison(op) || is_logical(op) {
        program.types_mut().bool()
    } else {
        match (program.types().kind(lhs), program.types().kind(rhs)) {
            (TypeKind::Pointer(_), TypeKind::Int { .. })
                if matches!(op, HMIRBinaryOp::Add | HMIRBinaryOp::Sub) =>
            {
                lhs
            }
            (TypeKind::Int { .. }, TypeKind::Pointer(_)) if op == HMIRBinaryOp::Add => rhs,
            (TypeKind::Pointer(_), TypeKind::Pointer(_)) if op == HMIRBinaryOp::Sub => {
                program.types_mut().int(cx_hmir::HMIRIntWidth::I64, true)
            }
            _ => {
                let rhs = if matches!(op, HMIRBinaryOp::LShift | HMIRBinaryOp::RShift) {
                    lhs
                } else {
                    rhs
                };
                arithmetic_type(program.types_mut(), lhs, rhs).ok_or_else(|| {
                    staging_error(
                        span,
                        format!(
                            "{} is invalid for '{}' and '{}'",
                            op.path(),
                            program.types().display(lhs),
                            program.types().display(rhs)
                        ),
                    )
                })?
            }
        }
    };
    Ok(ty)
}

pub(super) fn unary(
    program: &mut Program<'_>,
    op: HMIRUnaryOp,
    ty: TypeID,
    span: &TokenRange,
) -> CXResult<TypeID> {
    let ty = match op {
        HMIRUnaryOp::LNot => program.types_mut().bool(),
        HMIRUnaryOp::Neg | HMIRUnaryOp::BNot
            if op != HMIRUnaryOp::BNot || program.types().int_info(ty).is_some() =>
        {
            arithmetic_type(program.types_mut(), ty, ty).ok_or_else(|| {
                staging_error(
                    span,
                    format!(
                        "{} is invalid for '{}'",
                        op.path(),
                        program.types().display(ty)
                    ),
                )
            })?
        }
        HMIRUnaryOp::BNot => {
            return Err(staging_error(
                span,
                format!(
                    "{} is invalid for '{}'",
                    op.path(),
                    program.types().display(ty)
                ),
            ));
        }
        _ => ty,
    };
    Ok(ty)
}

pub(super) fn common(
    program: &mut Program<'_>,
    lhs: TypeID,
    rhs: TypeID,
    span: &TokenRange,
) -> CXResult<TypeID> {
    let ty = if lhs == rhs || program.types().is_unreachable(rhs) {
        lhs
    } else if program.types().is_unreachable(lhs) {
        rhs
    } else {
        let lhs = decay(program.types_mut(), lhs);
        let rhs = decay(program.types_mut(), rhs);
        match (program.types().kind(lhs), program.types().kind(rhs)) {
            (TypeKind::Pointer(left), TypeKind::Pointer(right))
                if left == right || program.types().is_void(*left) =>
            {
                lhs
            }
            (TypeKind::Pointer(_), TypeKind::Pointer(right)) if program.types().is_void(*right) => {
                rhs
            }
            _ => arithmetic_type(program.types_mut(), lhs, rhs).ok_or_else(|| {
                staging_error(
                    span,
                    format!(
                        "'{}' and '{}' have no common type",
                        program.types().display(lhs),
                        program.types().display(rhs)
                    ),
                )
            })?,
        }
    };
    Ok(ty)
}
