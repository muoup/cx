use cx_hmir::HMIRTypeOp;
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::EvalFrame,
    program::Program,
    staging_error,
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
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
