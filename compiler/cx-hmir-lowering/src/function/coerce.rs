use cx_hmir::{HMIRCoerceMode, HMIRExprID};
use cx_mir::{MIRInternalIntrinsic, MIRTarget};
use cx_tokens::TokenRange;

use crate::{
    function::{Expect, FunctionLowering, Lower, Operand},
    ty::TypeKind,
};

pub(super) fn coerce(
    lowering: &mut FunctionLowering<'_, '_>,
    frame: usize,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
    span: &TokenRange,
) -> Lower<Operand> {
    if mode == HMIRCoerceMode::Truthy {
        let value = lowering.expr(frame, value, Expect::Any)?;
        return lowering.truthy(value, span);
    }
    let ty = lowering.eval_type(frame, target)?;
    let reference = match lowering.program.types().kind(ty) {
        TypeKind::Reference(inner) => Some(*inner),
        _ => None,
    };
    let expect = if reference.is_some() {
        Expect::Any
    } else {
        Expect::Type(ty)
    };
    let value = lowering.expr(frame, value, expect)?;
    let value = if mode == HMIRCoerceMode::CCast
        && reference.is_some_and(|inner| inner != value.ty())
        && matches!(
            lowering.program.types().kind(value.ty()),
            TypeKind::Pointer(_)
        ) {
        let value = lowering.value(value, span)?;
        let out = lowering.register(ty, span)?;
        let target_ty = lowering.mir(ty, span)?;
        lowering.intrinsic(
            MIRInternalIntrinsic::Bitcast {
                out: MIRTarget::Register(out),
                value,
                target_ty,
            },
            span,
        );
        Operand::register(out, ty)
    } else {
        lowering.convert(value, ty, span)?
    };
    if let Some(inner) = reference {
        if matches!(lowering.program.types().kind(inner), TypeKind::Function(_)) {
            let pointer = lowering.program.types_mut().pointer(inner);
            return Ok(value.with_type(pointer));
        }
        return lowering.auto_deref(value, span);
    }
    Ok(value)
}
