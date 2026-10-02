use cx_mir::{MIRInternalIntrinsic, MIRTarget};
use cx_tokens::TokenRange;

use crate::{
    function::{
        FunctionLowering, LowerResult, Operand, OperandKind,
        coerce::lower_convert,
        operand::{lower_auto_deref, lower_copy, lower_spill, lower_value},
    },
    module::declare_function,
    ty::{TypeID, TypeKind},
    value::{StaticValue, promote_integer_type},
};

// The value an operator works on: references are read through, storage decays, lvalues are
// copied and small integers widen
pub(super) fn lower_promote(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let operand = lower_auto_deref(cx, operand, span)?;
    let operand = lower_decay(cx, operand, span)?;
    let operand = if operand.is_lvalue() {
        if !cx.program.types().is_pod(operand.ty()) {
            return cx.error(
                span,
                format!("cannot copy '{}'", cx.program.types().display(operand.ty())),
            );
        }
        Operand::value(lower_copy(cx, &operand, span)?, operand.ty())
    } else {
        operand
    };
    let ty = promote_integer_type(cx.program.types_mut(), operand.ty());
    lower_convert(cx, operand, ty, span)
}

// Arrays, strings and functions used as values become pointers; see 'TypeTable::decayed'
pub(super) fn lower_decay(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let ty = cx.program.types_mut().decayed(operand.ty());
    match cx.program.types().kind(operand.ty()) {
        TypeKind::Array { .. } => lower_array_to_pointer(cx, operand, ty, span),
        TypeKind::Str => lower_str_to_pointer(cx, operand, ty, span),
        TypeKind::Function(_) => lower_function_to_pointer(cx, operand, ty, span),
        _ => Ok(operand),
    }
}

fn lower_array_to_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    ty: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let operand = lower_spill(cx, operand, span)?;
    let out = cx.register(ty, span)?;
    let array = operand.address().expect("spilled operand is addressable");
    cx.intrinsic(
        MIRInternalIntrinsic::ArrayAddress {
            out: MIRTarget::Register(out),
            array,
        },
        span,
    );
    Ok(Operand::register(out, ty).with_pointee_origin(operand.origin()))
}

// A literal is addressed directly; any other string is only reachable through a 'str&'
fn lower_str_to_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    ty: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if operand.as_static().is_some() {
        let value = lower_value(cx, operand.with_type(ty), span)?;
        return Ok(Operand::value(value, ty));
    }
    let Some(reference) = operand.address() else {
        return cx.error(span, "string value has no storage to point at");
    };
    let out = cx.register(ty, span)?;
    cx.intrinsic(
        MIRInternalIntrinsic::ReferenceAddress {
            out: MIRTarget::Register(out),
            reference,
        },
        span,
    );
    Ok(Operand::register(out, ty))
}

fn lower_function_to_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    ty: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if cx.unevaluated {
        let out = cx.register(ty, span)?;
        return Ok(Operand::register(out, ty));
    }
    let OperandKind::Static(StaticValue::Function { def, args }) = operand.kind() else {
        let value = lower_value(cx, operand, span)?;
        return Ok(Operand::value(value, ty));
    };
    let id = declare_function(cx.program, &(*def, args.clone()), span)?;
    cx.program.module_mut().use_function(id);
    let out = cx.register(ty, span)?;
    cx.intrinsic(
        MIRInternalIntrinsic::GetFnPtr {
            out: MIRTarget::Register(out),
            fn_id: id,
        },
        span,
    );
    Ok(Operand::register(out, ty))
}
