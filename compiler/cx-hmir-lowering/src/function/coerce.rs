use cx_hmir::{HMIRCoerceMode, HMIRExprID, HMIRIntWidth};
use cx_mir::{
    MIRAggregateIntrinsic, MIRConstant, MIRFloatIntrinsic, MIRIntIntrinsic, MIRInternalIntrinsic,
    MIRPtrIntrinsic, MIRTarget, MIRValue,
};
use cx_tokens::TokenRange;

use crate::{
    eval::ops::coerce_static,
    function::{
        Expect, FunctionLowering, LowerResult, Operand, OperandKind,
        aggregate::lower_deref_pointer,
        expr::lower_expr,
        lower_eval_type,
        operand::{lower_auto_deref, lower_int_constant, lower_read, lower_spill, lower_value},
        promote::lower_decay,
    },
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

pub(super) fn lower_nonnull_pointer(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let ty = operand.ty();
    let pointer = lower_value(cx, operand, span)?;
    let bool = cx.program.types_mut().bool();
    let condition = cx.register(bool, span)?;
    let null_ty = cx.mir(ty, span)?;
    cx.intrinsic(
        MIRPtrIntrinsic::Neq {
            out: MIRTarget::Register(condition),
            lhs: pointer.clone(),
            rhs: MIRValue::Constant(MIRConstant::Nullptr { ty: null_ty }),
        },
        span,
    );
    cx.intrinsic(
        MIRInternalIntrinsic::Assert {
            condition: MIRValue::Register(condition),
            message: Some("dereferenced a null pointer".into()),
        },
        span,
    );
    Ok(pointer)
}

pub(super) fn lower_coerce(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if mode == HMIRCoerceMode::Truthy {
        let value = lower_expr(cx, frame, value, Expect::Any)?;
        return lower_truthy(cx, value, span);
    }
    let ty = lower_eval_type(cx, frame, target)?;
    let types = cx.program.types();
    let reference = types.reference_inner(ty);
    let expect = if reference.is_some() {
        Expect::Any
    } else {
        Expect::Type(ty)
    };
    let value = lower_expr(cx, frame, value, expect)?;
    let value = if mode == HMIRCoerceMode::CCast
        && reference.is_some_and(|inner| inner != value.ty())
        && cx.program.types().is_pointer(value.ty())
    {
        let value = lower_nonnull_pointer(cx, value, span)?;
        let out = cx.register(ty, span)?;
        let target_ty = cx.mir(ty, span)?;
        cx.intrinsic(
            MIRInternalIntrinsic::Bitcast {
                out: MIRTarget::Register(out),
                value,
                target_ty,
            },
            span,
        );
        Operand::register(out, ty)
    } else {
        lower_convert(cx, value, ty, span)?
    };
    if let Some(inner) = reference {
        if cx.program.types().is_function(inner) {
            let pointer = cx.program.types_mut().pointer_to(inner);
            return Ok(value.with_type(pointer));
        }
        return lower_auto_deref(cx, value, span);
    }
    Ok(value)
}

pub(super) fn lower_truthy(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let bool = cx.program.types_mut().bool();
    if operand.ty() == bool {
        return lower_read(cx, operand, span);
    }
    if let Some(value) = operand.as_static().and_then(StaticValue::is_truthy) {
        return Ok(Operand::new(
            OperandKind::Static(StaticValue::bool(value, cx.program.types_mut())),
            bool,
        ));
    }
    let operand = lower_decay(cx, operand, span)?;
    let kind = cx.program.types().kind(operand.ty()).clone();
    let source_ty = operand.ty();
    let value = lower_value(cx, operand, span)?;
    let out = cx.register(bool, span)?;
    let target = MIRTarget::Register(out);
    match kind {
        TypeKind::Int { .. } => {
            let zero = lower_int_constant(cx, 0, source_ty);
            cx.intrinsic(
                MIRIntIntrinsic::Neq {
                    out: target,
                    lhs: value,
                    rhs: zero,
                },
                span,
            );
        }
        TypeKind::Float { width } => {
            let zero = MIRValue::Constant(MIRConstant::Float {
                value: 0.0f64.into(),
                ty: TypeTable::mir_float(width),
            });
            cx.intrinsic(
                MIRFloatIntrinsic::Neq {
                    out: target,
                    lhs: value,
                    rhs: zero,
                },
                span,
            );
        }
        TypeKind::Pointer(_) | TypeKind::Str | TypeKind::Function(_) => {
            let null = MIRValue::Constant(MIRConstant::Nullptr {
                ty: cx.mir(source_ty, span)?,
            });
            cx.intrinsic(
                MIRPtrIntrinsic::Neq {
                    out: target,
                    lhs: value,
                    rhs: null,
                },
                span,
            );
        }
        _ => {
            return cx.error(
                span,
                format!(
                    "'{}' has no truth value",
                    cx.program.types().display(source_ty)
                ),
            );
        }
    }
    Ok(Operand::register(out, bool))
}

// A function may stand in for one returning nothing unless its result has to be consumed
fn discards_nodrop_result(types: &TypeTable, source: TypeID, target: TypeID) -> bool {
    let function = |ty: TypeID| match types.kind(ty) {
        TypeKind::Pointer(inner) => match types.kind(*inner) {
            TypeKind::Function(function) => Some(function.ret()),
            _ => None,
        },
        TypeKind::Function(function) => Some(function.ret()),
        _ => None,
    };
    match (function(source), function(target)) {
        (Some(from), Some(to)) => types.is_void(to) && types.is_nodrop(from),
        _ => false,
    }
}

// Converts between value types; covers C's implicit conversions and explicit casts
pub(crate) fn lower_convert(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    target: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let source = operand.ty();
    if source == target {
        return Ok(operand);
    }
    let types = cx.program.types();
    let source_kind = types.kind(source).clone();
    let target_kind = types.kind(target).clone();
    if discards_nodrop_result(types, source, target) {
        return cx.error(
            span,
            format!(
                "cannot convert '{}' to '{}': the @nodrop result would be discarded",
                types.display(source),
                types.display(target)
            ),
        );
    }

    if let OperandKind::Static(value) = operand.kind()
        && !cx.program.types().is_reference(target)
        && !(matches!(value, StaticValue::Str(_)) && cx.program.types().is_array(target))
        && let Ok(value) = coerce_static(cx.program, value.clone(), target, span)
    {
        return Ok(Operand::new(OperandKind::Static(value), target));
    }

    match (&source_kind, &target_kind) {
        (_, TypeKind::Void) => return Ok(Operand::unit(cx.program.types_mut())),
        (TypeKind::Reference(inner), _) if *inner == target => {
            let operand = lower_auto_deref(cx, operand, span)?;
            return Ok(operand);
        }
        (TypeKind::Str, TypeKind::Reference(inner))
            if *inner == source && operand.as_static().is_some() =>
        {
            let value = lower_value(cx, operand.with_type(target), span)?;
            return Ok(Operand::value(value, target));
        }
        (_, TypeKind::Reference(inner)) if *inner == source => {
            let operand = lower_spill(cx, operand, span)?;
            let address = operand.address().expect("spilled operand is addressable");
            return Ok(Operand::value(address, target));
        }
        (_, TypeKind::Reference(inner)) => {
            let operand = lower_decay(cx, operand, span)?;
            if cx.program.types().pointer_inner(operand.ty()) == Some(*inner) {
                let operand = lower_deref_pointer(cx, operand, span)?;
                let address = operand
                    .address()
                    .expect("dereferenced pointer is addressable");
                return Ok(Operand::value(address, target));
            }
            return cx.error(
                span,
                format!(
                    "cannot convert '{}' to '{}'",
                    cx.program.types().display(source),
                    cx.program.types().display(target)
                ),
            );
        }
        (
            TypeKind::Int {
                width: from,
                signed,
            },
            TypeKind::Int { width: to, .. },
        ) => {
            let (from, signed, to) = (*from, *signed, *to);
            let value = lower_value(cx, operand, span)?;
            if from == to {
                return Ok(Operand::value(value, target));
            }
            let out = cx.register(target, span)?;
            if to == HMIRIntWidth::I1 {
                let zero = lower_int_constant(cx, 0, source);
                cx.intrinsic(
                    MIRIntIntrinsic::Neq {
                        out: MIRTarget::Register(out),
                        lhs: value,
                        rhs: zero,
                    },
                    span,
                );
            } else {
                cx.intrinsic(
                    MIRIntIntrinsic::IntCast {
                        out: MIRTarget::Register(out),
                        value,
                        target: TypeTable::mir_int(to),
                        sign_extend: signed && from != HMIRIntWidth::I1,
                    },
                    span,
                );
            }
            return Ok(Operand::register(out, target));
        }
        (TypeKind::Int { signed, .. }, TypeKind::Float { width }) => {
            let (signed, width) = (*signed, *width);
            let value = lower_value(cx, operand, span)?;
            let out = cx.register(target, span)?;
            cx.intrinsic(
                MIRIntIntrinsic::ToFloat {
                    out: MIRTarget::Register(out),
                    value,
                    target: TypeTable::mir_float(width),
                    signed,
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (TypeKind::Float { .. }, TypeKind::Int { width, signed }) => {
            if *width == HMIRIntWidth::I1 {
                return lower_truthy(cx, operand, span);
            }
            let signed = *signed;
            let value = lower_value(cx, operand, span)?;
            let out = cx.register(target, span)?;
            let target_ty = cx.mir(target, span)?;
            cx.intrinsic(
                MIRFloatIntrinsic::ToInt {
                    out: MIRTarget::Register(out),
                    value,
                    target_ty,
                    signed,
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (TypeKind::Float { width: from }, TypeKind::Float { width: to }) => {
            let value = lower_value(cx, operand, span)?;
            if from == to {
                return Ok(Operand::value(value, target));
            }
            let width = *to;
            let out = cx.register(target, span)?;
            cx.intrinsic(
                MIRFloatIntrinsic::FloatCast {
                    out: MIRTarget::Register(out),
                    value,
                    float_ty: TypeTable::mir_float(width),
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (
            TypeKind::Pointer(_) | TypeKind::Str | TypeKind::Function(_),
            TypeKind::Int { width, .. },
        ) => {
            if *width == HMIRIntWidth::I1 {
                return lower_truthy(cx, operand, span);
            }
            let operand = lower_decay(cx, operand, span)?;
            let value = lower_value(cx, operand, span)?;
            let out = cx.register(target, span)?;
            let target_ty = cx.mir(target, span)?;
            cx.intrinsic(
                MIRPtrIntrinsic::ToInt {
                    out: MIRTarget::Register(out),
                    ptr: value,
                    target_ty,
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (TypeKind::Int { signed, .. }, TypeKind::Pointer(_)) => {
            let signed = *signed;
            let value = lower_value(cx, operand, span)?;
            let out = cx.register(target, span)?;
            cx.intrinsic(
                MIRIntIntrinsic::ToPtr {
                    out: MIRTarget::Register(out),
                    value,
                    sign_extend: signed,
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (TypeKind::Str, TypeKind::Array { length, .. }) => {
            let length = *length;
            return lower_string_array(cx, operand, target, length, span);
        }
        (TypeKind::Array { .. } | TypeKind::Str | TypeKind::Function(_), TypeKind::Pointer(_)) => {
            let operand = lower_decay(cx, operand, span)?;
            return lower_convert(cx, operand, target, span);
        }
        (TypeKind::Pointer(_), TypeKind::Pointer(_)) => {
            let source_mir = cx.mir(source, span)?;
            let target_mir = cx.mir(target, span)?;
            let value = lower_value(cx, operand, span)?;
            if source_mir == target_mir {
                return Ok(Operand::value(value, target));
            }
            let out = cx.register(target, span)?;
            cx.intrinsic(
                MIRInternalIntrinsic::Bitcast {
                    out: MIRTarget::Register(out),
                    value,
                    target_ty: target_mir,
                },
                span,
            );
            return Ok(Operand::register(out, target));
        }
        (
            TypeKind::Array { element: from, .. },
            TypeKind::Array {
                element: to,
                length: None,
            },
        ) if from == to => {
            return Ok(operand.with_type(source));
        }
        (TypeKind::Reference(_), _) => {
            let operand = lower_auto_deref(cx, operand, span)?;
            return lower_convert(cx, operand, target, span);
        }
        (TypeKind::Unreachable, _) => return Ok(operand.with_type(target)),
        _ => {}
    }

    let source_mir = cx.mir(source, span)?;
    let target_mir = cx.mir(target, span)?;
    if source_mir == target_mir {
        return Ok(operand.with_type(target));
    }
    cx.error(
        span,
        format!(
            "cannot convert '{}' to '{}'",
            cx.program.types().display(source),
            cx.program.types().display(target)
        ),
    )
}

fn lower_string_array(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    target: TypeID,
    length: Option<u64>,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Some(StaticValue::Str(string)) = operand.as_static().cloned() else {
        return cx.error(span, "array initialized from a non-constant string");
    };
    let length = length.unwrap_or(string.len() as u64 + 1) as usize;
    if string.len() > length {
        return cx.error(span, "string is longer than its array");
    }
    let mut fields = string
        .bytes()
        .enumerate()
        .map(|(index, byte)| {
            (
                index,
                MIRValue::Constant(MIRConstant::Integer {
                    ty: cx_mir::MIRIntType::I8,
                    value: byte as i128,
                }),
            )
        })
        .collect::<Vec<_>>();
    if string.len() < length {
        fields.push((
            string.len(),
            MIRValue::Constant(MIRConstant::Integer {
                ty: cx_mir::MIRIntType::I8,
                value: 0,
            }),
        ));
    }
    let target = match cx.program.types().kind(target).clone() {
        TypeKind::Array {
            element,
            length: None,
        } => cx.program.types_mut().intern(TypeKind::Array {
            element,
            length: Some(length as u64),
        }),
        _ => target,
    };
    let out = cx.register(target, span)?;
    let ty = cx.mir(target, span)?;
    cx.intrinsic(
        MIRAggregateIntrinsic::AggregateInit {
            out: MIRTarget::Register(out),
            ty,
            fields,
        },
        span,
    );
    Ok(Operand::register(out, target))
}
