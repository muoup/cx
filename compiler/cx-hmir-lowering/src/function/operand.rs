use cx_hmir::HMIRIntWidth;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBindable, MIRBitfieldAccess, MIRConstant, MIRFloatIntrinsic,
    MIRGlobalRef, MIRInstructionKind, MIRIntIntrinsic, MIRInternalIntrinsic, MIRPlaceID,
    MIRPtrIntrinsic, MIRRegisterID, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    eval::{eval_global_type, ops::coerce_static},
    function::{FunctionLowering, LowerResult, aggregate::lower_deref_pointer},
    module::{declare_function, global_ref, to_constant},
    ty::{TypeID, TypeKind, TypeTable},
    value::StaticValue,
};

#[derive(Debug, Clone)]
pub(crate) struct Operand {
    kind: OperandKind,
    ty: TypeID,
}

#[derive(Debug, Clone)]
pub(crate) enum OperandKind {
    Place(MIRPlaceID),
    // A reference register; 'origin' is the place whose storage it views, when known
    Ref {
        reg: MIRRegisterID,
        bitfield: Option<MIRBitfieldAccess>,
        origin: Option<MIRPlaceID>,
    },
    Global(MIRGlobalRef),
    Value(MIRValue),
    Static(StaticValue),
}

impl Operand {
    pub(crate) fn new(kind: OperandKind, ty: TypeID) -> Self {
        Self { kind, ty }
    }

    pub(crate) fn place(place: MIRPlaceID, ty: TypeID) -> Self {
        Self::new(OperandKind::Place(place), ty)
    }

    pub(crate) fn value(value: MIRValue, ty: TypeID) -> Self {
        Self::new(OperandKind::Value(value), ty)
    }

    pub(crate) fn register(register: MIRRegisterID, ty: TypeID) -> Self {
        Self::value(MIRValue::Register(register), ty)
    }

    pub(crate) fn reference(reg: MIRRegisterID, ty: TypeID, origin: Option<MIRPlaceID>) -> Self {
        Self::new(
            OperandKind::Ref {
                reg,
                bitfield: None,
                origin,
            },
            ty,
        )
    }

    pub(crate) fn unit(types: &mut TypeTable) -> Self {
        Self::value(MIRValue::Constant(MIRConstant::Unit), types.void())
    }

    pub(crate) fn kind(&self) -> &OperandKind {
        &self.kind
    }

    pub(crate) fn ty(&self) -> TypeID {
        self.ty
    }

    pub(crate) fn with_type(mut self, ty: TypeID) -> Self {
        self.ty = ty;
        self
    }

    pub(crate) fn is_lvalue(&self) -> bool {
        matches!(
            self.kind,
            OperandKind::Place(_) | OperandKind::Ref { .. } | OperandKind::Global(_)
        )
    }

    pub(crate) fn as_static(&self) -> Option<&StaticValue> {
        match &self.kind {
            OperandKind::Static(value) => Some(value),
            _ => None,
        }
    }

    // The value naming this lvalue's storage
    pub(crate) fn address(&self) -> Option<MIRValue> {
        Some(match &self.kind {
            OperandKind::Place(place) => MIRValue::PlaceRef(*place),
            OperandKind::Ref { reg, .. } => MIRValue::Register(*reg),
            OperandKind::Global(global) => MIRValue::Constant(MIRConstant::GlobalRef(*global)),
            _ => return None,
        })
    }

    pub(crate) fn target(&self) -> Option<MIRTarget> {
        Some(match &self.kind {
            OperandKind::Place(place) => MIRTarget::Place(*place),
            OperandKind::Ref { reg, .. } => MIRTarget::Indirect(*reg),
            OperandKind::Global(global) => MIRTarget::Global(*global),
            _ => return None,
        })
    }

    pub(crate) fn bitfield(&self) -> Option<MIRBitfieldAccess> {
        match &self.kind {
            OperandKind::Ref { bitfield, .. } => *bitfield,
            _ => None,
        }
    }

    // The place owning this lvalue's storage
    pub(crate) fn origin(&self) -> Option<MIRPlaceID> {
        match &self.kind {
            OperandKind::Place(place) => Some(*place),
            OperandKind::Ref { origin, .. } => *origin,
            _ => None,
        }
    }
}

pub(super) fn lower_store(
    cx: &mut FunctionLowering<'_, '_>,
    target: MIRTarget,
    value: MIRValue,
    ty: TypeID,
    bitfield: Option<MIRStoreBitfield>,
    span: &TokenRange,
) -> LowerResult<()> {
    let ty = cx.mir(ty, span)?;
    cx.emit(
        MIRInstructionKind::Store {
            target,
            value,
            ty,
            bitfield,
        },
        span,
    );
    Ok(())
}

// Copies an lvalue's current value into a register
pub(super) fn lower_copy(
    cx: &mut FunctionLowering<'_, '_>,
    operand: &Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let out = cx.register(operand.ty, span)?;
    let source = operand.address().expect("copied operand is an lvalue");
    let bitfield = operand.bitfield().map(MIRStoreBitfield::Source);
    lower_store(
        cx,
        MIRTarget::Register(out),
        source,
        operand.ty,
        bitfield,
        span,
    )?;
    Ok(MIRValue::Register(out))
}

// Moves an lvalue's value out, consuming the liveness of the place that owns it
pub(super) fn lower_lift(
    cx: &mut FunctionLowering<'_, '_>,
    operand: &Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let Some(origin) = operand.origin() else {
        return lower_copy(cx, operand, span);
    };
    let source = match operand.kind() {
        OperandKind::Ref { reg, .. } => MIRBindable::Register(*reg),
        _ => MIRBindable::Place(origin),
    };
    let out = cx.register(operand.ty, span)?;
    cx.emit(
        MIRInstructionKind::Lift {
            out,
            source,
            origin: MIRBindable::Place(origin),
        },
        span,
    );
    cx.emit(
        MIRInstructionKind::Invalidate {
            place: MIRBindable::Place(origin),
            kind: MIRInvalidationKind::Move,
        },
        span,
    );
    Ok(MIRValue::Register(out))
}

// The operand's value, moving out of storage whose type cannot be copied
pub(crate) fn lower_value(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    match operand.kind {
        OperandKind::Value(value) => Ok(value),
        OperandKind::Static(value) => lower_static_value(cx, value, operand.ty, span),
        _ if cx.program.types().is_pod(operand.ty) => lower_copy(cx, &operand, span),
        _ => lower_lift(cx, &operand, span),
    }
}

pub(super) fn lower_read(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let ty = operand.ty;
    Ok(Operand::value(lower_value(cx, operand, span)?, ty))
}

fn lower_static_value(
    cx: &mut FunctionLowering<'_, '_>,
    value: StaticValue,
    ty: TypeID,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    if cx.unevaluated {
        return Ok(MIRValue::Constant(MIRConstant::Unit));
    }
    match value {
        StaticValue::Str(string) => {
            let ty = match cx.program.types().kind(ty) {
                TypeKind::Pointer(_) => ty,
                _ => cx.program.types_mut().char_pointer(),
            };
            let out = cx.register(ty, span)?;
            cx.intrinsic(
                MIRInternalIntrinsic::StringAddress {
                    out: MIRTarget::Register(out),
                    string,
                },
                span,
            );
            Ok(MIRValue::Register(out))
        }
        StaticValue::Function { def, args } => {
            let id = declare_function(cx.program, &(def, args), span)?;
            cx.program.module_mut().use_function(id);
            Ok(MIRValue::Constant(MIRConstant::Function(id)))
        }
        StaticValue::Global(def) => {
            let global = global_ref(cx.program, def, span)?;
            let ty = eval_global_type(cx.program, def, span)?;
            lower_value(cx, Operand::new(OperandKind::Global(global), ty), span)
        }
        value => Ok(MIRValue::Constant(to_constant(
            cx.program, &value, ty, span,
        )?)),
    }
}

// Makes the operand addressable, spilling values into a temporary place
pub(super) fn lower_spill(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if operand.is_lvalue() {
        return Ok(operand);
    }
    if let OperandKind::Static(StaticValue::Global(def)) = operand.kind {
        let global = global_ref(cx.program, def, span)?;
        let ty = eval_global_type(cx.program, def, span)?;
        return Ok(Operand::new(OperandKind::Global(global), ty));
    }
    let ty = operand.ty;
    let value = lower_value(cx, operand, span)?;
    let place = cx.place(ty, None, span)?;
    lower_store(cx, MIRTarget::Place(place), value.clone(), ty, None, span)?;
    if let MIRValue::Register(register) = value
        && !cx.program.types().is_pod(ty)
    {
        cx.emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Register(register),
                kind: MIRInvalidationKind::Move,
            },
            span,
        );
    }
    cx.emit(
        MIRInstructionKind::Initialize {
            place: MIRBindable::Place(place),
        },
        span,
    );
    Ok(Operand::place(place, ty))
}

// Reading a reference-typed lvalue yields the referenced storage
pub(super) fn lower_auto_deref(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Some(inner) = cx.program.types().reference_inner(operand.ty) else {
        return Ok(operand);
    };
    let reference = match operand.kind {
        OperandKind::Value(MIRValue::Register(register)) => register,
        _ => match lower_value(cx, operand, span)? {
            MIRValue::Register(register) => register,
            _ => return cx.error(span, "reference is not a register"),
        },
    };
    Ok(Operand::reference(reference, inner, None))
}

pub(super) fn lower_int_constant(
    cx: &mut FunctionLowering<'_, '_>,
    value: i128,
    ty: TypeID,
) -> MIRValue {
    let width = cx
        .program
        .types()
        .int_info(ty)
        .map(|(width, _)| width)
        .unwrap_or(HMIRIntWidth::I64);
    MIRValue::Constant(MIRConstant::Integer {
        ty: TypeTable::mir_int(width),
        value,
    })
}

pub(super) fn lower_truthy(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let bool = cx.program.types_mut().bool();
    if operand.ty == bool {
        return lower_read(cx, operand, span);
    }
    if let Some(value) = operand.as_static().and_then(StaticValue::is_truthy) {
        return Ok(Operand::new(
            OperandKind::Static(StaticValue::bool(value, cx.program.types_mut())),
            bool,
        ));
    }
    let operand = lower_decay(cx, operand, span)?;
    let kind = cx.program.types().kind(operand.ty).clone();
    let source_ty = operand.ty;
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

// Converts between value types; covers C's implicit conversions and explicit casts
pub(crate) fn lower_convert(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    target: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let source = operand.ty;
    if source == target {
        return Ok(operand);
    }
    let types = cx.program.types();
    let source_kind = types.kind(source).clone();
    let target_kind = types.kind(target).clone();

    if let OperandKind::Static(value) = &operand.kind
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

// Arrays, strings and functions used as values become pointers to their first element
pub(super) fn lower_decay(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let kind = cx.program.types().kind(operand.ty).clone();
    match kind {
        TypeKind::Array { element, .. } => {
            let operand = lower_spill(cx, operand, span)?;
            let ty = cx.program.types_mut().pointer_to(element);
            let out = cx.register(ty, span)?;
            let array = operand.address().expect("spilled operand is addressable");
            cx.intrinsic(
                MIRInternalIntrinsic::ArrayAddress {
                    out: MIRTarget::Register(out),
                    array,
                },
                span,
            );
            Ok(Operand::register(out, ty))
        }
        TypeKind::Str => {
            let ty = cx.program.types_mut().char_pointer();
            let value = lower_value(cx, operand, span)?;
            Ok(Operand::value(value, ty))
        }
        TypeKind::Function(_) => {
            let ty = cx.program.types_mut().pointer_to(operand.ty);
            if cx.unevaluated {
                let out = cx.register(ty, span)?;
                return Ok(Operand::register(out, ty));
            }
            match operand.kind {
                OperandKind::Static(StaticValue::Function { def, args }) => {
                    let id = declare_function(cx.program, &(def, args), span)?;
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
                _ => {
                    let value = lower_value(cx, operand, span)?;
                    Ok(Operand::value(value, ty))
                }
            }
        }
        _ => Ok(operand),
    }
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
