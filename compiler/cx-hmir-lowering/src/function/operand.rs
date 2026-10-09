use cx_hmir::HMIRIntWidth;
use cx_log::catalogue::mir;
use cx_mir::{
    MIRBindable, MIRBitfieldAccess, MIRConstant, MIRGlobalRef, MIRInstructionKind,
    MIRInternalIntrinsic, MIRPlaceID, MIRRegisterID, MIRStoreBitfield, MIRTarget, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    eval::eval_global_type,
    function::{FunctionLowering, LowerResult},
    module::{declare_function, global_ref, to_constant},
    ty::{HMIRTypeID, HMIRTypeKind, TypeTable},
    value::StaticValue,
};

#[derive(Debug, Clone)]
pub(crate) struct Operand {
    kind: OperandKind,
    ty: HMIRTypeID,
    pointee_origin: Option<MIRPlaceID>,
}

#[derive(Debug, Clone)]
pub(crate) enum OperandKind {
    Place(MIRPlaceID),
    AdoptedPlace(MIRPlaceID),
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
    pub(crate) fn new(kind: OperandKind, ty: HMIRTypeID) -> Self {
        Self {
            kind,
            ty,
            pointee_origin: None,
        }
    }

    pub(crate) fn place(place: MIRPlaceID, ty: HMIRTypeID) -> Self {
        Self::new(OperandKind::Place(place), ty)
    }

    pub(crate) fn value(value: MIRValue, ty: HMIRTypeID) -> Self {
        Self::new(OperandKind::Value(value), ty)
    }

    pub(crate) fn register(register: MIRRegisterID, ty: HMIRTypeID) -> Self {
        Self::value(MIRValue::Register(register), ty)
    }

    pub(crate) fn reference(reg: MIRRegisterID, ty: HMIRTypeID, origin: Option<MIRPlaceID>) -> Self {
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

    pub(crate) fn ty(&self) -> HMIRTypeID {
        self.ty
    }

    pub(crate) fn with_type(mut self, ty: HMIRTypeID) -> Self {
        self.ty = ty;
        self
    }

    pub(crate) fn pointee_origin(&self) -> Option<MIRPlaceID> {
        self.pointee_origin
    }

    pub(crate) fn with_pointee_origin(mut self, origin: Option<MIRPlaceID>) -> Self {
        self.pointee_origin = origin;
        self
    }

    pub(crate) fn is_lvalue(&self) -> bool {
        matches!(
            self.kind,
            OperandKind::Place(_)
                | OperandKind::AdoptedPlace(_)
                | OperandKind::Ref { .. }
                | OperandKind::Global(_)
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
            OperandKind::Place(place) | OperandKind::AdoptedPlace(place) => {
                MIRValue::PlaceRef(*place)
            }
            OperandKind::Ref { reg, .. } => MIRValue::Register(*reg),
            OperandKind::Global(global) => MIRValue::Constant(MIRConstant::GlobalRef(*global)),
            _ => return None,
        })
    }

    pub(crate) fn target(&self) -> Option<MIRTarget> {
        Some(match &self.kind {
            OperandKind::Place(place) | OperandKind::AdoptedPlace(place) => {
                MIRTarget::Place(*place)
            }
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
            OperandKind::Place(place) | OperandKind::AdoptedPlace(place) => Some(*place),
            OperandKind::Ref { origin, .. } => *origin,
            _ => None,
        }
    }
}

pub(super) fn lower_store(
    cx: &mut FunctionLowering<'_, '_>,
    target: MIRTarget,
    value: MIRValue,
    ty: HMIRTypeID,
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
    cx.invalidate(MIRBindable::Place(origin), MIRInvalidationKind::Move, span);
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
    let ty = cx.program.types_mut().unqualified(operand.ty);
    Ok(Operand::value(lower_value(cx, operand, span)?, ty))
}

fn lower_static_value(
    cx: &mut FunctionLowering<'_, '_>,
    value: StaticValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    if cx.unevaluated {
        return Ok(MIRValue::Constant(MIRConstant::Unit));
    }
    match value {
        StaticValue::Str(string) => {
            let ty = match cx.program.types().kind(ty) {
                HMIRTypeKind::PointerTo(_) | HMIRTypeKind::ReferenceTo(_) => ty,
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
            let ty = cx.program.types_mut().decayed(ty);
            let out = cx.register(ty, span)?;
            cx.intrinsic(
                MIRInternalIntrinsic::GetFnPtr {
                    out: MIRTarget::Register(out),
                    fn_id: id,
                },
                span,
            );
            Ok(MIRValue::Register(out))
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
        cx.invalidate(
            MIRBindable::Register(register),
            MIRInvalidationKind::Move,
            span,
        );
    }
    cx.initialize(place, span);
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
            _ => {
                return cx.error(
                    span,
                    &mir::UNSUPPORTED_LOWERING,
                    "a reference outside of a register".into(),
                );
            }
        },
    };
    Ok(Operand::reference(reference, inner, None))
}

pub(super) fn lower_int_constant(
    cx: &mut FunctionLowering<'_, '_>,
    value: i128,
    ty: HMIRTypeID,
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
