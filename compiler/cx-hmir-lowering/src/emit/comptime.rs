mod op;

pub use op::{binary, cast, unary};

use cx_hmir::{
    HMIRDefID,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::{LocalLoweringContext, comptime::ComptimeSlotState},
    log::hmir_error,
    value::{HMIRPlace, HMIRSlotID, HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
) -> HMIRValue {
    let slot = context.comptime_mut().allocate_slot(ty, name);

    HMIRValue::place(ty, HMIRPlace::Local(slot))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let fields = fields.iter().map(|(index, value)| {
        Ok((*index, Box::new(read(context, value, span)?)))
    }).collect::<CXResult<_>>()?;

    Ok(HMIRValue::constant(ty, HMIRConstant::Aggregate { ty, fields }))
}

// The slot whose state a value's liveness is recorded on; storage behind an address is untracked
fn tracked_slot(context: &LocalLoweringContext, value: &HMIRValue) -> Option<HMIRSlotID> {
    match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => Some(*slot),
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => None,
        HMIRValueKind::Constant(HMIRConstant::Address {
            place: HMIRPlace::Local(slot),
            path,
            ..
        }) if path.is_empty() && context.env().is_reference(value.ty()) => Some(*slot),
        HMIRValueKind::Constant(_) | HMIRValueKind::Indirect(_) => None,
    }
}

fn read(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    let global = match value.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Global(def)) => Some((*def, &[][..])),
        HMIRValueKind::Constant(HMIRConstant::Address {
            place: HMIRPlace::Global(def),
            path,
            ..
        }) if context.env().is_reference(value.ty()) => Some((*def, path.as_slice())),
        _ => None,
    };
    if let Some((def, path)) = global {
        return read_global(context, def, path, span);
    }
    let slot = match (value.kind(), tracked_slot(context, value)) {
        (_, Some(slot)) => slot,
        (HMIRValueKind::Constant(constant), None)
            if !matches!(constant, HMIRConstant::Address { .. })
                || !context.env().is_reference(value.ty()) =>
        {
            return Ok(constant.clone());
        }
        _ => return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, "reading through this address".into())),
    };

    let slot = context.comptime().slot(slot);
    let subject = match slot.name() {
        Some(name) => format!("'{name}'"),
        None => "this temporary".into(),
    };
    match slot.state() {
        ComptimeSlotState::Value(constant) => Ok(constant.clone()),
        ComptimeSlotState::Uninitialized => Err(hmir_error(span, &mir::COMPTIME_UNSET, subject)),
        ComptimeSlotState::Invalidated => Err(hmir_error(
            span,
            &mir::COMPTIME_UNSET,
            format!("{subject}, which was moved or leaked,"),
        )),
    }
}

fn read_global(
    context: &mut LocalLoweringContext,
    def: HMIRDefID,
    path: &[usize],
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    if !context.initializer() || context.visible_through().is_some() {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_UNAVAILABLE,
            format!(
                "value of '{}'",
                context.env().symbols().resolve_definition(def).name()
            ),
        ));
    }
    let result = crate::symbol::initialize(context, def)?;
    let HMIRValueKind::Constant(mut constant) = result.kind().clone() else {
        unreachable!("a global initializer is frozen");
    };
    for index in path {
        let HMIRConstant::Aggregate { ty, fields } = constant else {
            return Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "reading a global subobject".into(),
            ));
        };
        constant = match fields.into_iter().find(|(field, _)| field == index) {
            Some((_, value)) => *value,
            None => {
                let field = match context.env().resolve_kind(ty) {
                    HMIRTypeKind::Array { element, .. } => *element,
                    HMIRTypeKind::Struct { fields }
                    | HMIRTypeKind::Union { fields }
                    | HMIRTypeKind::TaggedUnion { fields } => *fields[*index].ty(),
                    _ => unreachable!("a global subobject path indexes an aggregate"),
                };
                crate::symbol::zero(context, field, span)?
            }
        };
    }
    Ok(constant)
}

pub fn copy(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a comptime copy reads from a reference")
    };

    Ok(HMIRValue::constant(inner, read(context, &value, span)?))
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<()> {
    let constant = read(context, value, span)?;
    let Some(slot) = tracked_slot(context, target) else {
        return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION, "writing through this address".into()));
    };
    *context.comptime_mut().slot_mut(slot).state_mut() = ComptimeSlotState::Value(constant);

    Ok(())
}

pub fn initialize(context: &mut LocalLoweringContext, value: &HMIRValue) {
    if let Some(slot) = tracked_slot(context, value) {
        debug_assert!(
            matches!(
                context.comptime().slot(slot).state(),
                ComptimeSlotState::Value(_)
            ),
            "comptime slot initialized before it was written"
        );
    }
}

pub fn invalidate(context: &mut LocalLoweringContext, value: &HMIRValue, kind: MIRInvalidationKind) {
    if let Some(slot) = tracked_slot(context, value) {
        *context.comptime_mut().slot_mut(slot).state_mut() = match kind {
            MIRInvalidationKind::Drop => ComptimeSlotState::Uninitialized,
            MIRInvalidationKind::Move | MIRInvalidationKind::Leak => ComptimeSlotState::Invalidated,
        };
    }
}

pub fn address(value: &HMIRValue, ty: HMIRTypeID) -> HMIRValue {
    let (place, path) = match value.kind() {
        HMIRValueKind::PlaceRef(place) => (place, Vec::new()),
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => (place, path.clone()),
        HMIRValueKind::Indirect(pointer) => match pointer.kind() {
            HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
                (place, path.clone())
            }
            _ => todo!("comptime address through a pointer that holds no address"),
        },
        HMIRValueKind::Constant(_) => unreachable!("only storage has an address"),
    };

    HMIRValue::constant(
        ty,
        HMIRConstant::Address {
            place: *place,
            path,
            ty,
        },
    )
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = HMIRValue::constant(pointer.ty(), read(context, &pointer, span)?);
    if !matches!(
        pointer.kind(),
        HMIRValueKind::Constant(HMIRConstant::Address { .. })
    ) {
        let operation = "dereference".into();
        return Err(hmir_error(span, &mir::COMPTIME_UNDEFINED_ARITHMETIC, operation));
    }

    Ok(HMIRValue::indirect(inner, pointer))
}

pub fn field(base: &HMIRValue, index: usize, ty: HMIRTypeID) -> HMIRValue {
    let address = address(base, ty);
    let HMIRValueKind::Constant(HMIRConstant::Address {
        place, mut path, ..
    }) = address.kind().clone()
    else {
        unreachable!()
    };
    path.push(index);
    HMIRValue::constant(ty, HMIRConstant::Address { place, path, ty })
}
