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
    symbol,
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
    
    if let HMIRValueKind::Constant(HMIRConstant::Address {
        place: HMIRPlace::Local(slot),
        path,
        ..
    }) = value.kind()
        && context.env().is_reference(value.ty())
        && !path.is_empty()
    {
        let root = HMIRValue::place(context.comptime().slot(*slot).ty(), HMIRPlace::Local(*slot));
        let constant = read(context, &root, span)?;
        return project(context, constant, path, span);
    }
    
    let slot = match (value.kind(), tracked_slot(context, value)) {
        (_, Some(slot)) => slot,
        (HMIRValueKind::Constant(constant), None)
            if !matches!(constant, HMIRConstant::Address { .. })
                || !context.env().is_reference(value.ty()) =>
        {
            return Ok(constant.clone());
        }
        _ => {
            return Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "reading through this address".into(),
            ));
        }
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
    let result = symbol::initialize(context, def)?;
    let HMIRValueKind::Constant(constant) = result.kind().clone() else {
        unreachable!("a global initializer is frozen");
    };
    project(context, constant, path, span)
}

fn project(
    context: &mut LocalLoweringContext,
    mut constant: HMIRConstant,
    path: &[usize],
    span: &TokenRange,
) -> CXResult<HMIRConstant> {
    for index in path {
        let HMIRConstant::Aggregate { ty, fields } = constant else {
            return Err(hmir_error(
                span,
                &mir::COMPTIME_INVALID_OPERATION,
                "reading an aggregate subobject".into(),
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
                symbol::zero(context, field, span)?
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
    let payload = read(context, value, span)?;
    if let HMIRValueKind::Constant(HMIRConstant::Address {
        place: HMIRPlace::Local(slot),
        path,
        ..
    }) = target.kind()
        && !path.is_empty()
    {
        let root = HMIRValue::place(context.comptime().slot(*slot).ty(), HMIRPlace::Local(*slot));
        let mut constant = read(context, &root, span)?;
        replace(context, &mut constant, path, payload, span)?;
        *context.comptime_mut().slot_mut(*slot).state_mut() = ComptimeSlotState::Value(constant);
        return Ok(());
    }
    let Some(slot) = tracked_slot(context, target) else {
        return Err(hmir_error(
            span,
            &mir::COMPTIME_INVALID_OPERATION,
            "writing through this address".into(),
        ));
    };
    *context.comptime_mut().slot_mut(slot).state_mut() = ComptimeSlotState::Value(payload);

    Ok(())
}

fn replace(
    context: &mut LocalLoweringContext,
    constant: &mut HMIRConstant,
    path: &[usize],
    value: HMIRConstant,
    span: &TokenRange,
) -> CXResult<()> {
    let Some((index, rest)) = path.split_first() else {
        *constant = value;
        return Ok(());
    };
    let missing = project(context, constant.clone(), &[*index], span)?;
    let HMIRConstant::Aggregate { fields, .. } = constant else {
        unreachable!("a projected value is an aggregate")
    };
    let position = match fields.iter().position(|(field, _)| field == index) {
        Some(position) => position,
        None => {
            fields.push((*index, Box::new(missing)));
            fields.len() - 1
        }
    };
    replace(context, &mut fields[position].1, rest, value, span)
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
