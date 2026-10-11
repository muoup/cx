// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

use cx_hmir::{
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext},
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRInstruction,
    MIRInstructionKind, MIRInternalIntrinsic, MIRIntrinsic, MIRPlaceDecl, MIRPlaceID, MIRRegister,
    MIRRegisterDecl, MIRScopeID, MIRTarget, MIRTypeID, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    env::{function::MIRFunctionBuilder, lowering::FnLoweringContext},
    lower::constant::mir_constant,
};

pub fn terminated(builder: &MIRFunctionBuilder) -> bool {
    builder
        .block(builder.current_block())
        .and_then(MIRBasicBlock::last_instruction)
        .is_some_and(MIRInstruction::is_terminator)
}

pub fn jump(
    builder: &mut MIRFunctionBuilder,
    target: MIRBasicBlockID,
    args: Vec<MIRValue>,
    span: TokenRange,
) {
    builder.emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::with_args(target, args),
        },
        span,
    )
}

pub fn branch(
    builder: &mut MIRFunctionBuilder,
    cond: MIRValue,
    true_target: MIRBlockTarget,
    false_target: MIRBlockTarget,
    span: TokenRange,
) {
    builder.emit(
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        },
        span,
    )
}

pub fn intrinsic(
    builder: &mut MIRFunctionBuilder,
    intrinsic: impl Into<MIRIntrinsic>,
    span: TokenRange,
) {
    builder.emit(MIRInstructionKind::IntrinsicOp(intrinsic.into()), span)
}

pub fn store(
    builder: &mut MIRFunctionBuilder,
    target: MIRTarget,
    value: MIRValue,
    ty: MIRTypeID,
    span: TokenRange,
) {
    builder.emit(
        MIRInstructionKind::Store {
            target,
            value,
            ty,
            bitfield: None,
        },
        span,
    )
}

pub fn register(
    context: &mut FnLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let ty = context.env_mut().lower_type(ty, span)?;

    Ok(context
        .builder_mut()
        .allocate_register(MIRRegisterDecl::new(ty, None)))
}

pub fn materialize(
    context: &mut FnLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
            address(context, *place, path, value.ty(), span)?
        }
        HMIRValueKind::Constant(constant) => {
            MIRValue::Constant(mir_constant(context.env_mut(), constant, span)?)
        }
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => match context.slot(*slot) {
            MIRBindable::Register(register) => MIRValue::Register(*register),
            MIRBindable::Place(place) => MIRValue::PlaceRef(*place),
        },
        HMIRValueKind::Place(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Indirect(pointer) => MIRValue::Register(reference(context, pointer)),
    })
}

pub fn target(context: &FnLoweringContext, value: &HMIRValue) -> Option<MIRTarget> {
    match value.kind() {
        HMIRValueKind::Constant(_) => None,
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => match context.slot(*slot) {
            MIRBindable::Register(_) => None,
            MIRBindable::Place(place) => Some(MIRTarget::Place(*place)),
        },
        HMIRValueKind::Place(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Indirect(pointer) => Some(MIRTarget::Indirect(reference(context, pointer))),
    }
}

fn reference(context: &FnLoweringContext, pointer: &HMIRValue) -> MIRRegister {
    match pointer.kind() {
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => match context.slot(*slot) {
            MIRBindable::Register(register) => *register,
            MIRBindable::Place(_) => {
                unreachable!("an indirect value addresses through a register")
            }
        },
        _ => todo!("indirect value through a subobject or global address"),
    }
}

fn address(
    context: &mut FnLoweringContext,
    place: HMIRPlace,
    path: &[usize],
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    if !path.is_empty() {
        todo!("address of a subobject: aggregates are not lowered yet");
    }

    let HMIRPlace::Local(slot) = place else {
        todo!("global places: globals are not lowered yet");
    };
    let MIRBindable::Place(place) = *context.slot(slot) else {
        unreachable!("only a place has an address");
    };

    if matches!(
        context.env().resolve_type_id(ty).kind(),
        HMIRTypeKind::ReferenceTo(_)
    ) {
        return Ok(MIRValue::PlaceRef(place));
    }

    let out = register(context, ty, span)?;
    intrinsic(
        context.builder_mut(),
        MIRInternalIntrinsic::PlaceAddress {
            out: MIRTarget::Register(out),
            place,
        },
        span.clone(),
    );

    Ok(MIRValue::Register(out))
}

pub fn read(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if target(context, &value).is_none() {
        return Ok(value);
    }

    let ty = context.env_mut().lower_type(value.ty(), span)?;
    let source = materialize(context, &value, span)?;
    let out = register(context, value.ty(), span)?;

    store(
        context.builder_mut(),
        MIRTarget::Register(out),
        source,
        ty,
        span.clone(),
    );

    Ok(context.bind(value.ty(), MIRBindable::Register(out)))
}

pub fn spill(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if target(context, &value).is_some() {
        return Ok(value);
    }

    let ty = context.env_mut().lower_type(value.ty(), span)?;
    let source = materialize(context, &value, span)?;
    let scope = *context
        .open_scopes()
        .last()
        .expect("Value spilled outside of a scope");
    let place = context
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(ty, None, scope, false, false));

    store(
        context.builder_mut(),
        MIRTarget::Place(place),
        source,
        ty,
        span.clone(),
    );
    context.builder_mut().emit(
        MIRInstructionKind::Initialize {
            place: MIRBindable::Place(place),
        },
        span.clone(),
    );

    Ok(context.bind(value.ty(), MIRBindable::Place(place)))
}

pub fn referent(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let Some(inner) = context.env().reference_inner(&value.ty()) else {
        return Ok(value);
    };

    let reference = read(context, value, span)?;
    Ok(HMIRValue::indirect(inner, reference))
}

pub fn open_scope(context: &mut FnLoweringContext, span: TokenRange) -> MIRScopeID {
    let scope = context.builder_mut().add_scope(span);
    context.push_scope(scope);
    scope
}

pub fn close_scope(context: &mut FnLoweringContext, span: TokenRange) {
    let scope = context.pop_scope();

    if !terminated(context.builder()) {
        drop_scope(context.builder_mut(), scope, span);
    }
}

pub fn drop_scope(builder: &mut MIRFunctionBuilder, scope: MIRScopeID, span: TokenRange) {
    let places = builder
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        builder.emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(place),
                kind: MIRInvalidationKind::Drop,
            },
            span.clone(),
        );
    }
}
