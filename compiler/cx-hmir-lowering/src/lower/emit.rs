// Might have gone a bit overboard removing the old emit.rs, we should however make this thinner and just for helpers;
// ensure also that we call this via emit::method_name rather than naming it emit_method_name
//
// Avoid any routines in here that are hyper-specific and would only feasibly be used in one place

use cx_hmir::{
    ty::{HMIRTypeID, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlock, MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRInstruction,
    MIRInstructionKind, MIRIntrinsic, MIRPlaceDecl, MIRPlaceID, MIRRegister, MIRRegisterDecl,
    MIRScopeID, MIRTarget, MIRTypeID, MIRValue, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::env::{function::MIRFunctionBuilder, lowering::FnLoweringContext};

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

pub fn read(
    context: &mut FnLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if context.target(&value).is_none() {
        return Ok(value);
    }

    let ty = context.env_mut().lower_type(value.ty(), span)?;
    let source = context.materialize(&value, span)?;
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
    if context.target(&value).is_some() {
        return Ok(value);
    }

    let ty = context.env_mut().lower_type(value.ty(), span)?;
    let source = context.materialize(&value, span)?;
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
