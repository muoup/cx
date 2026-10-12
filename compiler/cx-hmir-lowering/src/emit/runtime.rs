mod op;

pub use op::{binary, cast, difference, offset, unary};

use cx_hmir::{
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::{HMIRPlace, HMIRValue, HMIRValueKind},
};
use cx_log::CXResult;
use cx_mir::{
    MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRConstant, MIRInstructionKind,
    MIRInternalIntrinsic, MIRIntrinsic, MIRPlaceDecl, MIRPlaceID, MIRPtrIntrinsic, MIRRegister,
    MIRRegisterDecl, MIRScopeID, MIRTarget, MIRTypeID, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit::{self, runtime}, env::{
        HMIREnvironment,
        function::{LocalLoweringContext, runtime::Emission},
    }, eval::types::{self, mir_float, mir_int},
};

pub fn block(context: &mut LocalLoweringContext, name: &str) -> MIRBasicBlockID {
    context
        .runtime_mut()
        .builder_mut()
        .add_block(Some(CXIdent::new(name)))
}

pub fn current_block(context: &mut LocalLoweringContext) -> MIRBasicBlockID {
    context.runtime().builder().current_block()
}

pub fn switch_to(context: &mut LocalLoweringContext, block: MIRBasicBlockID) {
    let runtime = context.runtime_mut();

    runtime.builder_mut().switch_to(block);
    runtime.set_emission(Emission::Enabled);
}

pub fn block_param(
    context: &mut LocalLoweringContext,
    block: MIRBasicBlockID,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let register = runtime::register(context, ty, span)?;
    let runtime = context.runtime_mut();

    runtime.builder_mut().add_block_param(block, register);
    Ok(runtime.bind(ty, MIRBindable::Register(register)))
}

pub fn jump(
    context: &mut LocalLoweringContext,
    target: MIRBasicBlockID,
    args: Vec<MIRValue>,
    span: TokenRange,
) {
    context.runtime_mut().emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::with_args(target, args),
        },
        span,
    )
}

pub fn branch(
    context: &mut LocalLoweringContext,
    cond: MIRValue,
    true_target: MIRBlockTarget,
    false_target: MIRBlockTarget,
    span: TokenRange,
) {
    context.runtime_mut().emit(
        MIRInstructionKind::Branch {
            cond,
            true_target,
            false_target,
        },
        span,
    )
}

fn intrinsic(
    context: &mut LocalLoweringContext,
    intrinsic: impl Into<MIRIntrinsic>,
    span: TokenRange,
) {
    context
        .runtime_mut()
        .emit(MIRInstructionKind::IntrinsicOp(intrinsic.into()), span)
}

fn store(
    context: &mut LocalLoweringContext,
    target: MIRTarget,
    value: MIRValue,
    ty: MIRTypeID,
    span: TokenRange,
) {
    context.runtime_mut().emit(
        MIRInstructionKind::Store {
            target,
            value,
            ty,
            bitfield: None,
        },
        span,
    )
}

fn register(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let ty = types::lower_type(context.env_mut(), ty, span)?;

    Ok(context
        .runtime_mut()
        .builder_mut()
        .allocate_register(MIRRegisterDecl::new(ty, None)))
}

fn output(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, HMIRValue)> {
    let register = register(context, ty, span)?;
    let value = context
        .context_mut()
        .runtime_mut()
        .bind(ty, MIRBindable::Register(register));

    Ok((MIRTarget::Register(register), value))
}

fn constant(
    env: &mut HMIREnvironment,
    constant: &HMIRConstant,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(match constant {
        HMIRConstant::Unit => MIRConstant::Unit,
        HMIRConstant::Int { value, ty } => match env.resolve_type_id(*ty).kind() {
            HMIRTypeKind::Int(int) => MIRConstant::Integer {
                ty: mir_int(int.width()),
                value: *value,
            },
            other => todo!("integer constant of type {other:?}"),
        },
        HMIRConstant::Float { value, ty } => match env.resolve_type_id(*ty).kind() {
            HMIRTypeKind::Float(float) => MIRConstant::Float {
                value: *value,
                ty: mir_float(float.width()),
            },
            other => todo!("float constant of type {other:?}"),
        },
        HMIRConstant::Null(ty) => MIRConstant::Nullptr {
            ty: types::lower_type(env, *ty, span)?,
        },
        HMIRConstant::Str(value) => MIRConstant::String(value.clone()),
        other => todo!("constant {other:?}"),
    })
}

pub fn materialize(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
            place_address(context, *place, path, value.ty(), span)?
        }
        HMIRValueKind::Constant(value) => {
            MIRValue::Constant(constant(context.env_mut(), value, span)?)
        }
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => {
            match context.context_mut().runtime().slot(*slot) {
                MIRBindable::Register(register) => MIRValue::Register(*register),
                MIRBindable::Place(place) => MIRValue::PlaceRef(*place),
            }
        }
        HMIRValueKind::Place(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Indirect(pointer) => MIRValue::Register(reference(context, pointer)),
    })
}

fn target(context: &mut LocalLoweringContext, value: &HMIRValue) -> Option<MIRTarget> {
    match value.kind() {
        HMIRValueKind::Constant(_) => None,
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => {
            match context.context_mut().runtime().slot(*slot) {
                MIRBindable::Register(_) => None,
                MIRBindable::Place(place) => Some(MIRTarget::Place(*place)),
            }
        }
        HMIRValueKind::Place(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Indirect(pointer) => Some(MIRTarget::Indirect(reference(context, pointer))),
    }
}

fn reference(context: &mut LocalLoweringContext, pointer: &HMIRValue) -> MIRRegister {
    match pointer.kind() {
        HMIRValueKind::Place(HMIRPlace::Local(slot)) => {
            match context.context_mut().runtime().slot(*slot) {
                MIRBindable::Register(register) => *register,
                MIRBindable::Place(_) => {
                    unreachable!("an indirect value addresses through a register")
                }
            }
        }
        _ => todo!("indirect value through a subobject or global address"),
    }
}

fn place_address(
    context: &mut LocalLoweringContext,
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
    let MIRBindable::Place(place) = *context.context_mut().runtime().slot(slot) else {
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
        context,
        MIRInternalIntrinsic::PlaceAddress {
            out: MIRTarget::Register(out),
            place,
        },
        span.clone(),
    );

    Ok(MIRValue::Register(out))
}

pub fn open_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    let runtime = context.runtime_mut();
    let scope = runtime.builder_mut().add_scope(span);

    runtime.push_scope(scope);
}

pub fn close_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    let scope = context.runtime_mut().pop_scope();

    drop_scope(context, scope, &span);
}

pub fn drop_scopes(context: &mut LocalLoweringContext, span: &TokenRange) {
    let scopes = context.runtime_mut().open_scopes().to_vec();

    for scope in scopes.into_iter().rev() {
        drop_scope(context, scope, span);
    }
}

fn drop_scope(context: &mut LocalLoweringContext, scope: MIRScopeID, span: &TokenRange) {
    let places = context
        .runtime()
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(_, place)| place.scope() == scope)
        .map(|(index, _)| MIRPlaceID::new(index))
        .collect::<Vec<_>>();

    for place in places.into_iter().rev() {
        context.runtime_mut().emit(
            MIRInstructionKind::Invalidate {
                place: MIRBindable::Place(place),
                kind: MIRInvalidationKind::Drop,
            },
            span.clone(),
        );
    }
}

fn place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    nodrop: bool,
    span: &TokenRange,
) -> CXResult<MIRPlaceID> {
    let ty = types::lower_type(context.env_mut(), ty, span)?;
    let runtime = context.runtime_mut();
    let scope = *runtime
        .open_scopes()
        .last()
        .expect("Place allocated outside of a scope");

    Ok(runtime
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(ty, name, scope, nodrop, false)))
}

pub fn allocate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    nodrop: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = place(context, ty, name, nodrop, span)?;

    Ok(context.runtime_mut().bind(ty, MIRBindable::Place(place)))
}

pub fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    nodrop: bool,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let place = place(context, ty, name, nodrop, span)?;
    let runtime = context.runtime_mut();

    runtime.builder_mut().add_parameter(place);
    runtime.emit(
        MIRInstructionKind::Initialize {
            place: MIRBindable::Place(place),
        },
        span.clone(),
    );

    Ok(context.runtime_mut().bind(ty, MIRBindable::Place(place)))
}

pub fn addressable(context: &mut LocalLoweringContext, value: &HMIRValue) -> bool {
    target(context, value).is_some()
}

pub fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if !addressable(context, &value) {
        return Ok(value);
    }

    let ty = types::lower_type(context.env_mut(), value.ty(), span)?;
    let source = runtime::materialize(context, &value, span)?;
    let (out, result) = runtime::output(context, value.ty(), span)?;
    runtime::store(context, out, source, ty, span.clone());

    Ok(result)
}

pub fn write(
    context: &mut LocalLoweringContext,
    target: &HMIRValue,
    value: HMIRValue,
    initialize: bool,
    span: &TokenRange,
) -> CXResult<()> {
    let Some(destination) = self::target(context, target) else {
        unreachable!("write to a value without storage");
    };
    let ty = types::lower_type(context.env_mut(), target.ty(), span)?;

    if initialize {
        let value = materialize(context, &value, span)?;
        emit::runtime::store(context, destination.clone(), value, ty, span.clone());

        if let MIRTarget::Place(place) = destination {
            let place = MIRBindable::Place(place);
            context
                .runtime_mut()
                .emit(MIRInstructionKind::Initialize { place }, span.clone());
        }

        return Ok(());
    }

    let value = read(context, value, span)?;
    let value = materialize(context, &value, span)?;

    if let MIRTarget::Place(place) = destination {
        let place = MIRBindable::Place(place);
        let runtime = context.runtime_mut();

        runtime.emit(
            MIRInstructionKind::Invalidate {
                place: place.clone(),
                kind: MIRInvalidationKind::Drop,
            },
            span.clone(),
        );
        runtime.emit(MIRInstructionKind::Initialize { place }, span.clone());
    }
    store(context, destination, value, ty, span.clone());

    Ok(())
}

pub fn address(
    context: &mut LocalLoweringContext,
    reference: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let reference = materialize(context, reference, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(
        context,
        MIRInternalIntrinsic::ReferenceAddress { out, reference },
        span.clone(),
    );

    Ok(result)
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = read(context, pointer, span)?;
    let value = materialize(context, &pointer, span)?;

    let null = MIRConstant::Nullptr {
        ty: types::lower_type(context.env_mut(), pointer.ty(), span)?,
    };
    let bool_ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let condition = register(context, bool_ty, span)?;
    intrinsic(
        context,
        MIRPtrIntrinsic::Neq {
            out: MIRTarget::Register(condition),
            lhs: value.clone(),
            rhs: MIRValue::Constant(null),
        },
        span.clone(),
    );
    intrinsic(
        context,
        MIRInternalIntrinsic::Assert {
            condition: MIRValue::Register(condition),
            message: Some("dereferenced a null pointer".into()),
        },
        span.clone(),
    );

    let reference_ty = context
        .env_mut()
        .resolver_mut()
        .realize_type(HMIRTypeKind::ReferenceTo(inner), span);
    let target_ty = types::lower_type(context.env_mut(), reference_ty, span)?;
    let (out, reference) = output(context, reference_ty, span)?;
    intrinsic(
        context,
        MIRInternalIntrinsic::Bitcast {
            out,
            value,
            target_ty,
        },
        span.clone(),
    );

    Ok(HMIRValue::indirect(inner, reference))
}
