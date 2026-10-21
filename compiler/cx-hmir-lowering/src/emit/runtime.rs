mod op;
mod storage;

pub use op::{binary, cast, difference, offset, unary};
pub use storage::{
    allocate, initialize, invalidate, parameter, copy, relocate, write,
};

use cx_hmir::{
    HMIRDefKind,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRConstant,
    MIRInstructionKind, MIRInternalIntrinsic, MIRIntrinsic, MIRPlaceID, MIRPtrIntrinsic,
    MIRRegister, MIRRegisterDecl, MIRScopeID, MIRTarget, MIRTypeID, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use self::storage::{slot, target};
use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{LocalLoweringContext, runtime::Emission},
    },
    eval::types::{self, mir_float, mir_int},
    value::{HMIRPlace, HMIRValue, HMIRValueKind, constant::HMIRConstant},
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

pub fn jump(context: &mut LocalLoweringContext, target: MIRBasicBlockID, span: TokenRange) {
    context.runtime_mut().emit(
        MIRInstructionKind::Jump {
            target: MIRBlockTarget::new(target),
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

fn in_memory(context: &LocalLoweringContext, ty: HMIRTypeID) -> bool {
    matches!(
        context.env().resolve_kind(ty),
        HMIRTypeKind::Struct { .. }
            | HMIRTypeKind::Union { .. }
            | HMIRTypeKind::TaggedUnion { .. }
            | HMIRTypeKind::Array { .. }
            | HMIRTypeKind::Opaque { .. }
    )
}

// Selects where an operation writes its result: fresh storage for an aggregate, a register otherwise. Nothing is
// initialized, so the caller marks the result initialized once the operation that writes it is emitted
fn output(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, HMIRValue)> {
    if in_memory(context, ty) {
        let value = allocate(context, ty, None, span)?;
        return Ok((
            target(context, &value).expect("allocated output has storage"),
            value,
        ));
    }
    let register = register(context, ty, span)?;

    Ok((
        MIRTarget::Register(register),
        slot(context, ty, MIRBindable::Register(register)),
    ))
}

fn constant(
    env: &mut HMIREnvironment,
    constant: &HMIRConstant,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(match constant {
        HMIRConstant::Unit => MIRConstant::Unit,
        HMIRConstant::Int { value, ty } => match env.resolve_kind(*ty) {
            HMIRTypeKind::Int(int) => MIRConstant::Integer {
                ty: mir_int(int.width()),
                value: *value,
            },
            other => todo!("integer constant of type {other:?}"),
        },
        HMIRConstant::Float { value, ty } => match env.resolve_kind(*ty) {
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
        HMIRConstant::Function { def, args } if args.is_empty() => {
            let HMIRDefKind::Function(function) = env.symbols().resolve_definition(*def).kind()
            else {
                unreachable!("function constant of a definition that is not a function")
            };
            let name = function.prototype().symbol_name().as_str();

            MIRConstant::Function(
                env.module()
                    .declared_function(name)
                    .expect("Function referenced before its declaration"),
            )
        }
        other => todo!("constant {other:?}"),
    })
}

// Reads a value that is behind a pointer into storage of its own, leaving any other value as it is
fn detach(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match value.kind() {
        HMIRValueKind::Indirect(_) => copy(context, value, span),
        _ => Ok(value),
    }
}

pub fn materialize(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    let value = detach(context, value.clone(), span)?;

    lower_value(context, &value, span)
}

// Moves a value that a register produced into a place of the current scope when it is an aggregate, which is kept
// in memory, or has move semantics, whose obligations only the cleanup of a scope discharges
pub fn settle(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if !in_memory(context, value.ty()) && !context.env().is_nocopy(value.ty()) {
        return Ok(value);
    }

    emit::spill(context, value, span)
}

// Names the storage of a value rather than what it holds, where it is behind a reference
fn lower_value(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
            place_address(context, *place, path, value.ty(), span)?
        }
        HMIRValueKind::Constant(function @ HMIRConstant::Function { args, .. })
            if args.is_empty() && context.env().pointer_inner(value.ty()).is_some() =>
        {
            let MIRConstant::Function(fn_id) = constant(context.env_mut(), function, span)? else {
                unreachable!("function constant lowered to a non-function")
            };
            let out = register(context, value.ty(), span)?;
            intrinsic(
                context,
                MIRInternalIntrinsic::GetFnPtr {
                    out: MIRTarget::Register(out),
                    fn_id,
                },
                span.clone(),
            );
            MIRValue::Register(out)
        }
        HMIRValueKind::Constant(value) => {
            MIRValue::Constant(constant(context.env_mut(), value, span)?)
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => {
            match context.context_mut().runtime().slot(*slot) {
                MIRBindable::Register(register) => MIRValue::Register(*register),
                MIRBindable::Place(place) => MIRValue::PlaceRef(*place),
            }
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Global(_)) => {
            todo!("global places: globals are not lowered yet")
        }
        HMIRValueKind::Indirect(pointer) => MIRValue::Register(reference(context, pointer)),
    })
}

fn reference(context: &mut LocalLoweringContext, pointer: &HMIRValue) -> MIRRegister {
    match pointer.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => {
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

    if matches!(context.env().resolve_kind(ty), HMIRTypeKind::ReferenceTo(_)) {
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

pub fn call(
    context: &mut LocalLoweringContext,
    callee: &HMIRValue,
    args: &[HMIRValue],
    result: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let callee = match callee.kind() {
        HMIRValueKind::Constant(function @ HMIRConstant::Function { args, .. })
            if args.is_empty() =>
        {
            MIRValue::Constant(constant(context.env_mut(), function, span)?)
        }
        _ => materialize(context, callee, span)?,
    };
    let args = args
        .iter()
        .map(|arg| detach(context, arg.clone(), span))
        .collect::<CXResult<Vec<_>>>()?;
    let lowered_args = args
        .iter()
        .map(|arg| lower_value(context, arg, span))
        .collect::<CXResult<_>>()?;

    let returns = !matches!(
        context.env().resolve_kind(result),
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable
    );
    let (out, value) = if returns {
        let register = register(context, result, span)?;

        (Some(register), slot(context, result, MIRBindable::Register(register)))
    } else {
        (None, HMIRValue::constant(result, HMIRConstant::Unit))
    };

    context.runtime_mut().emit(
        MIRInstructionKind::Call {
            out,
            callee,
            args: lowered_args,
        },
        span.clone(),
    );
    for arg in &args {
        emit::consume(context, arg, span);
    }

    settle(context, value, span)
}

pub fn open_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    let runtime = context.runtime_mut();
    let scope = runtime.builder_mut().add_scope(span);

    runtime.push_scope(scope);
}

pub fn close_scope(context: &mut LocalLoweringContext, span: TokenRange) {
    let scope = context.runtime_mut().pop_scope();

    drop_scope(context, scope, None, &span);
}

// Drops what the scopes opened past the first 'depth' hold, without closing them
pub fn drop_scopes(
    context: &mut LocalLoweringContext,
    depth: usize,
    keep: Option<MIRPlaceID>,
    span: &TokenRange,
) {
    let scopes = context.runtime_mut().open_scopes()[depth..].to_vec();

    for scope in scopes.into_iter().rev() {
        drop_scope(context, scope, keep, span);
    }
}

fn drop_scope(
    context: &mut LocalLoweringContext,
    scope: MIRScopeID,
    keep: Option<MIRPlaceID>,
    span: &TokenRange,
) {
    let places = context
        .runtime()
        .builder()
        .places()
        .iter()
        .enumerate()
        .filter(|(index, place)| place.scope() == scope && keep != Some(MIRPlaceID::new(*index)))
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
    initialize(context, &result, span);

    Ok(result)
}

pub fn dereference(
    context: &mut LocalLoweringContext,
    pointer: HMIRValue,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = copy(context, pointer, span)?;
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

    let reference_ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::ReferenceTo(inner),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
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
    initialize(context, &reference, span);

    Ok(HMIRValue::indirect(inner, reference))
}

pub fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let struct_ty = types::lower_type(context.env_mut(), base.ty(), span)?;
    let base = lower_value(context, base, span)?;

    let reference_ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::ReferenceTo(ty),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    let (out, reference) = output(context, reference_ty, span)?;
    intrinsic(
        context,
        MIRAggregateIntrinsic::StructField {
            out,
            base,
            field: index,
            struct_ty,
        },
        span.clone(),
    );
    initialize(context, &reference, span);

    Ok(HMIRValue::indirect(ty, reference))
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let fields = fields
        .iter()
        .map(|(index, value)| Ok((*index, detach(context, value.clone(), span)?)))
        .collect::<CXResult<Vec<_>>>()?;
    let lowered_fields = fields
        .iter()
        .map(|(index, value)| Ok((*index, lower_value(context, value, span)?)))
        .collect::<CXResult<_>>()?;

    let mir_ty = types::lower_type(context.env_mut(), ty, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(
        context,
        MIRAggregateIntrinsic::AggregateInit {
            out,
            ty: mir_ty,
            fields: lowered_fields,
        },
        span.clone(),
    );
    initialize(context, &result, span);
    for (_, field) in &fields {
        emit::consume(context, field, span);
    }

    Ok(result)
}
