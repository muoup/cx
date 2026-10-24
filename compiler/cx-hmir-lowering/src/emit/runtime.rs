mod op;
mod storage;
mod global;

pub use op::{binary, cast, difference, offset, unary};
pub use storage::{allocate, copy, initialize, invalidate, lift, parameter, scratch, write};

use cx_hmir::ty::{
    HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    intrinsic::HMIRIntWidth,
};
use cx_log::CXResult;
use cx_mir::{
    MIRAggregateIntrinsic, MIRBasicBlockID, MIRBindable, MIRBlockTarget, MIRConstant,
    MIRInstructionKind, MIRInternalIntrinsic, MIRIntrinsic, MIRPlaceID, MIRPtrIntrinsic,
    MIRRegister, MIRRegisterDecl, MIRScopeID, MIRTarget, MIRTypeID, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use self::storage::slot;
use crate::{
    env::{
        HMIREnvironment,
        function::{LocalLoweringContext, runtime::Emission},
    },
    eval::{
        constructor,
        types::{self, mir_float, mir_int},
    },
    value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
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
    if runtime.emission() != Emission::Unevaluated {
        runtime.set_emission(Emission::Enabled);
    }
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

pub fn assert(
    context: &mut LocalLoweringContext,
    condition: MIRValue,
    message: &str,
    span: &TokenRange,
) {
    let message = Some(message.into());
    intrinsic(context, MIRInternalIntrinsic::Assert { condition, message }, span.clone())
}

pub fn assume(context: &mut LocalLoweringContext, condition: MIRValue, span: &TokenRange) {
    intrinsic(context, MIRInternalIntrinsic::Assume { condition }, span.clone())
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

// A fresh register for the result of an operation, which holds it once the operation that writes it is emitted
fn output(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, HMIRValue)> {
    let register = register(context, ty, span)?;

    Ok((
        MIRTarget::Register(register),
        slot(context, ty, MIRBindable::Register(register)),
    ))
}

pub fn constant(
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
        HMIRConstant::Address { place: HMIRPlace::Global(def), path, ty } => {
            let inner = env
                .reference_inner(*ty)
                .or_else(|| env.pointer_inner(*ty))
                .expect("a global address has pointer or reference type");
            MIRConstant::GlobalRef(global::address(env, *def, path, inner, span)?)
        }
        HMIRConstant::Aggregate { ty, fields } => MIRConstant::Aggregate {
            ty: types::lower_type(env, *ty, span)?,
            fields: fields
                .iter()
                .map(|(index, field)| Ok((*index, self::constant(env, field, span)?)))
                .collect::<CXResult<_>>()?,
        },
        HMIRConstant::Function {
            function: HMIRFunctionRef::Constructor { ty, variant },
            ..
        } => MIRConstant::Function(constructor::declare(env, *ty, *variant, span)?),
        HMIRConstant::Function {
            function: function @ HMIRFunctionRef::Def(def),
            args,
        } => {
            if let Some(function) = env.generation().generated(*function, args) {
                return Ok(MIRConstant::Function(function));
            }
            let prototype = types::lower_prototype(env, *def, None, span)?;
            if def.brand() == env.symbols().local_unit().id()
                || prototype.linkage == LinkageMode::Static
            {
                env.generation_mut().request(*def);
            }
            MIRConstant::Function(env.module_mut().declare_function(prototype))
        }
        other => todo!("constant {other:?}"),
    })
}

// Names the storage of a value rather than what it holds, where it is behind a reference
pub fn lower_value(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
            place_address(context, *place, path, value.ty(), span)?
        }
        HMIRValueKind::Constant(function @ HMIRConstant::Function { .. })
            if context.env().pointer_inner(value.ty()).is_some() =>
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
        HMIRValueKind::PlaceRef(HMIRPlace::Global(def)) => MIRValue::Constant(MIRConstant::GlobalRef(
            global::address(context.env_mut(), *def, &[], value.ty(), span)?,
        )),
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
    let slot = match place {
        HMIRPlace::Local(slot) => {
            if !path.is_empty() {
                todo!("address of a compile-time local subobject at runtime");
            }
            slot
        }
        HMIRPlace::Global(def) => {
            let inner = context
                .env()
                .reference_inner(ty)
                .or_else(|| context.env().pointer_inner(ty))
                .expect("an address is a pointer or reference");
            let global = global::address(context.env_mut(), def, path, inner, span)?;
            if context.env().is_reference(ty) {
                return Ok(MIRValue::Constant(MIRConstant::GlobalRef(global)));
            }
            let out = register(context, ty, span)?;
            intrinsic(context, MIRInternalIntrinsic::GlobalAddress { out: MIRTarget::Register(out), global }, span.clone());
            return Ok(MIRValue::Register(out));
        }
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
        HMIRValueKind::Constant(function @ HMIRConstant::Function { .. }) => {
            MIRValue::Constant(constant(context.env_mut(), function, span)?)
        }
        _ => lower_value(context, callee, span)?,
    };
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

    Ok(value)
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

// Drops what the scopes opened past the first 'depth' hold, without closing them
pub fn drop_scopes(
    context: &mut LocalLoweringContext,
    depth: usize,
    span: &TokenRange,
) {
    let scopes = context.runtime_mut().open_scopes()[depth..].to_vec();

    for scope in scopes.into_iter().rev() {
        drop_scope(context, scope, span);
    }
}

fn drop_scope(
    context: &mut LocalLoweringContext,
    scope: MIRScopeID,
    span: &TokenRange,
) {
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

pub fn address(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let reference = match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address { place, path, .. }) => {
            return Ok(HMIRValue::constant(
                ty,
                HMIRConstant::Address {
                    place: *place,
                    path: path.clone(),
                    ty,
                },
            ));
        }
        HMIRValueKind::PlaceRef(place) => match place {
            HMIRPlace::Local(slot) => match context.runtime().slot(*slot) {
                MIRBindable::Place(_) => {
                    return Ok(HMIRValue::constant(
                        ty,
                        HMIRConstant::Address {
                            place: *place,
                            path: Vec::new(),
                            ty,
                        },
                    ));
                }
                MIRBindable::Register(_) if context.env().is_reference(value.ty()) => value,
                MIRBindable::Register(_) => unreachable!("a register has no address"),
            },
            HMIRPlace::Global(_) => {
                return Ok(HMIRValue::constant(ty, HMIRConstant::Address {
                    place: *place, path: Vec::new(), ty,
                }));
            }
        },
        HMIRValueKind::Indirect(pointer) => pointer,
        HMIRValueKind::Constant(_) => unreachable!("only storage has an address"),
    };

    let reference = lower_value(context, reference, span)?;
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
    let value = lower_value(context, &pointer, span)?;

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
    assert(
        context,
        MIRValue::Register(condition),
        "dereferenced a null pointer",
        span,
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
    let stored = context.env().reference_inner(base.ty()).unwrap_or(base.ty());
    let tagged = matches!(
        context.env().resolve_kind(stored),
        HMIRTypeKind::TaggedUnion { .. }
    );
    let aggregate_ty = types::lower_type(context.env_mut(), stored, span)?;
    let base = lower_value(context, base, span)?;

    let reference_ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::ReferenceTo(ty),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    let (out, reference) = output(context, reference_ty, span)?;
    let projection = if tagged {
        MIRAggregateIntrinsic::SumVariant {
            out,
            base,
            variant: index,
            sum_ty: aggregate_ty,
        }
    } else {
        MIRAggregateIntrinsic::StructField {
            out,
            base,
            field: index,
            struct_ty: aggregate_ty,
        }
    };
    intrinsic(context, projection, span.clone());
    initialize(context, &reference, span);

    Ok(HMIRValue::indirect(ty, reference))
}

pub fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    if let HMIRValueKind::Constant(constant) = value.kind()
        && let Some(tag) = constant.tag(ty)
    {
        return Ok(HMIRValue::constant(ty, tag));
    }

    let stored = context
        .env()
        .reference_inner(value.ty())
        .unwrap_or(value.ty());
    let sum_ty = types::lower_type(context.env_mut(), stored, span)?;
    let value = lower_value(context, value, span)?;
    let (out, result) = output(context, ty, span)?;
    intrinsic(
        context,
        MIRAggregateIntrinsic::SumIndex { out, value, sum_ty },
        span.clone(),
    );
    initialize(context, &result, span);

    Ok(result)
}

pub fn aggregate(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    fields: &[(usize, HMIRValue)],
    span: &TokenRange,
) -> CXResult<HMIRValue> {
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

    Ok(result)
}
