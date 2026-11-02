use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRConstant, MIRGlobalRef, MIRInstructionKind, MIRInternalIntrinsic, MIRPlaceID, MIRRegister,
    MIRTarget, MIRTypeID, MIRTypeKind, MIRValue, ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use super::{intrinsic, register};
use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    log::hmir_error,
    lower::{
        constructor, global,
        types::{lower_prototype, lower_type, mir_float, mir_int},
    },
    value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        address::Addressable,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub fn constant(
    env: &mut HMIREnvironment,
    constant: &HMIRConstant,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(match (constant, env.resolve_kind(ty).clone()) {
        (HMIRConstant::Unit, _) => MIRConstant::Unit,
        (HMIRConstant::Int(value), HMIRTypeKind::Int(int)) => MIRConstant::Integer {
            ty: mir_int(int.width()),
            value: *value,
        },
        (HMIRConstant::Float(value), HMIRTypeKind::Float(float)) => MIRConstant::Float {
            value: *value,
            ty: mir_float(float.width()),
        },
        (HMIRConstant::Null, _) => MIRConstant::Nullptr {
            ty: lower_type(env, ty, span)?,
        },
        (HMIRConstant::Str(value), _) => MIRConstant::String {
            value: value.clone(),
            offset: 0,
        },
        (HMIRConstant::Address(address), _) => match address.base() {
            Addressable::Place(HMIRPlace::Global(def)) => {
                let inner = env
                    .reference_inner(ty)
                    .or_else(|| env.pointer_inner(ty))
                    .expect("a global address has pointer or reference type");
                MIRConstant::GlobalAddress(global::address(env, *def, address.offset(), inner, span)?)
            }
            Addressable::Label { function, label } => MIRConstant::BlockAddress {
                function: *function,
                label: label.to_string(),
            },
            Addressable::Literal(value) => MIRConstant::String {
                value: value.clone(),
                offset: address.offset(),
            },
            Addressable::Place(_) => {
                return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
            }
        },
        (HMIRConstant::Aggregate(items), kind) => {
            let mut fields = Vec::new();
            for (index, item) in items.iter().enumerate() {
                let field = match &kind {
                    HMIRTypeKind::Array { element, .. } => *element,
                    HMIRTypeKind::Struct { fields } => *fields[index].ty(),
                    _ => unreachable!("an aggregate constant is an array or structure"),
                };
                if !item.is_zero() {
                    fields.push((index, self::constant(env, item, field, span)?));
                }
            }
            MIRConstant::Aggregate {
                ty: lower_type(env, ty, span)?,
                fields,
            }
        }
        (
            HMIRConstant::Union { member, value }
            | HMIRConstant::TaggedUnion {
                variant: member,
                payload: value,
            },
            HMIRTypeKind::Union { fields } | HMIRTypeKind::TaggedUnion { fields },
        ) => MIRConstant::Aggregate {
            ty: lower_type(env, ty, span)?,
            fields: vec![(
                *member,
                self::constant(env, value, *fields[*member].ty(), span)?,
            )],
        },
        (HMIRConstant::Function(HMIRFunctionRef::Constructor { ty, variant }), _) => {
            MIRConstant::Function(constructor::declare(env, *ty, *variant, span)?)
        }
        (HMIRConstant::Function(HMIRFunctionRef::Def(def)), _) => {
            let prototype = lower_prototype(env, *def, None, span)?;
            if def.brand() == env.symbols().local_unit().id()
                || prototype.linkage == LinkageMode::Static
            {
                env.generation_mut().request(*def);
            }
            MIRConstant::Function(env.module_mut().declare_function(prototype))
        }
        (HMIRConstant::Curried { callee, args }, _) => {
            let args = args.iter().flatten().cloned().collect::<Vec<_>>();
            if let HMIRValueKind::Constant(HMIRConstant::Function(function)) = callee.kind()
                && let Some(function) = env.generation().generated(*function, &args)
            {
                return Ok(MIRConstant::Function(function));
            }
            return Err(hmir_error(span, &mir::COMPTIME_INVALID_OPERATION,
                "lowering an unresolved curried callable to runtime".into()));
        }
        (other, _) => todo!("constant {other:?}"),
    })
}

#[derive(Clone, Copy)]
enum Storage {
    Place { place: MIRPlaceID, ty: MIRTypeID },
    Global(MIRGlobalRef),
    Pointee { register: MIRRegister, ty: MIRTypeID },
}

impl Storage {
    fn ty(self) -> MIRTypeID {
        match self {
            Storage::Place { ty, .. } | Storage::Pointee { ty, .. } => ty,
            Storage::Global(global) => global.ty,
        }
    }

    fn typed(self, ty: MIRTypeID) -> Self {
        match self {
            Storage::Place { place, .. } => Storage::Place { place, ty },
            Storage::Global(global) => Storage::Global(MIRGlobalRef { ty, ..global }),
            Storage::Pointee { register, .. } => Storage::Pointee { register, ty },
        }
    }
}

enum Designation {
    Value(MIRValue),
    Contents(Storage),
    Address(Storage),
}

// The runtime value of 'value', where a reference is the address of the storage it designates
pub fn operand(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    let designation = designate(context, value, span)?;
    resolve(context, designation, value.ty(), span)
}

// What a reference designates, read in place; any other value is its own contents
pub fn contents(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    match designate(context, value, span)? {
        Designation::Address(storage) => Ok(read(storage)),
        designation if let Some(inner) = context.env().reference_inner(value.ty()) => {
            let ty = lower_type(context.env_mut(), inner, span)?;
            let register = materialize(context, designation, value.ty(), span)?;
            Ok(MIRValue::Indirect { register, ty })
        }
        designation => resolve(context, designation, value.ty(), span),
    }
}

fn designate(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<Designation> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address(address)) => match (address.base(), address.offset()) {
            (Addressable::Place(HMIRPlace::Local(slot)), 0) => {
                let MIRBindable::Place(place) = *context.runtime().slot(*slot).binding() else {
                    unreachable!("only a place has an address");
                };
                let pointee = pointee(context, value.ty());
                let pointee = lower_type(context.env_mut(), pointee, span)?;
                let storage = Storage::Place { place, ty: context.runtime().builder().place(place).ty() };
                Designation::Address(retype(context, storage, pointee))
            }
            (Addressable::Place(HMIRPlace::Global(def)), offset) => {
                let pointee = pointee(context, value.ty());
                let global = global::address(context.env_mut(), *def, offset, pointee, span)?;
                Designation::Address(Storage::Global(global))
            }
            (Addressable::Label { .. } | Addressable::Literal(_), _) => {
                let address = HMIRConstant::Address(address.clone());
                Designation::Value(MIRValue::Constant(constant(context.env_mut(), &address, value.ty(), span)?))
            }
            _ => return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ())),
        },
        HMIRValueKind::Constant(function @ (HMIRConstant::Function(_) | HMIRConstant::Curried { .. }))
            if context.env().pointer_inner(value.ty()).is_some() =>
        {
            let MIRConstant::Function(fn_id) = constant(context.env_mut(), function, value.ty(), span)? else {
                unreachable!("function constant lowered to a non-function")
            };
            let pointer = produce(context, value.ty(), span, |out| MIRInternalIntrinsic::GetFnPtr { out, fn_id })?;
            Designation::Value(MIRValue::Register(pointer))
        }
        HMIRValueKind::Constant(constant) => {
            Designation::Value(MIRValue::Constant(self::constant(context.env_mut(), constant, value.ty(), span)?))
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => match *context.runtime().slot(*slot).binding() {
            MIRBindable::Register(register) => {
                let held = context.runtime().builder().register(register).ty();
                assert!(
                    context.env().is_reference(value.ty())
                        || !matches!(context.env().module().resolve_type(held).kind(), MIRTypeKind::MemoryReference { .. }),
                    "a reference register is read through an indirect value, never as a plain slot"
                );
                Designation::Value(MIRValue::Register(register))
            }
            MIRBindable::Place(place) => {
                let storage = Storage::Place { place, ty: context.runtime().builder().place(place).ty() };
                view(context, storage, value.ty(), span)?
            }
        },
        HMIRValueKind::PlaceRef(HMIRPlace::Global(def)) => {
            Designation::Contents(Storage::Global(global::address(context.env_mut(), *def, 0, value.ty(), span)?))
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Region(_)) => {
            return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
        }
        HMIRValueKind::Indirect(pointer) => {
            let storage = match designate(context, pointer, span)? {
                Designation::Address(storage) => storage,
                designation => {
                    let register = materialize(context, designation, pointer.ty(), span)?;
                    Storage::Pointee { register, ty: register_pointee(context, register) }
                }
            };
            view(context, storage, value.ty(), span)?
        }
    })
}

fn pointee(context: &LocalLoweringContext, ty: HMIRTypeID) -> HMIRTypeID {
    let env = context.env();
    env.reference_inner(ty)
        .or_else(|| env.pointer_inner(ty))
        .expect("an address is a pointer or reference")
}

// A value of type 'ty' over storage is its contents, or its address when 'ty' references what the storage holds or
// what it is viewed as
fn view(
    context: &mut LocalLoweringContext,
    storage: Storage,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Designation> {
    let lowered = lower_type(context.env_mut(), ty, span)?;
    if context.env().module().same_type(lowered, storage.ty()) {
        return Ok(Designation::Contents(storage));
    }
    Ok(match context.env().reference_inner(ty) {
        Some(inner) => {
            let inner = lower_type(context.env_mut(), inner, span)?;
            Designation::Address(retype(context, storage, inner))
        }
        None => Designation::Contents(storage.typed(lowered)),
    })
}

fn retype(context: &LocalLoweringContext, storage: Storage, ty: MIRTypeID) -> Storage {
    match context.env().module().same_type(ty, storage.ty()) {
        true => storage,
        false => storage.typed(ty),
    }
}

fn register_pointee(context: &LocalLoweringContext, register: MIRRegister) -> MIRTypeID {
    let pointer = context.runtime().builder().register(register).ty();
    match context.env().module().resolve_type(pointer).kind() {
        MIRTypeKind::MemoryReference { inner } | MIRTypeKind::PointerTo { inner } => *inner,
        _ => unreachable!("an indirect value addresses through a pointer"),
    }
}

fn resolve(
    context: &mut LocalLoweringContext,
    designation: Designation,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match designation {
        Designation::Value(value) => value,
        Designation::Contents(storage) => read(storage),
        Designation::Address(storage) => MIRValue::Register(address(context, storage, ty, span)?),
    })
}

fn read(storage: Storage) -> MIRValue {
    match storage {
        Storage::Place { place, ty } => MIRValue::PlaceRef { place, ty },
        Storage::Global(global) => MIRValue::GlobalRef(global),
        Storage::Pointee { register, ty } => MIRValue::Indirect { register, ty },
    }
}

fn address(
    context: &mut LocalLoweringContext,
    storage: Storage,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    match storage {
        Storage::Place { place, .. } => {
            produce(context, ty, span, |out| MIRInternalIntrinsic::PlaceAddress { out, place })
        }
        Storage::Global(global) => produce(context, ty, span, |out| MIRInternalIntrinsic::GlobalAddress { out, global }),
        Storage::Pointee { register, ty: viewed } => {
            if context.env().module().same_type(viewed, register_pointee(context, register)) {
                return Ok(register);
            }
            let target_ty = lower_type(context.env_mut(), ty, span)?;
            let value = MIRValue::Register(register);
            produce(context, ty, span, |out| MIRInternalIntrinsic::Bitcast { out, value, target_ty })
        }
    }
}

// Holds a pointer or reference in a register, so that what it addresses can be read through it
fn materialize(
    context: &mut LocalLoweringContext,
    designation: Designation,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let value = resolve(context, designation, ty, span)?;
    if let MIRValue::Register(register) = value {
        return Ok(register);
    }
    let out = register(context, ty, span)?;
    let stored = lower_type(context.env_mut(), ty, span)?;
    let store = MIRInstructionKind::Store { target: MIRTarget::Register(out), value, ty: stored, bitfield: None };
    context.runtime_mut().emit(store, span.clone());
    Ok(out)
}

pub(super) fn reference(context: &LocalLoweringContext, pointer: &HMIRValue) -> MIRRegister {
    match pointer.kind() {
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => {
            match context.runtime().slot(*slot).binding() {
                MIRBindable::Register(register) => *register,
                MIRBindable::Place(_) => {
                    unreachable!("an indirect value addresses through a register")
                }
            }
        }
        _ => todo!("indirect value through a subobject or global address"),
    }
}

fn produce(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
    instruction: impl FnOnce(MIRTarget) -> MIRInternalIntrinsic,
) -> CXResult<MIRRegister> {
    let out = register(context, ty, span)?;
    intrinsic(context, instruction(MIRTarget::Register(out)), span);
    Ok(out)
}
