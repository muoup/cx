use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRConstant, MIRInstructionKind, MIRInternalIntrinsic, MIRRegister, MIRTarget,
    MIRValue,
};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use super::{intrinsic, register};
use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext}, log::hmir_error, lower::{
        self, constructor, global, types::{lower_prototype, lower_type, mir_float, mir_int},
    }, value::{
        HMIRPlace, HMIRValue, HMIRValueKind,
        address::Addressable,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub fn value(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Constant(HMIRConstant::Address(address))
            if let Addressable::Place(HMIRPlace::Local(slot)) = address.base() =>
        {
            if address.offset() != 0 {
                return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
            }
            let MIRBindable::Place(place) = *context.runtime().slot(*slot).binding() else {
                unreachable!("only a place has an address");
            };
            MIRValue::Register(produce(context, value.ty(), span, |out| {
                MIRInternalIntrinsic::PlaceAddress { out, place }
            })?)
        }
        HMIRValueKind::Constant(function @ (HMIRConstant::Function(_) | HMIRConstant::Curried { .. }))
            if context.env().pointer_inner(value.ty()).is_some() =>
        {
            let MIRConstant::Function(fn_id) = constant(context.env_mut(), function, value.ty(), span)? else {
                unreachable!("function constant lowered to a non-function")
            };
            MIRValue::Register(produce(context, value.ty(), span, |out| {
                MIRInternalIntrinsic::GetFnPtr { out, fn_id }
            })?)
        }
        HMIRValueKind::Constant(constant) => {
            MIRValue::Constant(self::constant(context.env_mut(), constant, value.ty(), span)?)
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Local(slot)) => match *context.runtime().slot(*slot).binding() {
            MIRBindable::Register(register) => MIRValue::Register(register),
            MIRBindable::Place(place) => {
                assert!(context.env().is_reference(value.ty()), "storage must be captured before it is an operand");
                MIRValue::Register(produce(context, value.ty(), span, |out| {
                    MIRInternalIntrinsic::PlaceAddress { out, place }
                })?)
            }
        },
        HMIRValueKind::PlaceRef(HMIRPlace::Global(def)) => {
            let inner = context.env().reference_inner(value.ty()).expect("global storage is a reference");
            let global = global::address(context.env_mut(), *def, 0, inner, span)?;
            MIRValue::Register(produce(context, value.ty(), span, |out| {
                MIRInternalIntrinsic::GlobalAddress { out, global }
            })?)
        }
        HMIRValueKind::PlaceRef(HMIRPlace::Region(_)) => {
            return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
        }
        HMIRValueKind::Indirect(pointer) if context.env().is_reference(value.ty()) => {
            lower::mir::value(context, pointer, span)?
        }
        HMIRValueKind::Indirect(pointer) => MIRValue::Indirect {
            register: reference(context, pointer, span)?,
            ty: lower_type(context.env_mut(), value.ty(), span)?,
        },
    })
}

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

pub fn reference(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRRegister> {
    let ty = value.ty();
    let value = lower::mir::value(context, value, span)?;
    if let MIRValue::Register(register) = value {
        return Ok(register);
    }
    let out = register(context, ty, span)?;
    let ty = lower_type(context.env_mut(), ty, span)?;
    context.runtime_mut().emit(
        MIRInstructionKind::Store { target: MIRTarget::Register(out), value, ty, bitfield: None },
        span.clone(),
    );
    Ok(out)
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
