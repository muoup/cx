use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::{CXResult, catalogue::mir};
use cx_mir::{
    MIRBindable, MIRConstant, MIRInstructionKind, MIRInternalIntrinsic, MIRRegister, MIRTarget,
    MIRValue, constant::MIRAddressable,
};
use cx_tokens::TokenRange;
use cx_util::linkage::LinkageMode;

use super::{intrinsic, register};
use crate::{
    env::function::LocalLoweringContext,
    log::hmir_error,
    lower::{
        self, constructor, global,
        types::{lower_prototype, lower_type, mir_float, mir_int},
    },
    symbol,
    value::{
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
        HMIRValueKind::Constant(constant) => {
            MIRValue::Constant(self::constant(context, constant, value.ty(), span)?)
        }
        HMIRValueKind::PlaceRef(place) => match place {
            HMIRPlace::Local(slot) => match *context.runtime().slot(*slot).binding() {
                MIRBindable::Register(register) => MIRValue::Register(register),
                MIRBindable::Place(place) => {
                    assert!(
                        context.env().is_reference(value.ty()),
                        "storage must be captured before it is an operand"
                    );
                    MIRValue::Register(produce(context, value.ty(), span, |out| {
                        MIRInternalIntrinsic::PlaceAddress { out, place }
                    })?)
                }
            },

            HMIRPlace::Global(def) => {
                if context.env().is_reference(value.ty()) {
                    let object = symbol::value_type(context.env_mut(), *def, None)?;
                    let global = global::declare(context.env_mut(), *def, object, span)?;
                    MIRValue::Constant(MIRConstant::Address {
                        base: MIRAddressable::Global(global),
                        offset: 0,
                    })
                } else {
                    MIRValue::GlobalRef(global::reference(
                        context.env_mut(),
                        *def,
                        0,
                        value.ty(),
                        span,
                    )?)
                }
            }

            HMIRPlace::Region(_) => {
                // REVIEW: Perhaps this should incur a static memory allocation?
                return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
            }
        },
        HMIRValueKind::Indirect(pointer) => MIRValue::Indirect {
            register: reference(context, pointer, span)?,
            ty: lower_type(context.env_mut(), value.ty(), span)?,
        },
    })
}

pub fn constant(
    context: &mut LocalLoweringContext,
    constant: &HMIRConstant,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    Ok(match (constant, context.env_mut().resolve_kind(ty).clone()) {
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
            ty: lower_type(context.env_mut(), ty, span)?,
        },
        (HMIRConstant::Str(value), _) => MIRConstant::String {
            value: value.clone(),
            offset: 0,
        },
        (HMIRConstant::Address(address), _) => match address.base() {
            Addressable::Place(place) => match place {
                HMIRPlace::Global(def) => {
                    let object = symbol::value_type(context.env_mut(), *def, None)?;
                    let global_id = global::declare(context.env_mut(), *def, object, span)?;

                    MIRConstant::Address {
                        base: MIRAddressable::Global(global_id),
                        offset: address.offset(),
                    }
                }

                HMIRPlace::Local(slot) if context.is_runtime() => {
                    match context.runtime().slot(*slot).binding() {
                        MIRBindable::Place(place) => MIRConstant::Address {
                            base: MIRAddressable::Local(*place),
                            offset: address.offset(),
                        },
                        MIRBindable::Register(_) => {
                            return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ()));
                        }
                    }
                }
                _ => return Err(hmir_error(span, &mir::COMPTIME_VALUE_AT_RUNTIME, ())),
            },
            Addressable::Function(function) => {
                if address.offset() != 0 {
                    return Err(hmir_error(
                        span,
                        &mir::COMPTIME_INVALID_OPERATION,
                        "function address offset".into(),
                    ));
                }
                let HMIRValueKind::Constant(function_constant) = function.kind() else {
                    unreachable!("a function address names a function constant")
                };
                let MIRConstant::Function(function) =
                    self::constant(context, function_constant, function.ty(), span)?
                else {
                    unreachable!("a function address lowers from a function")
                };
                MIRConstant::Address {
                    base: MIRAddressable::Function(function),
                    offset: 0,
                }
            }
            Addressable::Label { function, label } => MIRConstant::BlockAddress {
                function: *function,
                label: label.to_string(),
            },
            Addressable::Literal(value) => MIRConstant::String {
                value: value.clone(),
                offset: address.offset(),
            },
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
                    fields.push((index, self::constant(context, item, field, span)?));
                }
            }
            MIRConstant::Aggregate {
                ty: lower_type(context.env_mut(), ty, span)?,
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
            ty: lower_type(context.env_mut(), ty, span)?,
            fields: vec![(
                *member,
                self::constant(context, value, *fields[*member].ty(), span)?,
            )],
        },
        (HMIRConstant::Function(HMIRFunctionRef::Constructor { ty, variant }), _) => {
            MIRConstant::Function(constructor::declare(
                context.env_mut(),
                *ty,
                *variant,
                span,
            )?)
        }
        (HMIRConstant::Function(HMIRFunctionRef::Def(def)), _) => {
            let prototype = lower_prototype(context.env_mut(), *def, None, span)?;
            if def.brand() == context.env().symbols().local_unit().id()
                || prototype.linkage == LinkageMode::Static
            {
                context.env_mut().generation_mut().request(*def);
            }
            MIRConstant::Function(context.env_mut().module_mut().declare_function(prototype))
        }
        (HMIRConstant::Curried { callee, args }, _) => {
            let args = args.iter().flatten().cloned().collect::<Vec<_>>();
            if let HMIRValueKind::Constant(HMIRConstant::Function(function)) = callee.kind()
                && let Some(function) = context.env().generation().generated(*function, &args)
            {
                MIRConstant::Function(function)
            } else {
                return Err(hmir_error(
                    span,
                    &mir::COMPTIME_INVALID_OPERATION,
                    "lowering an unresolved curried callable to runtime".into(),
                ));
            }
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
        MIRInstructionKind::Store {
            target: MIRTarget::Register(out),
            value,
            ty,
            bitfield: None,
        },
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
