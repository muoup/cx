pub mod flow;
pub mod intrinsics;
pub mod storage;

use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext};
use cx_log::CXResult;
use cx_mir::{
    MIRConstant, MIRInstructionKind, MIRPlaceDecl, MIRRegisterDecl, MIRTarget, MIRValue,
    constant::MIRAddressable, expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::{LocalLoweringContext, runtime::RuntimeScope, scope::Bindings},
    eval::{aggregate::variant, call, symbol},
    lower::{
        declaration, global,
        types::{lower_prototype, lower_type, mir_float, mir_int},
    },
    value::{
        HMIRPlaceID, HMIRRegisterID, HMIRValue, HMIRValueKind,
        address::{Addressable, HMIRAddress},
        immediate::HMIRImmediate,
    },
};

pub fn open_scope(context: &mut LocalLoweringContext) -> usize {
    let scopes = context.runtime_mut().scopes_mut();
    let parent = scopes.top();

    scopes.push(RuntimeScope::new(false), Some(parent), Bindings::default())
}

pub fn close_scope(context: &mut LocalLoweringContext, span: &TokenRange) {
    let index = context.runtime().scopes().top();
    drop_scope(context, index, span);
    context.runtime_mut().scopes_mut().pop();
}

pub fn drop_scope(context: &mut LocalLoweringContext, index: usize, span: &TokenRange) {
    let places = context.runtime().scopes().control(index).places().to_vec();
    for place in places.into_iter().rev() {
        let place = context.runtime().place(place);
        let invalidate = MIRInstructionKind::Invalidate {
            place,
            kind: MIRInvalidationKind::Drop,
        };
        context.runtime_mut().emit(invalidate, span.clone());
    }
}

// The operand a value is at its own type: a reference or pointer is the address it holds, and a reference to a
// place used as a plain value is the contents of that place
pub fn value(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<MIRValue> {
    Ok(match value.kind() {
        HMIRValueKind::Immediate(constant) => {
            MIRValue::Constant(self::constant(context, constant, value.ty(), span)?)
        }
        HMIRValueKind::Register(register) => MIRValue::Register(context.runtime().register(*register)),
        HMIRValueKind::Reference(_)
            if context.env().is_reference(value.ty()) || context.env().is_pointer(value.ty()) =>
        {
            MIRValue::Constant(local(context, value))
        }
        HMIRValueKind::Reference(_) => storage::contents(context, value, span)?,
    })
}

pub(super) fn local(context: &LocalLoweringContext, value: &HMIRValue) -> MIRConstant {
    let HMIRValueKind::Reference(reference) = value.kind() else {
        unreachable!("a local address names a place")
    };
    MIRConstant::Address {
        base: MIRAddressable::Local(context.runtime().place(*reference.base())),
        offset: reference.offset(),
    }
}

pub fn constant(
    context: &mut LocalLoweringContext,
    constant: &HMIRImmediate,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    let kind = context.env_mut().resolve_kind(ty).clone();
    Ok(match (constant, kind) {
        (HMIRImmediate::Unit, _) => MIRConstant::Unit,
        (HMIRImmediate::Int(0), HMIRTypeKind::PointerTo(_)) => MIRConstant::Nullptr {
            ty: lower_type(context.env_mut(), ty, span)?,
        },
        (HMIRImmediate::Int(value), HMIRTypeKind::Int(int)) => MIRConstant::Integer {
            ty: mir_int(int.width()),
            value: *value,
        },
        (HMIRImmediate::Float(value), HMIRTypeKind::Float(float)) => MIRConstant::Float {
            value: *value,
            ty: mir_float(float.width()),
        },
        (HMIRImmediate::Address(address), kind) => self::address(context, address, kind, span)?,
        (HMIRImmediate::Aggregate { fields: items }, kind) => {
            let mut fields = Vec::with_capacity(items.len());
            for (index, item) in items.iter().enumerate() {
                let field = match &kind {
                    HMIRTypeKind::Array { element, .. } => *element,
                    HMIRTypeKind::Struct { fields } => *fields[index].ty(),
                    _ => unreachable!("an aggregate constant is an array or structure"),
                };
                fields.push((index, self::constant(context, item, field, span)?));
            }
            MIRConstant::Aggregate {
                ty: lower_type(context.env_mut(), ty, span)?,
                fields,
            }
        }
        (HMIRImmediate::TaggedUnion { variant, payload }, HMIRTypeKind::TaggedUnion { fields }) => {
            let payload = self::constant(context, payload, *fields[*variant].ty(), span)?;
            MIRConstant::Aggregate {
                ty: lower_type(context.env_mut(), ty, span)?,
                fields: vec![(*variant, payload)],
            }
        }
        (HMIRImmediate::Constructor { ty, variant }, _) => {
            MIRConstant::Function(variant::function(context.env_mut(), *ty, *variant, span)?)
        }
        (HMIRImmediate::Curried { callee, args }, _) => {
            MIRConstant::Function(call::dependent::template(context, callee, args, ty, span)?)
        }
        (constant, _) => unreachable!("{constant:?} has no runtime form"),
    })
}

// A function designator is the function itself; any other address is a constant pointer into what it names
fn address(
    context: &mut LocalLoweringContext,
    address: &HMIRAddress<Addressable>,
    kind: HMIRTypeKind,
    span: &TokenRange,
) -> CXResult<MIRConstant> {
    let offset = address.offset();
    let base = match address.base() {
        Addressable::Def(def) => {
            if declaration::is_function(context.env().symbols().resolve_definition(*def)) {
                let prototype = lower_prototype(context.env_mut(), *def, span)?;
                let function = context.env_mut().module_mut().declare_function(prototype);
                if matches!(kind, HMIRTypeKind::Function(_)) {
                    return Ok(MIRConstant::Function(function));
                }
                MIRAddressable::Function(function)
            } else {
                let object = symbol::value_type(context.env_mut(), *def)?;
                MIRAddressable::Global(global::declare(context.env_mut(), *def, object, span)?)
            }
        }
        Addressable::Label { function, label } => {
            return Ok(MIRConstant::BlockAddress {
                function: *function,
                label: label.to_string(),
            });
        }
        Addressable::Literal(value) => {
            return Ok(MIRConstant::String {
                value: value.clone(),
                offset,
            });
        }
    };
    Ok(MIRConstant::Address { base, offset })
}

pub(crate) fn register(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRRegisterID> {
    let ty = lower_type(context.env_mut(), ty, span)?;
    let register = context
        .runtime_mut()
        .builder_mut()
        .allocate_register(MIRRegisterDecl::new(ty, None));

    Ok(HMIRRegisterID::new(register.0))
}

// A fresh register for the result of an instruction, which holds it once the instruction is emitted
pub(crate) fn output(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<(MIRTarget, HMIRValue)> {
    let register = register(context, ty, span)?;
    let target = MIRTarget::Register(context.runtime().register(register));

    Ok((target, HMIRValue::new(ty, register)))
}

pub(crate) fn place(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: Option<CXIdent>,
    nodrop: bool,
    adopted: bool,
    span: &TokenRange,
) -> CXResult<HMIRPlaceID> {
    let ty = lower_type(context.env_mut(), ty, span)?;
    let place = context
        .runtime_mut()
        .builder_mut()
        .allocate_place(MIRPlaceDecl::new(ty, name, nodrop, adopted));

    Ok(HMIRPlaceID::new(place.0))
}
