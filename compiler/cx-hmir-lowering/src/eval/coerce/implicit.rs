use cx_hmir::{
    expr::constant::HMIRConstant,
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
    value::{HMIRValue, HMIRValueKind},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        coerce::{
            compatible,
            pending::Coercion,
            promotion::{decay, reference},
            truthy::truthy,
        },
        constant,
        place::dereference,
        types,
    },
    log::hmir_error,
};

// The conversions C performs without a cast (C11 6.3, 6.5.16.1), one step at a time; the caller applies steps
// until the value has the target type
pub(crate) fn rule(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    try_unreachable(env, value)
        .or_else(|| try_void(env, target))
        .or_else(|| try_truthy(env, value, target))
        .or_else(|| reference::try_load(env, value))
        .or_else(|| try_bind_reference(env, target))
        .or_else(|| try_string_pointer(env, value, target))
        .or_else(|| try_string_array(env, value, target))
        .or_else(|| decay::try_decay(env, value))
        .or_else(|| try_numeric(env, value, target))
        .or_else(|| try_null_pointer(env, value, target))
        .or_else(|| try_pointer(env, value, target))
}

// The value of an expression that never completes is never used
fn try_unreachable(env: &HMIREnvironment, value: &HMIRValue) -> Coercion {
    if !matches!(env.resolve_kind(value.ty()), HMIRTypeKind::Unreachable) {
        return Coercion::unapplied();
    }

    Coercion::apply(|_, value, _| Ok(value))
}

fn try_void(env: &HMIREnvironment, target: HMIRTypeID) -> Coercion {
    if !matches!(env.resolve_kind(target), HMIRTypeKind::Void) {
        return Coercion::unapplied();
    }

    Coercion::apply(|context, _, span| Ok(constant::unit(context, span)))
}

fn try_truthy(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        (
            HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_),
            HMIRTypeKind::Int(to),
        ) if to.width() == HMIRIntWidth::I1 => Coercion::apply(move |context, value, span| {
            Ok(truthy(context, value, span)?.with_ty(target))
        }),
        _ => Coercion::unapplied(),
    }
}

fn try_bind_reference(env: &HMIREnvironment, target: HMIRTypeID) -> Coercion {
    let Some(inner) = env.reference_inner(target) else {
        return Coercion::unapplied();
    };

    Coercion::apply(move |context, value, span| bind_reference(context, value, inner, target, span))
}

// A reference binds to storage of its type, or to what a pointer to that type points to
fn bind_reference(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    inner: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointee = context.env().pointer_inner(value.ty());
    let value = match pointee {
        Some(pointee) if context.env().same_type(pointee, inner) => {
            dereference(context, value, span)?
        }
        _ => value,
    };

    if !context.env().same_type(value.ty(), inner) {
        return Err(hmir_error(
            span,
            &typecheck::INVALID_CONVERSION,
            (
                types::name(context.env(), value.ty()),
                types::name(context.env(), inner),
            ),
        ));
    }
    // A string constant already stands for the address of its characters
    if matches!(context.env().resolve_kind(inner), HMIRTypeKind::Str)
        && matches!(value.kind(), HMIRValueKind::Constant(_))
    {
        return Ok(value.with_ty(target));
    }

    let value = emit::spill(context, value, span)?;
    Ok(match value.kind() {
        HMIRValueKind::Place(place) => HMIRValue::constant(
            target,
            HMIRConstant::Address {
                place: *place,
                path: Vec::new(),
                ty: target,
            },
        ),
        HMIRValueKind::Indirect(reference) => reference.as_ref().clone().with_ty(target),
        HMIRValueKind::Constant(_) => unreachable!("a spilled value is addressable"),
    })
}

// A string converts to any pointer, whatever the signedness of the characters it is taken to point to
fn try_string_pointer(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        (HMIRTypeKind::Str, HMIRTypeKind::PointerTo(pointee)) => {
            let pointee = *pointee;
            Coercion::apply(move |context, value, span| decay::decay(context, value, pointee, span))
        }
        _ => Coercion::unapplied(),
    }
}

// C11 6.7.9p14: a string literal initializes an array of characters, which is as long as the string and its
// terminator when the array has no length of its own
fn try_string_array(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    let HMIRValueKind::Constant(HMIRConstant::Str(string)) = value.kind() else {
        return Coercion::unapplied();
    };
    let HMIRTypeKind::Array { element, length } = env.resolve_kind(target) else {
        return Coercion::unapplied();
    };
    if !matches!(env.resolve_kind(*element), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)
    {
        return Coercion::unapplied();
    }

    let (element, length, bytes) = (*element, *length, string.clone().into_bytes());
    Coercion::apply(move |context, _, span| {
        // The terminator is left out where the array only has room for the characters
        let length = length.unwrap_or(bytes.len() as u64 + 1);
        if bytes.len() as u64 > length {
            let limit = ("string".into(), Some(length as usize));
            return Err(hmir_error(span, &typecheck::INITIALIZER_LIMIT, limit));
        }

        let ty = types::array(context.env_mut(), target, length, span);
        let fields = (0..length as usize)
            .map(|index| {
                let value = bytes.get(index).copied().unwrap_or(0) as i128;
                (index, Box::new(HMIRConstant::Int { value, ty: element }))
            })
            .collect();
        Ok(HMIRValue::constant(
            ty,
            HMIRConstant::Aggregate { ty, fields },
        ))
    })
}

fn try_numeric(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        // Signedness only changes how the same bits are read
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) if from.width() == to.width() => {
            retype(target)
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::Float(_), HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_))
        | (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => cast(target),
        _ => Coercion::unapplied(),
    }
}

// C11 6.3.2.3: an integer constant 0 is the null pointer of any pointer type
fn try_null_pointer(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    let zero = matches!(
        value.kind(),
        HMIRValueKind::Constant(HMIRConstant::Int { value: 0, .. })
    );
    if !zero || !matches!(env.resolve_kind(target), HMIRTypeKind::PointerTo(_)) {
        return Coercion::unapplied();
    }

    Coercion::apply(move |_, _, _| Ok(HMIRValue::constant(target, HMIRConstant::Null(target))))
}

fn try_pointer(env: &HMIREnvironment, value: &HMIRValue, target: HMIRTypeID) -> Coercion {
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        (HMIRTypeKind::PointerTo(from), HMIRTypeKind::PointerTo(to))
            if compatible::pointees(env, *from, *to) =>
        {
            retype(target)
        }
        _ => Coercion::unapplied(),
    }
}

// The value is unchanged, but read as the target type
pub(crate) fn retype(target: HMIRTypeID) -> Coercion {
    Coercion::apply(move |context, value, span| {
        Ok(emit::read(context, value, span)?.with_ty(target))
    })
}

pub(crate) fn cast(target: HMIRTypeID) -> Coercion {
    Coercion::apply(move |context, value, span| emit::cast(context, &value, target, span))
}
