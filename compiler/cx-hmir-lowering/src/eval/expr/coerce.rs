pub(crate) mod explicit;
pub(crate) mod implicit;
pub(crate) mod promotion;

use cx_hmir::{
    HMIRBinaryOp, HMIRBodyContext, HMIRCoerceMode,
    expr::HMIRExprID,
    ty::{
        HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext, intrinsic::HMIRIntWidth,
    },
};
use cx_intrinsics::{FloatIntrinsic, IntIntrinsic, InternalIntrinsic, PointerIntrinsic};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit::{self, intrinsics},
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, arithmetic, eval, eval_type, flow::Evaluation, place, types},
    log::hmir_error,
    value::{self, HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
};

pub(crate) enum Coercion {
    Applied(HMIRValue),
    Unapplied(HMIRValue),
}

impl Coercion {
    fn map(self, f: impl FnOnce(HMIRValue) -> HMIRValue) -> Self {
        match self {
            Coercion::Applied(value) => Coercion::Applied(f(value)),
            unapplied => unapplied,
        }
    }

    fn or_else(self, f: impl FnOnce(HMIRValue) -> CXResult<Self>) -> CXResult<Self> {
        match self {
            Coercion::Unapplied(value) => f(value),
            applied => Ok(applied),
        }
    }
}

pub(crate) fn eval_coerce(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    mode: HMIRCoerceMode,
    value: HMIRExprID,
    target: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    if matches!(mode, HMIRCoerceMode::Truthy) {
        let value = (eval(context, body, value))?.ready()?;
        return Ok(Evaluation::Ready(condition(context, value, span)?));
    }
    let target = (eval_type(context, body, target))?.ready()?;
    let value = eval(context, body, value)?.complete(context, body, Some(target))?;
    Ok(Evaluation::Ready(match mode {
        HMIRCoerceMode::Implicit => implicit::standard(context, value, target, span)?,
        HMIRCoerceMode::CCast => explicit::standard(context, value, target, span)?,

        // REVIEW: This is a temporary solution to allow for the use of explicit casts in the compiler. It should be removed once the compiler is fully implemented.
        _ => explicit::standard(context, value, target, span)?,
    }))
}

// Collapses a type T&...& to T&, any non-reference type remains unchanged
pub(crate) fn collapse(
    context: &mut LocalLoweringContext,
    mut value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    loop {
        let Some(inner) = context.env().reference_inner(value.ty()) else {
            return Ok(value);
        };
        if !context.env().is_reference(inner) {
            return Ok(value);
        };

        value = promotion::reference_load(context, value, inner, span)?;
    }
}

// Gives a value storage of its own, coercing T to T&
pub(crate) fn temporary(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let storage = emit::storage::scratch(context, value.ty(), span)?;
    emit::storage::initialize(context, &storage, Some(value), span)?;

    Ok(storage)
}

// Converts between scalar types, which the caller has already checked are convertible
pub(crate) fn cast(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = value.clone();
    let env = context.env();
    match (env.resolve_kind(value.ty()), env.resolve_kind(target)) {
        (HMIRTypeKind::Int(from), HMIRTypeKind::Int(_)) => {
            let sign_extend = from.signed() && from.width() != HMIRIntWidth::I1;
            let cast = IntIntrinsic::Cast { value, target, sign_extend };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::Float(_)) => {
            let cast = IntIntrinsic::ToFloat { value, target, signed: from.signed() };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Int(from), HMIRTypeKind::PointerTo(_)) => {
            let cast = IntIntrinsic::ToPtr { value, sign_extend: from.signed() };
            intrinsics::int(context, &cast, target, span)
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Int(to)) => {
            let cast = FloatIntrinsic::ToInt { value, target, signed: to.signed() };
            intrinsics::float(context, &cast, target, span)
        }
        (HMIRTypeKind::Float(_), HMIRTypeKind::Float(_)) => {
            intrinsics::float(context, &FloatIntrinsic::Cast { value, target }, target, span)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::Int(_)) => {
            let cast = PointerIntrinsic::ToInt { ptr: value, target };
            intrinsics::pointer(context, &cast, target, span)
        }
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            let cast = InternalIntrinsic::Bitcast { value, target };
            intrinsics::internal(context, &cast, target, span)
        }
        _ => unreachable!("conversions are between scalar types"),
    }
}

// Coerces towards a single reference layer -- i.e. T& -- useful for aggregate operations 
pub(crate) fn aggregate(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange
) -> CXResult<HMIRValue> {
    let coerced = match context.env().resolve_kind(value.ty()) {
        HMIRTypeKind::ReferenceTo { inner, bitfield: _ } => {
            if !context.env().is_reference(*inner) { return Ok(value); }

            promotion::reference_load(context, value, *inner, span)?
        },

        HMIRTypeKind::PointerTo(_) => {
            place::dereference(context, value, None, span)?
        },

        _ => temporary(context, &value, span)?
    };

    aggregate(context, coerced, span)
}

pub(crate) fn arithmetic(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::operand(context, value, span)?;
    let target = promotion::integer(context, value.ty(), span);
    implicit::standard(context, value, target, span)
}

pub(crate) fn default_argument(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::operand(context, value, span)?;
    let target = promotion::argument(context, value.ty(), span);
    implicit::standard(context, value, target, span)
}

pub(crate) fn condition(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::operand(context, value, span)?;
    let source = value.ty();
    match truth(context, value, span)? {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(
            span,
            &typecheck::NO_TRUTH_VALUE,
            types::name(context.env(), source),
        )),
    }
}

fn truth(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let ty = value.ty();
    let env = context.env();
    let zero = match env.resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => {
            return Ok(Coercion::Applied(value));
        }
        HMIRTypeKind::Int(_) => value::int(env, 0, ty),
        HMIRTypeKind::Float(_) => value::float(env, 0.0, ty),
        HMIRTypeKind::PointerTo(_) => HMIRValue::new(ty, HMIRImmediate::Int(0)),
        _ => return Ok(Coercion::Unapplied(value)),
    };
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let known = match value.kind() {
        HMIRValueKind::Immediate(HMIRImmediate::Int(value)) => Some(*value != 0),
        HMIRValueKind::Immediate(HMIRImmediate::Float(value)) => Some(f64::from(value) != 0.0),
        _ => None,
    };
    Ok(Coercion::Applied(match known {
        Some(known) => value::int(context.env(), i128::from(known), boolean),
        None => arithmetic::binary(context, HMIRBinaryOp::Neq, &value, &zero, boolean, span)?,
    }))
}

pub(crate) fn pointer(
    context: &mut LocalLoweringContext,
    left: HMIRTypeID,
    right: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let (pointee, other) = match env.is_void(right) {
        true => (right, left),
        false => (left, right),
    };
    let other = env.semantics(other);
    let qualifiers = HMIRTypeSemantics {
        constant: other.constant,
        volatile: other.volatile,
        ..Default::default()
    };
    let pointee = types::qualify::specify(context, pointee, qualifiers, span);
    types::pointer(context.env_mut(), pointee, span)
}

fn compatible(env: &HMIREnvironment, lhs: HMIRTypeID, rhs: HMIRTypeID) -> bool {
    if env.same_type(lhs, rhs) {
        return true;
    }
    if env.semantics(lhs).move_semantics != env.semantics(rhs).move_semantics {
        return false;
    }
    match (env.resolve_kind(lhs), env.resolve_kind(rhs)) {
        (HMIRTypeKind::Staged(lhs), HMIRTypeKind::Staged(rhs)) => compatible(env, *lhs, *rhs),
        (HMIRTypeKind::PointerTo(lhs), HMIRTypeKind::PointerTo(rhs)) => {
            env.is_void(*lhs) || env.is_void(*rhs) || compatible(env, *lhs, *rhs)
        }
        (
            HMIRTypeKind::ReferenceTo {
                inner: lhs,
                bitfield: lhs_bitfield,
            },
            HMIRTypeKind::ReferenceTo {
                inner: rhs,
                bitfield: rhs_bitfield,
            },
        ) => lhs_bitfield == rhs_bitfield && compatible(env, *lhs, *rhs),
        (
            HMIRTypeKind::Array {
                element: lhs,
                length: lhs_length,
            },
            HMIRTypeKind::Array {
                element: rhs,
                length: rhs_length,
            },
        ) => lhs_length == rhs_length && compatible(env, *lhs, *rhs),
        (
            HMIRTypeKind::Undefined(HMIRUndefined::Array { element: lhs }),
            HMIRTypeKind::Undefined(HMIRUndefined::Array { element: rhs }),
        ) => compatible(env, *lhs, *rhs),
        _ => false,
    }
}

fn qualified(env: &HMIREnvironment, from: HMIRTypeID, to: HMIRTypeID) -> bool {
    let pointee = |ty| env.pointer_inner(ty).or_else(|| env.reference_inner(ty));
    match (pointee(from), pointee(to)) {
        (Some(from), Some(to)) => {
            let (from, to) = (env.semantics(from), env.semantics(to));
            (!from.constant || to.constant) && (!from.volatile || to.volatile)
        }
        _ => true,
    }
}

fn is_designator(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(env.resolve_kind(ty), HMIRTypeKind::Str)
}

fn is_char(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(env.resolve_kind(ty), HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I8)
}

fn applied(
    context: &LocalLoweringContext,
    coercion: Coercion,
    source: HMIRTypeID,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    match coercion {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => {
            let names = (
                types::name(context.env(), source),
                types::name(context.env(), target),
            );
            Err(hmir_error(span, &typecheck::INVALID_CONVERSION, names))
        }
    }
}
