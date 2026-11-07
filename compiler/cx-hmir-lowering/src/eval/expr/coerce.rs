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
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    emit::comptime::{float, int},
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, eval, eval_type, flow::Evaluation, types},
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
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

pub(crate) fn value_type(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    let ty = context.env().referent(ty);
    types::qualify::unqualified(context, ty, span)
}

pub(crate) fn read(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let inner = context
        .env()
        .reference_inner(value.ty())
        .expect("a read value is a reference");

    promotion::reference_load(context, value, inner, span)
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

        value = read(context, value, span)?;
    }
}

pub(crate) fn arithmetic(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = promotion::integer(context, value.ty(), span);
    implicit::standard(context, value, target, span)
}

pub(crate) fn default_argument(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
    let target = promotion::argument(context, value.ty(), span);
    implicit::standard(context, value, target, span)
}

pub(crate) fn condition(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = promotion::std_rvalue(context, value, span)?;
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
    let zero = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => {
            return Ok(Coercion::Applied(value));
        }
        HMIRTypeKind::Int(_) => HMIRImmediate::Int(0),
        HMIRTypeKind::Float(_) => HMIRImmediate::Float(FloatWrapper::from(0.0)),
        HMIRTypeKind::PointerTo(_) => HMIRImmediate::Null,
        _ => return Ok(Coercion::Unapplied(value)),
    };
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let known = match (value.kind(), context.env().resolve_kind(ty)) {
        (HMIRValueKind::Constant(HMIRImmediate::Int(value)), HMIRTypeKind::Int(ty)) => {
            Some(int::normalize(*value, *ty) != 0)
        }
        (HMIRValueKind::Constant(HMIRImmediate::Float(value)), HMIRTypeKind::Float(ty)) => {
            Some(float::round(f64::from(value), ty.width()) != 0.0)
        }
        (HMIRValueKind::Constant(HMIRImmediate::Null), HMIRTypeKind::PointerTo(_)) => Some(false),
        _ => None,
    };
    Ok(Coercion::Applied(match known {
        Some(known) => HMIRValue::constant(boolean, HMIRImmediate::Int(i128::from(known))),
        None => {
            let zero = HMIRValue::constant(ty, zero);
            emit::binary(context, HMIRBinaryOp::Neq, &value, &zero, boolean, span)?
        }
    }))
}

pub(crate) fn common(
    context: &mut LocalLoweringContext,
    lhs: HMIRTypeID,
    rhs: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    promotion::common(context, lhs, rhs, span)
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
    match env.resolve_kind(ty) {
        HMIRTypeKind::Str => true,
        HMIRTypeKind::Function(_) => !types::comptime_only(env, ty),
        _ => false,
    }
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
