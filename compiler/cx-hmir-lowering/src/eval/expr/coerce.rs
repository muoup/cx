mod implicit;
mod promotion;

use cx_hmir::{
    HMIRBinaryOp, HMIRBody, HMIRCoerceMode,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::unsafe_float::FloatWrapper;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, eval, eval_type, expr,
        flow::Evaluation,
        types,
    },
    log::hmir_error,
    value::{
        HMIRValue, HMIRValueKind,
        constant::HMIRConstant,
        scalar::{float, int},
    },
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
    body: &HMIRBody,
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
        HMIRCoerceMode::Implicit => implicit(context, value, target, span)?,
        _ => explicit(context, value, target, span)?,
    }))
}

pub(crate) fn value_type(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    let ty = context.env().referent(ty);
    let qualifiers = HMIRTypeSemantics { constant: true, volatile: true, ..Default::default() };
    types::unspecify(context, ty, qualifiers, span)
}

pub(crate) fn implicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    let coercion = implicit::convert(context, value, target, span, false)?;
    applied(context, coercion, source, target, span)
}

pub(crate) fn explicit(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let source = value.ty();
    let coercion = match context.env().is_void(target) {
        true => Coercion::Applied(expr::unit(context, span).with_ty(target)),
        false => implicit::convert(context, value, target, span, true)?.or_else(|value| extension(context, value, target, span))?,
    };
    applied(context, coercion, source, target, span)
}

fn extension(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    target: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<Coercion> {
    let subject = context.env().referent(value.ty());
    let array = matches!(context.env().resolve_kind(subject), HMIRTypeKind::Array { .. });
    let value = match context.env().resolve_kind(subject) {
        HMIRTypeKind::PointerTo(_) | HMIRTypeKind::Array { .. } => std_rvalue(context, value, span)?,
        _ => value,
    };
    let env = context.env();
    let source = value.ty();
    if env.semantics(source).move_semantics != env.semantics(target).move_semantics {
        return Ok(Coercion::Unapplied(value));
    }
    Ok(match (env.resolve_kind(source), env.resolve_kind(target)) {
        (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => {
            expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::PointerTo(char), HMIRTypeKind::ReferenceTo { inner: string, .. })
            if env.is_str(*string) && is_char(env, *char) =>
        {
            let operation = if array { "Unsafe type conversion" } else { "Dereferencing a pointer" };
            expr::require_nonsafe(context, span, operation)?;
            Coercion::Applied(value.with_ty(target))
        }
        (HMIRTypeKind::Int(_), HMIRTypeKind::PointerTo(_)) => {
            if !matches!(value.kind(), HMIRValueKind::Constant(HMIRConstant::Int(0))) {
                expr::require_nonsafe(context, span, "Unsafe type conversion")?;
            }
            Coercion::Applied(emit::cast(context, &value, target, span)?)
        }
        _ => Coercion::Unapplied(value),
    })
}

pub(crate) fn std_rvalue(
    context: &mut LocalLoweringContext,
    mut value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    loop {
        match promotion::step(context, value, span)? {
            Coercion::Applied(promoted) => value = promoted,
            Coercion::Unapplied(value) => return Ok(value),
        }
    }
}

pub(crate) fn arithmetic(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> CXResult<HMIRValue> {
    let value = std_rvalue(context, value, span)?;
    let target = promotion::integer(context, value.ty(), span);
    implicit(context, value, target, span)
}

pub(crate) fn default_argument(
    context: &mut LocalLoweringContext,
    value: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let value = std_rvalue(context, value, span)?;
    let target = promotion::argument(context, value.ty(), span);
    implicit(context, value, target, span)
}

pub(crate) fn condition(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> CXResult<HMIRValue> {
    let value = std_rvalue(context, value, span)?;
    let source = value.ty();
    match truth(context, value, span)? {
        Coercion::Applied(value) => Ok(value),
        Coercion::Unapplied(_) => Err(hmir_error(span, &typecheck::NO_TRUTH_VALUE, types::name(context.env(), source)).into()),
    }
}

fn truth(context: &mut LocalLoweringContext, value: HMIRValue, span: &TokenRange) -> CXResult<Coercion> {
    let ty = value.ty();
    let zero = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Int(int) if int.width() == HMIRIntWidth::I1 => return Ok(Coercion::Applied(value)),
        HMIRTypeKind::Int(_) => HMIRConstant::Int(0),
        HMIRTypeKind::Float(_) => HMIRConstant::Float(FloatWrapper::from(0.0)),
        HMIRTypeKind::PointerTo(_) => HMIRConstant::Null,
        _ => return Ok(Coercion::Unapplied(value)),
    };
    let boolean = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    let known = match (value.kind(), context.env().resolve_kind(ty)) {
        (HMIRValueKind::Constant(HMIRConstant::Int(value)), HMIRTypeKind::Int(ty)) => {
            Some(int::normalize(*value, *ty) != 0)
        }
        (HMIRValueKind::Constant(HMIRConstant::Float(value)), HMIRTypeKind::Float(ty)) => {
            Some(float::round(f64::from(value), ty.width()) != 0.0)
        }
        (HMIRValueKind::Constant(HMIRConstant::Null), HMIRTypeKind::PointerTo(_)) => Some(false),
        _ => None,
    };
    Ok(Coercion::Applied(match known {
        Some(known) => HMIRValue::constant(boolean, HMIRConstant::Int(i128::from(known))),
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
    let pointee = types::specify(context, pointee, qualifiers, span);
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
            HMIRTypeKind::ReferenceTo { inner: lhs, bitfield: lhs_bitfield },
            HMIRTypeKind::ReferenceTo { inner: rhs, bitfield: rhs_bitfield },
        ) => lhs_bitfield == rhs_bitfield && compatible(env, *lhs, *rhs),
        (
            HMIRTypeKind::Array { element: lhs, length: lhs_length },
            HMIRTypeKind::Array { element: rhs, length: rhs_length },
        ) => lhs_length == rhs_length && compatible(env, *lhs, *rhs),
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
            let names = (types::name(context.env(), source), types::name(context.env(), target));
            Err(hmir_error(span, &typecheck::INVALID_CONVERSION, names).into())
        }
    }
}
