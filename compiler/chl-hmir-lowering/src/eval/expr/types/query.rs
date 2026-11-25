use chl_hmir::{
    HMIRBodyContext,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use chl_log::{CHLResult, catalogue::typecheck};
use chl_tokens::TokenRange;

use crate::{
    env::{function::LocalLoweringContext, resolver::demand},
    eval::{
        EvalResult, deduce, eval, eval_type,
        flow::Evaluation,
        types::{self, alike, layout, qualify},
    },
    log::hmir_error,
    value::{self, HMIRValue, immediate::HMIRImmValue},
};

pub(crate) fn eval_type_op(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    op: &HMIRTypeOp,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = match op {
        HMIRTypeOp::SizeOf(_) | HMIRTypeOp::AlignOf(_) | HMIRTypeOp::OffsetOf { .. } => {
            return layout::eval_type_op(context, body, span, op);
        }
        HMIRTypeOp::Member { .. } | HMIRTypeOp::Aggregate { .. } => {
            todo!("HMIRTypeOp: {}", op.path())
        }
        HMIRTypeOp::TypeOf(operand) => {
            let ty = deduce::typed(context, span, true, |child| eval(child, body, *operand))?;
            type_value(context, ty.ready()?, span)
        }
        HMIRTypeOp::Equal(lhs, rhs) => {
            let lhs = eval_type(context, body, *lhs)?.ready()?;
            let rhs = eval_type(context, body, *rhs)?.ready()?;
            let equal = alike(context.env(), lhs, rhs, true);
            boolean(context, equal, span)
        }
        HMIRTypeOp::PointerInner(ty) => {
            let ty = subject(context, body, *ty)?;
            let inner = context.env().pointer_inner(ty);
            type_value(context, inner_or_error(context, ty, inner, "a pointer type", span)?, span)
        }
        HMIRTypeOp::ReferenceInner(ty) => {
            let ty = subject(context, body, *ty)?;
            let inner = context.env().reference_inner(ty);
            type_value(context, inner_or_error(context, ty, inner, "a reference type", span)?, span)
        }
        HMIRTypeOp::Decay(ty) => {
            let ty = subject(context, body, *ty)?;
            let decayed = decay(context, ty, span);
            type_value(context, decayed, span)
        }
        HMIRTypeOp::Unqualify(ty) => {
            let ty = subject(context, body, *ty)?;
            let unqualified = qualify::unqualified(context, ty, span);
            type_value(context, unqualified, span)
        }
        HMIRTypeOp::IsNodrop(ty)
        | HMIRTypeOp::IsNocopy(ty)
        | HMIRTypeOp::IsUnsafeMove(ty) => {
            let ty = subject(context, body, *ty)?;
            demand::force(context.env_mut(), ty)?;

            let holds = match op {
                HMIRTypeOp::IsNocopy(_) => context.env().is_nocopy(ty),
                HMIRTypeOp::IsNodrop(_) => context.env().is_nodrop(ty),
                HMIRTypeOp::IsUnsafeMove(_) => context.env().semantics(ty).unsafe_move,

                _ => unreachable!()
            };
            boolean(context, holds, span)
        },

        HMIRTypeOp::IsInt(ty)
        | HMIRTypeOp::IsFloat(ty)
        | HMIRTypeOp::IsPointer(ty)
        | HMIRTypeOp::IsSigned(ty)
        | HMIRTypeOp::IsReference(ty)
        | HMIRTypeOp::IsConst(ty) => {
            let ty = subject(context, body, *ty)?;
            if matches!(
                op,
                HMIRTypeOp::IsNodrop(_) | HMIRTypeOp::IsNocopy(_) | HMIRTypeOp::IsUnsafeMove(_)
            ) {
                demand::force(context.env_mut(), ty)?;
            }
            let env = context.env();
            let holds = match op {
                HMIRTypeOp::IsInt(_) => env.is_int(ty),
                HMIRTypeOp::IsFloat(_) => env.is_float(ty),
                HMIRTypeOp::IsPointer(_) => env.is_pointer(ty),
                HMIRTypeOp::IsReference(_) => env.is_reference(ty),
                HMIRTypeOp::IsSigned(_) => env.is_int(ty) && types::int_type(env, ty).signed(),
                HMIRTypeOp::IsConst(_) => env.semantics(ty).constant,

                _ => unreachable!()
            };
            boolean(context, holds, span)
        }
    };
    Ok(Evaluation::Ready(value))
}

fn subject(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    ty: HMIRExprID,
) -> EvalResult<HMIRTypeID> {
    eval_type(context, body, ty)?.ready()
}

fn inner_or_error(
    context: &LocalLoweringContext,
    ty: HMIRTypeID,
    inner: Option<HMIRTypeID>,
    expected: &str,
    span: &TokenRange,
) -> CHLResult<HMIRTypeID> {
    inner.ok_or_else(|| {
        let subject = format!("type '{}'", types::name(context.env(), ty));
        hmir_error(span, &typecheck::UNEXPECTED_KIND, (subject, expected.into()))
    })
}

// The type a value of 'ty' is passed as: no reference or qualifiers, with arrays and functions as pointers
fn decay(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    let ty = qualify::value(context, ty, span);
    let env = context.env();
    let pointee = match env.resolve_kind(ty) {
        HMIRTypeKind::Function { .. } => Some(ty),
        _ => env.array_inner(ty),
    };
    match pointee {
        Some(pointee) => types::pointer(context.env_mut(), pointee, span),
        None => ty,
    }
}

fn type_value(context: &mut LocalLoweringContext, ty: HMIRTypeID, span: &TokenRange) -> HMIRValue {
    let universe = types::universe(context.env_mut(), span);
    HMIRValue::new(universe, HMIRImmValue::Type(ty))
}

fn boolean(context: &mut LocalLoweringContext, holds: bool, span: &TokenRange) -> HMIRValue {
    let ty = types::int(context.env_mut(), HMIRIntWidth::I1, false, span);
    value::int(context.env(), holds as i128, ty)
}
