use cx_hmir::{
    HMIRBodyContext,
    expr::{HMIRExprID, HMIRExprIntrinsic},
    ty::{HMIRTypeID, HMIRTypeKind, context::HMIRTypeContext, intrinsic::HMIRIntWidth},
};
use cx_intrinsics::{
    FloatBinaryOp, FloatIntrinsic, IntBinaryOp, IntIntrinsic, InternalIntrinsic, Intrinsic,
    IntrinsicMapper, PointerIntrinsic, VAIntrinsic,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, coerce, eval, eval_type,
        expr::name,
        flow::{EvalExit, Evaluation},
        types::{self, alike},
    },
    log::hmir_error,
    value::{HMIRValIntrinsic, HMIRValue},
};

pub(crate) fn eval_intrinsic(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    intrinsic: &HMIRExprIntrinsic,
) -> EvalResult<Evaluation<HMIRValue>> {
    if let Intrinsic::VA(intrinsic) = intrinsic {
        return va(context, body, span, intrinsic);
    }
    let path = intrinsic.path();
    let intrinsic = intrinsic.try_map(&mut Operands { context, body, path })?;
    let result = result(context, &intrinsic, path, span)?;
    Ok(Evaluation::Ready(emit::intrinsics::intrinsic(
        context, &intrinsic, result, span,
    )?))
}

// Operands are taken exactly as they evaluate: a place is not read, and nothing is converted
struct Operands<'c, 'p, 'g, 'h, 'b> {
    context: &'c mut LocalLoweringContext<'p, 'g, 'h>,
    body: &'b HMIRBodyContext,
    path: &'static str,
}

impl IntrinsicMapper<HMIRExprID, HMIRExprID> for Operands<'_, '_, '_, '_, '_> {
    type Value = HMIRValue;
    type Type = HMIRTypeID;
    type Error = EvalExit;

    fn value(&mut self, value: &HMIRExprID) -> EvalResult<HMIRValue> {
        let span = self.body.expr(*value).span();
        let value = eval(self.context, self.body, *value)?.ready()?;
        if self.context.env().is_reference(value.ty()) {
            let subject = operand(self.context.env(), value.ty(), self.path);
            let hint = "copy its value out with '@copy'";
            let error = hmir_error(span, &typecheck::UNEXPECTED_KIND, (subject, "a value".into()));
            return Err(error.with_note(hint, None).into());
        }
        Ok(value)
    }

    fn ty(&mut self, ty: &HMIRExprID) -> EvalResult<HMIRTypeID> {
        eval_type(self.context, self.body, *ty)?.ready()
    }
}

fn operand(env: &HMIREnvironment, ty: HMIRTypeID, path: &str) -> String {
    format!("'{}' operand of '@{path}'", types::name(env, ty))
}

fn require(
    context: &LocalLoweringContext,
    ty: HMIRTypeID,
    holds: fn(&HMIREnvironment, HMIRTypeID) -> bool,
    expected: &str,
    path: &str,
    span: &TokenRange,
) -> CXResult<()> {
    match holds(context.env(), ty) {
        true => Ok(()),
        false => {
            let subject = operand(context.env(), ty, path);
            Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, (subject, expected.into())))
        }
    }
}

fn same(
    context: &LocalLoweringContext,
    lhs: &HMIRValue,
    rhs: &HMIRValue,
    path: &str,
    span: &TokenRange,
) -> CXResult<()> {
    let env = context.env();
    match alike(env, lhs.ty(), rhs.ty(), false) {
        true => Ok(()),
        false => {
            let operands = (format!("@{path}"), types::name(env, lhs.ty()), types::name(env, rhs.ty()));
            Err(hmir_error(span, &typecheck::INVALID_BINARY_OPERANDS, operands))
        }
    }
}

fn is_int(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.is_int(ty)
}

fn is_float(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.is_float(ty)
}

fn is_pointer(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.is_pointer(ty)
}

fn is_pointer_sized(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.is_int(ty) && types::int_type(env, ty).width() == HMIRIntWidth::pointer_width(env.architecture())
}

fn is_bool(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    env.is_int(ty) && types::int_type(env, ty).width() == HMIRIntWidth::I1
}

fn bool(context: &mut LocalLoweringContext, span: &TokenRange) -> HMIRTypeID {
    types::int(context.env_mut(), HMIRIntWidth::I1, false, span)
}

fn result(
    context: &mut LocalLoweringContext,
    intrinsic: &HMIRValIntrinsic,
    path: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    match intrinsic {
        Intrinsic::Int(intrinsic) => int(context, intrinsic, path, span),
        Intrinsic::Float(intrinsic) => float(context, intrinsic, path, span),
        Intrinsic::Pointer(intrinsic) => pointer(context, intrinsic, path, span),
        Intrinsic::Internal(intrinsic) => internal(context, intrinsic, path, span),
        Intrinsic::Aggregate(_) | Intrinsic::VA(_) => {
            unreachable!("aggregate and variadic intrinsics are not named by a path")
        }
    }
}

fn int(
    context: &mut LocalLoweringContext,
    intrinsic: &IntIntrinsic<HMIRValue, HMIRTypeID>,
    path: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let integer = "an integer";
    match intrinsic {
        IntIntrinsic::Unary { value, .. } => {
            require(context, value.ty(), is_int, integer, path, span)?;
            Ok(value.ty())
        }
        IntIntrinsic::Binary { op, lhs, rhs } => {
            require(context, lhs.ty(), is_int, integer, path, span)?;
            same(context, lhs, rhs, path, span)?;
            let predicate = matches!(
                op,
                IntBinaryOp::Eq
                    | IntBinaryOp::Neq
                    | IntBinaryOp::ULt
                    | IntBinaryOp::SLt
                    | IntBinaryOp::ULe
                    | IntBinaryOp::SLe
                    | IntBinaryOp::UGt
                    | IntBinaryOp::SGt
                    | IntBinaryOp::UGe
                    | IntBinaryOp::SGe
                    | IntBinaryOp::LAnd
                    | IntBinaryOp::LOr
            );
            Ok(match predicate {
                true => bool(context, span),
                false => lhs.ty(),
            })
        }
        IntIntrinsic::ToFloat { value, target, .. } => {
            require(context, value.ty(), is_int, integer, path, span)?;
            require(context, *target, is_float, "a floating type", path, span)?;
            Ok(*target)
        }
        IntIntrinsic::Cast { value, target, .. } => {
            require(context, value.ty(), is_int, integer, path, span)?;
            require(context, *target, is_int, "an integer type", path, span)?;
            Ok(*target)
        }
        IntIntrinsic::ToPtr { value, .. } => {
            require(context, value.ty(), is_int, integer, path, span)?;
            let void = types::unit(context.env_mut());
            Ok(types::pointer(context.env_mut(), void, span))
        }
    }
}

fn float(
    context: &mut LocalLoweringContext,
    intrinsic: &FloatIntrinsic<HMIRValue, HMIRTypeID>,
    path: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let floating = "a floating value";
    match intrinsic {
        FloatIntrinsic::Neg { value } => {
            require(context, value.ty(), is_float, floating, path, span)?;
            Ok(value.ty())
        }
        FloatIntrinsic::Binary { op, lhs, rhs } => {
            require(context, lhs.ty(), is_float, floating, path, span)?;
            same(context, lhs, rhs, path, span)?;
            let predicate = matches!(
                op,
                FloatBinaryOp::Eq
                    | FloatBinaryOp::Neq
                    | FloatBinaryOp::Lt
                    | FloatBinaryOp::Le
                    | FloatBinaryOp::Gt
                    | FloatBinaryOp::Geq
            );
            Ok(match predicate {
                true => bool(context, span),
                false => lhs.ty(),
            })
        }
        FloatIntrinsic::ToInt { value, target, .. } => {
            require(context, value.ty(), is_float, floating, path, span)?;
            require(context, *target, is_int, "an integer type", path, span)?;
            Ok(*target)
        }
        FloatIntrinsic::Cast { value, target } => {
            require(context, value.ty(), is_float, floating, path, span)?;
            require(context, *target, is_float, "a floating type", path, span)?;
            Ok(*target)
        }
    }
}

fn pointer(
    context: &mut LocalLoweringContext,
    intrinsic: &PointerIntrinsic<HMIRValue, HMIRTypeID>,
    path: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let pointer = "a pointer";
    match intrinsic {
        PointerIntrinsic::ToInt { ptr, target } => {
            require(context, ptr.ty(), is_pointer, pointer, path, span)?;
            require(context, *target, is_int, "an integer type", path, span)?;
            Ok(*target)
        }
        PointerIntrinsic::Offset { ptr, offset, .. } => {
            require(context, ptr.ty(), is_pointer, pointer, path, span)?;
            let expected = "an integer the size of a pointer";
            require(context, offset.ty(), is_pointer_sized, expected, path, span)?;
            Ok(ptr.ty())
        }
        PointerIntrinsic::Diff { lhs, rhs, .. } => {
            require(context, lhs.ty(), is_pointer, pointer, path, span)?;
            same(context, lhs, rhs, path, span)?;
            Ok(types::int(context.env_mut(), HMIRIntWidth::I64, true, span))
        }
        PointerIntrinsic::Compare { lhs, rhs, .. } => {
            require(context, lhs.ty(), is_pointer, pointer, path, span)?;
            same(context, lhs, rhs, path, span)?;
            Ok(bool(context, span))
        }
    }
}

fn internal(
    context: &mut LocalLoweringContext,
    intrinsic: &InternalIntrinsic<HMIRValue, HMIRTypeID>,
    path: &str,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    match intrinsic {
        InternalIntrinsic::Bitcast { value, target } => {
            let env = context.env();
            let compatible = match (env.resolve_kind(value.ty()), env.resolve_kind(*target)) {
                (HMIRTypeKind::PointerTo(_), HMIRTypeKind::PointerTo(_)) => true,
                (HMIRTypeKind::Int(from), HMIRTypeKind::Int(to)) => from.width() == to.width(),
                _ => false,
            };
            if !compatible {
                let subject = operand(env, value.ty(), path);
                let expected = format!("a pointer or an integer the size of '{}'", types::name(env, *target));
                return Err(hmir_error(span, &typecheck::UNEXPECTED_KIND, (subject, expected)));
            }
            Ok(*target)
        }
        InternalIntrinsic::Assert { condition, .. } | InternalIntrinsic::Assume { condition } => {
            require(context, condition.ty(), is_bool, "a bool", path, span)?;
            Ok(types::unit(context.env_mut()))
        }
    }
}

fn va(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    intrinsic: &VAIntrinsic<HMIRExprID, HMIRExprID>,
) -> EvalResult<Evaluation<HMIRValue>> {
    let intrinsic = match intrinsic {
        VAIntrinsic::Start { list, last } => {
            let list = (va_list(context, body, *list))?.ready()?;
            let last = (eval(context, body, *last))?.ready()?;
            let last = coerce::promotion::std_rvalue(context, last, span)?;
            VAIntrinsic::Start { list, last }
        }
        VAIntrinsic::End { list } => VAIntrinsic::End {
            list: (va_list(context, body, *list))?.ready()?,
        },
        VAIntrinsic::Arg { list, ty } => VAIntrinsic::Arg {
            list: (va_list(context, body, *list))?.ready()?,
            ty: (eval_type(context, body, *ty))?.ready()?,
        },
    };
    let result = match &intrinsic {
        VAIntrinsic::Arg { ty, .. } => *ty,
        VAIntrinsic::Start { .. } | VAIntrinsic::End { .. } => types::unit(context.env_mut()),
    };
    Ok(Evaluation::Ready(emit::intrinsics::va(
        context, &intrinsic, result, span,
    )?))
}

fn va_list(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    list: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let span = body.expr(list).span();
    let value = (eval(context, body, list))?.ready()?;
    let expected = name::intrinsic_type(context, "__builtin_va_list", span)
        .expect("va_list is an intrinsic type");
    let actual = context.env().reference_inner(value.ty());
    if !actual.is_some_and(|actual| alike(context.env(), actual, expected, false)) {
        let mismatch = (
            "va_list expression".into(),
            types::name(context.env(), expected),
            types::name(context.env(), value.ty()),
        );
        return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
    }
    let ty = types::pointer(context.env_mut(), expected, span);
    Ok(Evaluation::Ready(value.with_ty(ty)))
}
