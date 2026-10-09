use std::rc::Rc;

use cx_hmir::{
    HMIRBlockKind, HMIRExprID, HMIRExprKind, HMIRIntrinsic, HMIRLocalID, HMIROp, HMIROwnershipOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_log::catalogue::{mir, typecheck};
use cx_mir::{
    MIRBindable, MIRInternalIntrinsic, MIRTarget, MIRVAIntrinsic, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    eval::{eval_global_type, eval_static_type, ops::coerce_static},
    function::{
        Expect, Frame, FunctionLowering, LowerResult, Operand, OperandKind,
        aggregate::{lower_address_of, lower_aggregate, lower_deref_pointer},
        coerce::{lower_coerce, lower_convert, lower_nonnull_pointer},
        control::{lower_block, lower_control},
        inspect, lower_eval, lower_eval_type, lower_eval_type_hint,
        operand::{lower_auto_deref, lower_lift, lower_store, lower_value},
        ops::{lower_assign, lower_binary, lower_unary},
        promote::lower_decay,
    },
    lower::{LowerContext, LowerOutput, lower},
    module::global_ref,
    program::DefKey,
    ty::{HMIRTypeID, HMIRTypeKind},
    value::StaticValue,
};

pub(crate) fn lower_expr(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
    expect: Expect,
) -> LowerResult<Operand> {
    let LowerOutput::Runtime(value) = lower(LowerContext::Runtime(cx, frame), id, expect)? else {
        unreachable!()
    };
    Ok(value)
}

pub(crate) fn lower_static_operand(
    cx: &mut FunctionLowering<'_, '_>,
    value: StaticValue,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if let StaticValue::Global(def) = value {
        return lower_global_operand(cx, def, span);
    }
    let ty = eval_static_type(cx.program, &value, span)?;
    Ok(Operand::new(OperandKind::Static(value), ty))
}

fn lower_global_operand(
    cx: &mut FunctionLowering<'_, '_>,
    def: DefKey,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let ty = eval_global_type(cx.program, def, span)?;
    if cx.unevaluated {
        return Ok(inspect::binding(cx, ty, span)?);
    }
    let global = global_ref(cx.program, def, span)?;
    Ok(Operand::new(OperandKind::Global(global), ty))
}

pub(crate) fn lower_local(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    local: HMIRLocalID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if let Some(value) = cx.static_binding(frame, local) {
        if cx.unevaluated
            && !matches!(
                value,
                StaticValue::Type(_) | StaticValue::Function { .. } | StaticValue::Quote(_)
            )
        {
            let ty = eval_static_type(cx.program, &value, span)?;
            let operand = inspect::binding(cx, ty, span)?;
            return lower_auto_deref(cx, operand, span);
        }
        return lower_static_operand(cx, value, span);
    }
    if cx.unevaluated && cx.binding(frame, local).is_none() {
        let ty = cx.frames[frame].body().local(local).ty();
        let ty = lower_eval_type(cx, frame, ty)?;
        let operand = inspect::binding(cx, ty, span)?;
        cx.bind(frame, local, operand);
    }
    let Some(operand) = cx.binding(frame, local) else {
        let name = cx.frames[frame]
            .body()
            .local(local)
            .name()
            .map(ToString::to_string)
            .unwrap_or_else(|| local.to_string());
        return cx.error(span, &mir::RUNTIME_UNAVAILABLE, name);
    };
    lower_auto_deref(cx, operand, span)
}

pub(crate) fn lower_let(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
    span: &TokenRange,
) -> LowerResult<()> {
    let decl = cx.frames[frame].body().local(local).clone();
    if decl.is_comptime() {
        let value = match initializer {
            Some(initializer) => {
                let declared = lower_eval_type_hint(cx, frame, decl.ty())?;
                let value = lower_eval(cx, frame, initializer, Expect::of(declared))?;
                match declared {
                    Some(ty) if !matches!(cx.program.types().kind(ty), HMIRTypeKind::Type) => {
                        coerce_static(cx.program, value, ty, span)?
                    }
                    _ => value,
                }
            }
            None => StaticValue::Unit,
        };
        cx.frames[frame].statics.insert(local, value);
        return Ok(());
    }

    let declared = lower_eval_type_hint(cx, frame, decl.ty())?;
    let name = decl.name().cloned();
    if let Some(initializer) = initializer
        && let HMIRExprKind::Native(HMIROp::OwnershipOp(op @ HMIROwnershipOp::Allocate(_))) =
            cx.kind(frame, initializer)
    {
        let place = lower_place_op(cx, frame, &op, declared, name, span)?;
        cx.bind(frame, local, place);
        return Ok(());
    }

    let init = initializer
        .map(|initializer| lower_expr(cx, frame, initializer, Expect::of(declared)))
        .transpose()?;
    if let Some(init) = &init
        && let OperandKind::AdoptedPlace(place) = init.kind()
    {
        if let Some(declared) = declared
            && declared != init.ty()
        {
            return cx.error(
                span,
                &typecheck::TYPE_MISMATCH,
                (
                    "adoption".into(),
                    format!("'{}'", cx.program.types().display(declared)),
                    format!("'{}'", cx.program.types().display(init.ty())),
                ),
            );
        }
        cx.body
            .place_mut(*place)
            .expect("adopted place exists")
            .debug_name = name;
        cx.bind(frame, local, Operand::place(*place, init.ty()));
        return Ok(());
    }
    let ty = match (declared, &init) {
        (Some(ty), Some(init)) => match cx.program.types().kind(ty) {
            HMIRTypeKind::Array { length: None, .. } => {
                let init = lower_convert(cx, init.clone(), ty, span)?;
                init.ty()
            }
            _ => ty,
        },
        (Some(ty), None) => ty,
        (None, Some(init)) => lower_inferred_type(cx, init.ty()),
        (None, None) => {
            return cx.error(
                span,
                &mir::MALFORMED_HIR,
                "local without a type or initializer".into(),
            );
        }
    };
    if cx.program.types().is_void(ty) {
        let unit = Operand::unit(cx.program.types_mut());
        cx.bind(frame, local, unit);
        return Ok(());
    }

    let place = cx.place(ty, name, span)?;
    if let Some(init) = init {
        let init = lower_convert(cx, init, ty, span)?;
        let value = lower_value(cx, init, span)?;
        lower_store(cx, MIRTarget::Place(place), value, ty, None, span)?;
    }
    cx.initialize(place, span);
    cx.bind(frame, local, Operand::place(place, ty));
    Ok(())
}

// The type a local takes from its initializer when it declares none
pub(super) fn lower_inferred_type(cx: &mut FunctionLowering<'_, '_>, ty: HMIRTypeID) -> HMIRTypeID {
    let types = cx.program.types_mut();
    let ty = match types.kind(ty).clone() {
        HMIRTypeKind::Array { .. } => ty,
        HMIRTypeKind::ReferenceTo(inner) => inner,
        _ => types.decayed(ty),
    };
    types.unqualified(ty)
}

// 'allocate' and 'adopt' create places rather than values
fn lower_place_op(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: &HMIROwnershipOp,
    declared: Option<HMIRTypeID>,
    name: Option<CXIdent>,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIROwnershipOp::Allocate(ty) => {
            let ty = match declared {
                Some(ty) => ty,
                None => lower_eval_type(cx, frame, *ty)?,
            };
            let place = cx.place(ty, name, span)?;
            Ok(Operand::place(place, ty))
        }
        HMIROwnershipOp::Adopt(reference) => {
            cx.require_unsafe("@adopt", span)?;
            let reference = lower_expr(cx, frame, *reference, Expect::Any)?;
            let reference = lower_auto_deref(cx, reference, span)?;
            if matches!(
                reference.kind(),
                OperandKind::Place(_) | OperandKind::AdoptedPlace(_)
            ) {
                return cx.error(span, &typecheck::ADOPT_LOCAL, ());
            }
            if reference.bitfield().is_some() {
                return cx.error(span, &typecheck::BITFIELD_REFERENCE, "adopt".into());
            }
            let Some(address) = reference.address() else {
                return cx.error(
                    span,
                    &typecheck::TYPE_REQUIREMENT,
                    ("@adopt".into(), "referenced storage".into(), None),
                );
            };
            cx.require_mutable(reference.ty(), "adopt", span)?;
            let ty = reference.ty();
            let place = cx.place(ty, name, span)?;
            cx.body.mark_adopted(place);
            cx.intrinsic(MIRInternalIntrinsic::AdoptPlace { place, address }, span);
            cx.initialize(place, span);
            Ok(Operand::new(OperandKind::AdoptedPlace(place), ty))
        }
        _ => unreachable!("only allocate and adopt create places"),
    }
}

pub(crate) fn lower_native(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
    op: HMIROp,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIROp::BinOp { op, lhs, rhs } => lower_binary(cx, frame, op, lhs, rhs, span),
        HMIROp::UnOp { op, operand } => lower_unary(cx, frame, op, operand, span),
        HMIROp::Coerce {
            mode,
            value,
            target,
        } => lower_coerce(cx, frame, mode, value, target, span),
        HMIROp::Assign { target, op, value } => lower_assign(cx, frame, target, op, value, span),
        HMIROp::AddressOf(inner) => lower_address_of(cx, frame, inner, expect, span),
        HMIROp::Dereference(inner) => {
            let operand = lower_expr(cx, frame, inner, Expect::Any)?;
            let operand = lower_decay(cx, operand, span)?;
            let Some(inner) = cx.program.types().pointer_inner(operand.ty()) else {
                return cx.error(
                    span,
                    &typecheck::UNEXPECTED_KIND,
                    (
                        format!("'{}'", cx.program.types().display(operand.ty())),
                        "a pointer".into(),
                    ),
                );
            };
            if matches!(cx.program.types().kind(inner), HMIRTypeKind::Function(_)) {
                let ty = operand.ty();
                let pointer = lower_nonnull_pointer(cx, operand, span)?;
                return Ok(Operand::value(pointer, ty));
            }
            lower_deref_pointer(cx, operand, span)
        }
        HMIROp::Type(_) => {
            let value = lower_eval(cx, frame, id, expect)?;
            lower_static_operand(cx, value, span)
        }
        HMIROp::Control(op) => lower_control(cx, frame, op, expect, span),
        HMIROp::OwnershipOp(op) => lower_ownership(cx, frame, op, expect, span),
        HMIROp::AggregateOp(op) => lower_aggregate(cx, frame, op, expect, span),
    }
}

fn lower_ownership(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: HMIROwnershipOp,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIROwnershipOp::Move(inner) => {
            // Moving a reference binding moves the reference itself, not its referent
            if let HMIRExprKind::Local(local) = cx.kind(frame, inner)
                && let Some(binding) = cx.binding(frame, local)
                && binding.origin().is_some()
                && cx.program.types().is_reference(binding.ty())
            {
                if let Some(referent) = cx.program.types().reference_inner(binding.ty()) {
                    cx.require_mutable(referent, "move out of", span)?;
                }
                let reference = lower_lift(cx, &binding, span)?;
                return lower_auto_deref(cx, Operand::value(reference, binding.ty()), span);
            }
            let operand = lower_expr(cx, frame, inner, expect)?;
            if cx.program.types().is_unsafe_move(operand.ty()) {
                cx.require_unsafe("move of a type declared as @unsafe_move", span)?;
            }
            if !operand.is_lvalue() {
                return Ok(operand);
            }
            cx.require_mutable(operand.ty(), "move out of", span)?;
            let ty = operand.ty();
            let value = lower_lift(cx, &operand, span)?;
            Ok(Operand::value(value, ty))
        }
        HMIROwnershipOp::Leak(inner) => {
            cx.require_unsafe("@leak", span)?;
            let operand = lower_expr(cx, frame, inner, expect)?;
            if let OperandKind::Place(place) = operand.kind()
                && cx.program.types().is_nodrop(operand.ty())
            {
                cx.invalidate(MIRBindable::Place(*place), MIRInvalidationKind::Leak, span);
            }
            Ok(operand)
        }
        op => lower_place_op(cx, frame, &op, None, None, span),
    }
}

pub(crate) fn lower_splice(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    quote: HMIRExprID,
    args: &[HMIRExprID],
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let quote = if cx.unevaluated {
        let operand = lower_expr(cx, frame, quote, Expect::Any)?;
        match operand.as_static() {
            Some(StaticValue::Quote(quote)) => quote.clone(),
            _ => {
                let HMIRTypeKind::StagedExpr { params, result } =
                    cx.program.types().kind(operand.ty()).clone()
                else {
                    return cx.error(
                        span,
                        &typecheck::UNEXPECTED_KIND,
                        ("spliced value".into(), "a quote".into()),
                    );
                };
                if params.len() != args.len() {
                    return cx.error(
                        span,
                        &typecheck::ARGUMENT_COUNT,
                        ("expression".into(), params.len(), args.len(), false),
                    );
                }
                for (arg, param) in args.iter().zip(params) {
                    let arg = lower_expr(cx, frame, *arg, Expect::Type(param))?;
                    lower_convert(cx, arg, param, span)?;
                }
                return Ok(inspect::binding(cx, result, span)?);
            }
        }
    } else {
        let StaticValue::Quote(quote) = lower_eval(cx, frame, quote, Expect::Any)? else {
            return cx.error(
                span,
                &typecheck::UNEXPECTED_KIND,
                ("spliced value".into(), "a quote".into()),
            );
        };
        quote
    };
    let quote = quote.get();
    if quote.params().len() != args.len() {
        return cx.error(
            span,
            &typecheck::ARGUMENT_COUNT,
            ("quote".into(), quote.params().len(), args.len(), false),
        );
    }
    let mut operands = Vec::with_capacity(args.len());
    for arg in args {
        operands.push(lower_expr(cx, frame, *arg, Expect::Any)?);
    }

    let unit = cx.program.unit(quote.unit());
    let def = DefKey::new(quote.unit(), quote.def());
    let mut spliced = Frame::new(unit, def, Rc::new(quote.owner().clone()));
    spliced.statics = quote.env().clone();
    spliced.origin = quote
        .origin()
        .filter(|origin| origin.lowering() == cx.serial)
        .map(|origin| origin.frame());
    cx.frames.push(spliced);
    let index = cx.frames.len() - 1;
    if cx.unevaluated {
        cx.frames[index].origin = None;
        for (local, ty) in quote.runtime_types() {
            let operand = inspect::binding(cx, *ty, span)?;
            cx.bind(index, *local, operand);
        }
    }
    for (param, operand) in quote.params().iter().zip(operands) {
        let operand = match operand.kind() {
            OperandKind::Static(value) => {
                cx.frames[index].statics.insert(*param, value.clone());
                continue;
            }
            _ => operand,
        };
        cx.bind(index, *param, operand);
    }
    if quote.external_yield()
        && let HMIRExprKind::Block {
            kind: HMIRBlockKind::Yield,
            statements,
            tail,
        } = cx.frames[index].body().expr(quote.body()).kind().clone()
    {
        return lower_block(
            cx,
            index,
            HMIRBlockKind::Scope,
            &statements,
            tail,
            expect,
            span,
        );
    }
    // Code declared to produce a value is lowered for that value even where it is discarded
    let produces = quote.result().is_some_and(|result| {
        !cx.program.types().is_void(result) && !cx.program.types().is_unreachable(result)
    });
    let expect = match expect {
        _ if produces => {
            let result = quote.result().unwrap();
            Expect::Type(cx.program.types().reference_inner(result).unwrap_or(result))
        }
        expect => expect,
    };
    let value = lower_expr(cx, index, quote.body(), expect)?;
    if let Some(result) = quote.result()
        && !cx.program.types().is_void(result)
        && cx.program.types().is_void(value.ty())
    {
        return cx.error(
            span,
            &typecheck::TYPE_MISMATCH,
            (
                "staged expression".into(),
                format!("'{}'", cx.program.types().display(result)),
                "no value".into(),
            ),
        );
    }
    if produces {
        let result = quote.result().unwrap();
        if let Some(inner) = cx.program.types().reference_inner(result) {
            if value.is_lvalue() && cx.program.types_mut().same_unqualified(value.ty(), inner) {
                return Ok(value.with_type(inner));
            }
            let value = lower_convert(cx, value, result, span)?;
            return lower_auto_deref(cx, value, span);
        }
        return lower_convert(cx, value, result, span);
    }
    Ok(value)
}

pub(crate) fn lower_intrinsic_expr(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    intrinsic: HMIRIntrinsic,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Intrinsic::VA(intrinsic) = intrinsic else {
        return cx.error(
            span,
            &mir::UNSUPPORTED_LOWERING,
            "a non-variadic intrinsic".into(),
        );
    };
    match intrinsic {
        VAIntrinsic::Start { list, last } => {
            let list = lower_storage(cx, frame, list)?;
            let last = lower_storage(cx, frame, last)?;
            cx.intrinsic(MIRVAIntrinsic::VaStart { list, last }, span);
            Ok(Operand::unit(cx.program.types_mut()))
        }
        VAIntrinsic::End { list } => {
            let list = lower_storage(cx, frame, list)?;
            cx.intrinsic(MIRVAIntrinsic::VaEnd { list }, span);
            Ok(Operand::unit(cx.program.types_mut()))
        }
        VAIntrinsic::Arg { list, ty } => {
            let list = lower_storage(cx, frame, list)?;
            let ty = lower_eval_type(cx, frame, ty)?;
            let out = cx.register(ty, span)?;
            let mir = cx.mir(ty, span)?;
            cx.intrinsic(
                MIRVAIntrinsic::VaArg {
                    out: MIRTarget::Register(out),
                    list,
                    ty: mir,
                },
                span,
            );
            Ok(Operand::register(out, ty))
        }
    }
}

// The storage an lvalue names, or the value of anything else
fn lower_storage(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
) -> LowerResult<MIRValue> {
    let operand = lower_expr(cx, frame, id, Expect::Any)?;
    match operand.address() {
        Some(address) => Ok(address),
        None => lower_value(cx, operand, &cx.span(frame, id)),
    }
}
