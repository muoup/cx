use std::rc::Rc;

use cx_hmir::{
    HMIRExprID, HMIRExprKind, HMIRIntrinsic, HMIRLocalID, HMIRNativeOp, HMIROwnershipOp,
};
use cx_intrinsics::{Intrinsic, VAIntrinsic};
use cx_mir::{
    MIRBindable, MIRInstructionKind, MIRInternalIntrinsic, MIRTarget, MIRVAIntrinsic, MIRValue,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;

use crate::{
    eval::{eval_global_type, eval_static_type, ops::coerce_static},
    function::{
        Expect, Frame, FunctionLowering, LowerResult, Operand, OperandKind,
        aggregate::{lower_address_of, lower_aggregate, lower_deref_pointer},
        coerce::lower_coerce,
        control::lower_control,
        inspect, lower_eval, lower_eval_type, lower_eval_type_hint,
        operand::{
            lower_auto_deref, lower_convert, lower_decay, lower_lift, lower_store, lower_value,
        },
        ops::{lower_assign, lower_binary, lower_unary},
    },
    lower::{LowerContext, LowerOutput, lower},
    module::global_ref,
    program::DefKey,
    ty::{TypeID, TypeKind},
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
        return cx.error(span, format!("'{name}' is not bound at runtime"));
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
                    Some(ty) if !matches!(cx.program.types().kind(ty), TypeKind::Type) => {
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
        && let HMIRExprKind::Native(HMIRNativeOp::OwnershipOp(
            op @ (HMIROwnershipOp::Adopt(_) | HMIROwnershipOp::Allocate(_)),
        )) = cx.kind(frame, initializer)
    {
        let place = lower_place_op(cx, frame, &op, declared, name, span)?;
        cx.bind(frame, local, place);
        return Ok(());
    }

    let init = initializer
        .map(|initializer| lower_expr(cx, frame, initializer, Expect::of(declared)))
        .transpose()?;
    let ty = match (declared, &init) {
        (Some(ty), Some(init)) => match cx.program.types().kind(ty) {
            TypeKind::Array { length: None, .. } => {
                let init = lower_convert(cx, init.clone(), ty, span)?;
                init.ty()
            }
            _ => ty,
        },
        (Some(ty), None) => ty,
        (None, Some(init)) => lower_inferred_type(cx, init.ty()),
        (None, None) => return cx.error(span, "local without a type or initializer"),
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
    cx.emit(
        MIRInstructionKind::Initialize {
            place: MIRBindable::Place(place),
        },
        span,
    );
    cx.bind(frame, local, Operand::place(place, ty));
    Ok(())
}

// The type a local takes from its initializer when it declares none
pub(super) fn lower_inferred_type(cx: &mut FunctionLowering<'_, '_>, ty: TypeID) -> TypeID {
    let types = cx.program.types_mut();
    match types.kind(ty).clone() {
        TypeKind::Str => types.char_pointer(),
        TypeKind::Function(_) => types.pointer_to(ty),
        TypeKind::Reference(inner) => inner,
        _ => ty,
    }
}

// 'allocate' and 'adopt' create places rather than values
fn lower_place_op(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    op: &HMIROwnershipOp,
    declared: Option<TypeID>,
    name: Option<cx_util::identifier::CXIdent>,
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
        HMIROwnershipOp::Adopt(pointer) => {
            let pointer = lower_expr(cx, frame, *pointer, Expect::Any)?;
            let pointer = lower_decay(cx, pointer, span)?;
            let ty = match declared.or_else(|| cx.program.types().pointer_inner(pointer.ty())) {
                Some(ty) => ty,
                None => return cx.error(span, "adopted a non-pointer"),
            };
            let pointer_ty = cx.program.types_mut().pointer_to(ty);
            let pointer = lower_convert(cx, pointer, pointer_ty, span)?;
            let address = lower_value(cx, pointer, span)?;
            let place = cx.place(ty, name, span)?;
            cx.body.mark_adopted(place);
            cx.intrinsic(MIRInternalIntrinsic::AdoptPlace { place, address }, span);
            cx.emit(
                MIRInstructionKind::Initialize {
                    place: MIRBindable::Place(place),
                },
                span,
            );
            Ok(Operand::place(place, ty))
        }
        _ => unreachable!("only allocate and adopt create places"),
    }
}

pub(crate) fn lower_native(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    id: HMIRExprID,
    op: HMIRNativeOp,
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match op {
        HMIRNativeOp::BinOp { op, lhs, rhs } => lower_binary(cx, frame, op, lhs, rhs, span),
        HMIRNativeOp::UnOp { op, operand } => lower_unary(cx, frame, op, operand, span),
        HMIRNativeOp::Coerce {
            mode,
            value,
            target,
        } => lower_coerce(cx, frame, mode, value, target, span),
        HMIRNativeOp::Assign { target, op, value } => {
            lower_assign(cx, frame, target, op, value, span)
        }
        HMIRNativeOp::AddressOf(inner) => lower_address_of(cx, frame, inner, expect, span),
        HMIRNativeOp::Dereference(inner) => {
            let operand = lower_expr(cx, frame, inner, Expect::Any)?;
            let operand = lower_decay(cx, operand, span)?;
            let Some(inner) = cx.program.types().pointer_inner(operand.ty()) else {
                return cx.error(span, "dereferenced a non-pointer");
            };
            if matches!(cx.program.types().kind(inner), TypeKind::Function(_)) {
                return Ok(operand);
            }
            lower_deref_pointer(cx, operand, span)
        }
        HMIRNativeOp::Type(_) => {
            let value = lower_eval(cx, frame, id, expect)?;
            lower_static_operand(cx, value, span)
        }
        HMIRNativeOp::Control(op) => lower_control(cx, frame, op, expect, span),
        HMIRNativeOp::OwnershipOp(op) => lower_ownership(cx, frame, op, expect, span),
        HMIRNativeOp::AggregateOp(op) => lower_aggregate(cx, frame, op, expect, span),
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
            let operand = lower_expr(cx, frame, inner, expect)?;
            if !operand.is_lvalue() {
                return Ok(operand);
            }
            let ty = operand.ty();
            let value = lower_lift(cx, &operand, span)?;
            Ok(Operand::value(value, ty))
        }
        HMIROwnershipOp::Leak(inner) => {
            let operand = lower_expr(cx, frame, inner, expect)?;
            if let OperandKind::Place(place) = operand.kind()
                && cx.program.types().is_nodrop(operand.ty())
            {
                cx.emit(
                    MIRInstructionKind::Invalidate {
                        place: MIRBindable::Place(*place),
                        kind: MIRInvalidationKind::Leak,
                    },
                    span,
                );
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
                let TypeKind::Expr { params, result } =
                    cx.program.types().kind(operand.ty()).clone()
                else {
                    return cx.error(span, "spliced a value that is not a quote");
                };
                if params.len() != args.len() {
                    return cx.error(
                        span,
                        "expression argument count does not match its parameters",
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
            return cx.error(span, "spliced a value that is not a quote");
        };
        quote
    };
    let quote = quote.get();
    if quote.params().len() != args.len() {
        return cx.error(
            span,
            format!(
                "quote expects {} arguments, found {}",
                quote.params().len(),
                args.len()
            ),
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
    lower_expr(cx, index, quote.body(), expect)
}

pub(crate) fn lower_intrinsic_expr(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    intrinsic: HMIRIntrinsic,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let Intrinsic::VA(intrinsic) = intrinsic else {
        return cx.error(span, "only variadic intrinsics are lowered from HMIR");
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
