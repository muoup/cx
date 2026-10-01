use std::rc::Rc;

use cx_hmir::{
    HMIRAggregateOp, HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth,
    HMIRNativeOp,
};
use cx_mir::{MIRConstant, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    deduce::{deduce_template, template_params},
    eval::{EvalFrame, def_value, eval_signature, eval_static_type, eval_type},
    function::{
        Expect, FunctionLowering, LowerResult, Operand, Stop,
        aggregate::{lower_address_of_operand, lower_deref_pointer, lower_member},
        expr::{lower_expr, lower_static_operand},
        inspect, lower_eval,
        operand::{lower_convert, lower_decay, lower_value},
    },
    module::declare_function,
    program::DefKey,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

enum CallArg {
    Static(StaticValue),
    Runtime(Operand),
}

pub(crate) fn lower_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    if let HMIRExprKind::Native(HMIRNativeOp::AggregateOp(HMIRAggregateOp::Member { base, name })) =
        cx.kind(frame, callee)
    {
        let receiver = lower_expr(cx, frame, base, Expect::Any)?;
        return lower_method_call(cx, frame, receiver, &name, args, expect, span);
    }
    let callee = match lower_static_callee(cx, frame, callee)? {
        Some(StaticValue::Function { def, args: bound }) => {
            return lower_static_call(cx, frame, def, bound, None, args, expect, span);
        }
        Some(value) => lower_static_operand(cx, value, span)?,
        None => lower_expr(cx, frame, callee, Expect::Any)?,
    };
    match callee.as_static() {
        Some(StaticValue::Function { def, args: bound }) => {
            let (def, bound) = (*def, bound.clone());
            lower_static_call(cx, frame, def, bound, None, args, expect, span)
        }
        _ => lower_indirect_call(cx, frame, callee, args, span),
    }
}

fn lower_static_callee(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    callee: HMIRExprID,
) -> LowerResult<Option<StaticValue>> {
    let span = cx.span(frame, callee);
    Ok(match cx.kind(frame, callee) {
        HMIRExprKind::Def(def) => {
            let unit = cx.frames[frame].def.unit();
            let key = cx.program.resolve(unit, &def, &span)?;
            Some(def_value(cx.program, key, &span)?)
        }
        HMIRExprKind::Local(local) => cx.static_binding(frame, local),
        HMIRExprKind::Comptime(_) => Some(lower_eval(cx, frame, callee, Expect::Any)?),
        _ => None,
    })
}

// A member that is not a field names an associated function taking the receiver first
fn lower_method_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    receiver: Operand,
    name: &CXIdent,
    args: &[HMIRExprID],
    expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let object = match cx.program.types().kind(receiver.ty()) {
        TypeKind::Pointer(inner) => *inner,
        _ => receiver.ty(),
    };
    if cx.program.types().field(object, name.as_str()).is_some() {
        let member = lower_member(cx, receiver, name, span)?;
        return lower_indirect_call(cx, frame, member, args, span);
    }
    let Some(owner) = cx
        .program
        .types()
        .nominal_of(object)
        .map(|nominal| nominal.key().owner())
    else {
        return cx.error(
            span,
            format!(
                "'{}' has no method '{name}'",
                cx.program.types().display(object)
            ),
        );
    };
    let Some(def) = cx.program.associated(owner, name) else {
        return cx.error(
            span,
            format!(
                "'{}' has no method '{name}'",
                cx.program.types().display(object)
            ),
        );
    };
    let receiver = lower_deref_pointer(cx, receiver, span)?;
    lower_static_call(
        cx,
        frame,
        def,
        Vec::new(),
        Some(receiver),
        args,
        expect,
        span,
    )
}

#[allow(clippy::too_many_arguments)]
fn lower_static_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    def: DefKey,
    bound: Vec<StaticValue>,
    receiver: Option<Operand>,
    args: &[HMIRExprID],
    _expect: Expect,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let unit = cx.program.unit(def.unit());
    let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
        return cx.error(
            span,
            format!("'{}' is not a function", unit.def(def.def()).name()),
        );
    };
    let params = function.signature().params().to_vec();
    let variadic = function.signature().is_variadic();
    let body = function.body();
    let template = template_params(cx.program, def);
    let rest_len = params.len() - template.len();
    let given = bound.len() + receiver.is_some() as usize + args.len();
    let explicit_len = given.saturating_sub(rest_len).min(template.len());
    if given < rest_len || (!variadic && given > params.len()) {
        return cx.error(
            span,
            format!(
                "'{}' expects {} arguments, found {given}",
                unit.def(def.def()).name(),
                rest_len
            ),
        );
    }

    let mut bound = bound.into_iter();
    let mut args = args.iter().copied();
    let mut explicit = Vec::with_capacity(template.len());
    for _ in 0..explicit_len {
        if let Some(value) = bound.next() {
            explicit.push(Some(value));
            continue;
        }
        let arg = args.next().expect("explicit arguments were counted");
        if matches!(cx.kind(frame, arg), HMIRExprKind::Hole(_)) {
            explicit.push(None);
        } else {
            explicit.push(Some(lower_eval(cx, frame, arg, Expect::Any)?));
        }
    }

    let mut hints = EvalFrame::new(unit.clone(), def, Rc::new((def, Vec::new())));
    for (param, value) in template.iter().zip(&explicit) {
        if let Some(value) = value {
            hints.bind(*param, value.clone());
        }
    }
    let mut rest = bound.map(CallArg::Static).collect::<Vec<_>>();
    let receiver_index = receiver.as_ref().map(|_| rest.len());
    rest.extend(receiver.map(CallArg::Runtime));
    for arg in args {
        let param = params.get(template.len() + rest.len()).copied();
        let hint = param.and_then(|param| {
            let ty = body.local(param).ty();
            eval_type(cx.program, &mut hints, ty).ok()
        });
        let comptime = param.is_some_and(|param| body.local(param).is_comptime());
        rest.push(if comptime {
            CallArg::Static(lower_eval(cx, frame, arg, Expect::Any)?)
        } else {
            CallArg::Runtime(lower_expr(cx, frame, arg, Expect::of(hint))?)
        });
    }

    let mut actual = Vec::with_capacity(rest_len);
    for arg in rest.iter().take(rest_len) {
        actual.push(match arg {
            CallArg::Static(value) => eval_static_type(cx.program, value, span).ok(),
            CallArg::Runtime(operand) => Some(operand.ty()),
        });
    }
    let mut instance_args = if template.is_empty() {
        Vec::new()
    } else {
        deduce_template(cx.program, def, explicit, &actual, span)?
    };
    for (param, arg) in params[template.len()..].iter().zip(&rest) {
        if !body.local(*param).is_comptime() {
            continue;
        }
        match arg {
            CallArg::Static(value) => instance_args.push(value.clone()),
            CallArg::Runtime(operand) => match operand.as_static() {
                Some(value) => instance_args.push(value.clone()),
                None => {
                    return cx.error(span, "comptime argument is not known at compile time");
                }
            },
        }
    }

    let instance = (def, instance_args);
    let callee = if cx.unevaluated {
        MIRValue::Constant(MIRConstant::Unit)
    } else {
        let id = declare_function(cx.program, &instance, span)?;
        cx.program.module_mut().use_function(id);
        MIRValue::Constant(MIRConstant::Function(id))
    };
    let signature = eval_signature(cx.program, &instance, span)?;
    let mut values = Vec::with_capacity(rest.len());
    for (index, arg) in rest.into_iter().enumerate() {
        let operand = match arg {
            CallArg::Static(value) => lower_static_operand(cx, value, span),
            CallArg::Runtime(operand) => Ok(operand),
        };
        match params.get(template.len() + index) {
            Some(param) if body.local(*param).is_comptime() => continue,
            Some(param) => {
                let Some(position) = signature.runtime().iter().position(|local| local == param)
                else {
                    continue;
                };
                let ty = signature.params()[position].1;
                let mut operand = operand?;
                if receiver_index == Some(index) {
                    operand = lower_receiver(cx, operand, ty, span)?;
                }
                let operand = lower_convert(cx, operand, ty, span)?;
                values.push(lower_value(cx, operand, span)?);
            }
            None => {
                let value = lower_variadic_value(cx, operand?, span)?;
                values.push(value);
            }
        }
    }
    lower_emit_call(cx, callee, values, signature.ret(), span)
}

// A receiver lvalue is passed by address to a method taking a pointer
fn lower_receiver(
    cx: &mut FunctionLowering<'_, '_>,
    receiver: Operand,
    param: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    match cx.program.types().kind(param) {
        TypeKind::Pointer(inner) if *inner == receiver.ty() => {
            lower_address_of_operand(cx, receiver, span)
        }
        _ => Ok(receiver),
    }
}

fn lower_indirect_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    callee: Operand,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> LowerResult<Operand> {
    let function = match cx.program.types().kind(callee.ty()) {
        TypeKind::Pointer(inner) => cx.program.types().kind(*inner).clone(),
        kind => kind.clone(),
    };
    let TypeKind::Function(function) = function else {
        return cx.error(
            span,
            format!(
                "'{}' is not callable",
                cx.program.types().display(callee.ty())
            ),
        );
    };
    let params = function.params().len();
    if args.len() < params || (!function.is_variadic() && args.len() > params) {
        return cx.error(
            span,
            format!("expected {params} arguments, found {}", args.len()),
        );
    }
    let callee = lower_decay(cx, callee, span)?;
    let callee = lower_value(cx, callee, span)?;
    let mut values = Vec::with_capacity(args.len());
    for (index, arg) in args.iter().enumerate() {
        let param = function.params().get(index).copied();
        let operand = lower_expr(cx, frame, *arg, Expect::of(param))?;
        values.push(match param {
            Some(param) => {
                let operand = lower_convert(cx, operand, param, span)?;
                lower_value(cx, operand, span)?
            }
            None => lower_variadic_value(cx, operand, span)?,
        });
    }
    lower_emit_call(cx, callee, values, function.ret(), span)
}

// C's default argument promotions
fn lower_variadic_value(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    let operand = lower_decay(cx, operand, span)?;
    let promoted = match cx.program.types().kind(operand.ty()).clone() {
        TypeKind::Int { width, signed } if width < HMIRIntWidth::I32 => Some(
            cx.program
                .types_mut()
                .int(HMIRIntWidth::I32, signed || width == HMIRIntWidth::I1),
        ),
        TypeKind::Float {
            width: HMIRFloatWidth::F32,
        } => Some(cx.program.types_mut().intern(TypeKind::Float {
            width: HMIRFloatWidth::F64,
        })),
        _ => None,
    };
    let operand = match promoted {
        Some(ty) => lower_convert(cx, operand, ty, span)?,
        None => operand,
    };
    lower_value(cx, operand, span)
}

fn lower_emit_call(
    cx: &mut FunctionLowering<'_, '_>,
    callee: MIRValue,
    args: Vec<MIRValue>,
    ret: TypeID,
    span: &TokenRange,
) -> LowerResult<Operand> {
    let types = cx.program.types();
    let unreachable = types.is_unreachable(ret);
    if cx.unevaluated && matches!(types.kind(ret), TypeKind::Expr { .. } | TypeKind::Type) {
        return Ok(inspect::binding(cx, ret, span)?);
    }
    let out = if types.is_void(ret) || unreachable {
        None
    } else {
        Some(cx.register(ret, span)?)
    };
    cx.emit(MIRInstructionKind::Call { out, callee, args }, span);
    if unreachable {
        cx.emit(MIRInstructionKind::Unreachable, span);
        return Err(Stop::Diverged);
    }
    Ok(match out {
        Some(register) => Operand::register(register, ret),
        None => Operand::unit(cx.program.types_mut()),
    })
}
