use std::rc::Rc;

use cx_hmir::{HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRFloatWidth, HMIRIntWidth};
use cx_mir::{MIRConstant, MIRInstructionKind, MIRValue};
use cx_tokens::TokenRange;

use crate::{
    deduce::{deduce_template, template_params},
    eval::{EvalFrame, def_value, eval_signature, eval_static_type, eval_type},
    function::{
        Expect, FunctionLowering, LowerResult, Operand, Stop,
        coerce::lower_convert,
        contract::{lower_call_postcondition, lower_call_precondition},
        expr::{lower_expr, lower_static_operand},
        inspect, lower_eval, lower_eval_frame,
        operand::lower_value,
        promote::lower_decay,
    },
    module::declare_function,
    program::DefKey,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

enum CallArg {
    Static(StaticValue),
    Expr(HMIRExprID),
}

pub(crate) fn lower_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> LowerResult<Operand> {
    let callee = match lower_static_callee(cx, frame, callee)? {
        Some(StaticValue::Function { def, args: bound }) => {
            return lower_static_call(cx, frame, def, bound, args, span);
        }
        Some(value) => lower_static_operand(cx, value, span)?,
        None => lower_expr(cx, frame, callee, Expect::Any)?,
    };
    match callee.as_static() {
        Some(StaticValue::Function { def, args: bound }) => {
            let (def, bound) = (*def, bound.clone());
            lower_static_call(cx, frame, def, bound, args, span)
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

fn lower_static_call(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    def: DefKey,
    bound: Vec<StaticValue>,
    args: &[HMIRExprID],
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
    let given = bound.len() + args.len();
    if given < params.len() {
        return lower_curry(cx, frame, def, bound, args, span);
    }
    if !variadic && given > params.len() {
        return cx.error(
            span,
            format!(
                "'{}' expects {} arguments, found {given}",
                unit.def(def.def()).name(),
                params.len()
            ),
        );
    }

    let mut bound = bound.into_iter();
    let mut args = args.iter().copied();
    let mut explicit = Vec::with_capacity(template.len());
    for _ in 0..template.len() {
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

    let mut rest = bound.map(CallArg::Static).collect::<Vec<_>>();
    for arg in args {
        let param = params.get(template.len() + rest.len()).copied();
        let comptime = param.is_some_and(|param| body.local(param).is_comptime());
        rest.push(if comptime {
            CallArg::Static(lower_eval(cx, frame, arg, Expect::Any)?)
        } else {
            CallArg::Expr(arg)
        });
    }

    let mut instance_args = if template.is_empty() {
        Vec::new()
    } else {
        let mut actual = Vec::new();
        if explicit.iter().any(Option::is_none) {
            let mut hints = EvalFrame::new(unit.clone(), def, Rc::new((def, Vec::new())));
            for (param, value) in template.iter().zip(&explicit) {
                if let Some(value) = value {
                    hints.bind(*param, value.clone());
                }
            }
            let source = lower_eval_frame(cx, frame);
            for (param, arg) in params[template.len()..].iter().zip(&rest) {
                actual.push(match arg {
                    CallArg::Static(value) => eval_static_type(cx.program, value, span).ok(),
                    CallArg::Expr(arg) => {
                        let hint = eval_type(cx.program, &mut hints, body.local(*param).ty()).ok();
                        Some(inspect::inspect(cx.program, &source, *arg, hint)?)
                    }
                });
            }
        }
        deduce_template(cx.program, def, explicit, &actual, span)?
    };
    for (param, arg) in params[template.len()..].iter().zip(&rest) {
        if !body.local(*param).is_comptime() {
            continue;
        }
        let value = match arg {
            CallArg::Static(value) => Some(value),
            CallArg::Expr(_) => None,
        };
        let Some(value) = value else {
            return cx.error(span, "comptime argument is not known at compile time");
        };
        instance_args.push(value.clone());
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
        let param = params.get(template.len() + index);
        if param.is_some_and(|param| body.local(*param).is_comptime()) {
            continue;
        }
        let ty = param
            .and_then(|param| signature.runtime().iter().position(|local| local == param))
            .map(|position| signature.params()[position].1);
        let operand = match arg {
            CallArg::Static(value) => lower_static_operand(cx, value, span)?,
            CallArg::Expr(arg) => lower_expr(cx, frame, arg, Expect::of(ty))?,
        };
        if param.is_some() && ty.is_none() {
            continue;
        }
        values.push(lower_argument(cx, operand, ty, span)?);
    }
    let contract = lower_call_precondition(cx, &instance, &signature, &values)?;
    let result = lower_emit_call(cx, callee, values, signature.ret(), span)?;
    lower_call_postcondition(cx, contract, &result)?;
    Ok(result)
}

// Fewer arguments than parameters bind the leading comptime parameters and yield the function
fn lower_curry(
    cx: &mut FunctionLowering<'_, '_>,
    frame: usize,
    def: DefKey,
    mut bound: Vec<StaticValue>,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> LowerResult<Operand> {
    let unit = cx.program.unit(def.unit());
    let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
        unreachable!("curried def is a function");
    };
    let params = function.signature().params();
    let given = bound.len() + args.len();
    if params[..given]
        .iter()
        .any(|param| !function.body().local(*param).is_comptime())
    {
        return cx.error(
            span,
            format!(
                "'{}' expects {} arguments, found {given}",
                unit.def(def.def()).name(),
                params.len()
            ),
        );
    }
    for arg in args {
        bound.push(lower_eval(cx, frame, *arg, Expect::Any)?);
    }
    lower_static_operand(cx, StaticValue::Function { def, args: bound }, span)
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
        values.push(lower_argument(cx, operand, param, span)?);
    }
    lower_emit_call(cx, callee, values, function.ret(), span)
}

fn lower_argument(
    cx: &mut FunctionLowering<'_, '_>,
    operand: Operand,
    param: Option<TypeID>,
    span: &TokenRange,
) -> LowerResult<MIRValue> {
    if let Some(param) = param {
        let operand = lower_convert(cx, operand, param, span)?;
        return lower_value(cx, operand, span);
    }
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
