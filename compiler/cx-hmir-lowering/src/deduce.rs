use std::rc::Rc;

use cx_hmir::{
    HMIRConstant, HMIRDefKind, HMIRExprID, HMIRExprKind, HMIRIntWidth, HMIRLocalID, HMIRNativeOp,
    HMIRTypeDesc, HMIRTypeOp,
};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, eval, eval_static_type},
    program::{DefKey, Program, def_body},
    staging_error,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

// Leading comptime type parameters; the explicit template arguments of a call fill them first
pub(crate) fn template_params(cx: &Program<'_>, def: DefKey) -> Vec<HMIRLocalID> {
    let unit = cx.unit(def.unit());
    let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
        return Vec::new();
    };
    let body = function.body();
    function
        .signature()
        .params()
        .iter()
        .take_while(|param| {
            let local = body.local(**param);
            local.is_comptime()
                && matches!(
                    body.expr(local.ty()).kind(),
                    HMIRExprKind::Constant(HMIRConstant::Type(ty))
                        if matches!(unit.types().get(*ty), HMIRTypeDesc::Type)
                )
        })
        .copied()
        .collect()
}

fn param_count(cx: &Program<'_>, def: DefKey) -> usize {
    let unit = cx.unit(def.unit());
    match unit.def(def.def()).kind() {
        HMIRDefKind::Function(function) => function.signature().params().len(),
        _ => 0,
    }
}

// Completes the template prefix of 'def' from explicit arguments ('None' for '_') and the
// types of the arguments that follow it
pub(crate) fn deduce_template(
    cx: &mut Program<'_>,
    def: DefKey,
    explicit: Vec<Option<StaticValue>>,
    actual: &[Option<TypeID>],
    span: &TokenRange,
) -> CXResult<Vec<StaticValue>> {
    let unit = cx.unit(def.unit());
    let HMIRDefKind::Function(function) = unit.def(def.def()).kind() else {
        return Err(staging_error(
            span,
            "template arguments for a non-function".into(),
        ));
    };
    let template = template_params(cx, def);
    let mut frame = EvalFrame::new(unit.clone(), def, Rc::new((def, Vec::new())));
    for (param, value) in template.iter().zip(explicit) {
        if let Some(value) = value {
            frame.bind(*param, value);
        }
    }
    let rest = &function.signature().params()[template.len()..];
    for (param, actual) in rest.iter().zip(actual) {
        if let Some(actual) = actual {
            let declared = function.body().local(*param).ty();
            unify(cx, &mut frame, declared, *actual, &template);
        }
    }
    template
        .iter()
        .map(|param| {
            frame.local(*param).cloned().ok_or_else(|| {
                let name = function
                    .body()
                    .local(*param)
                    .name()
                    .map(|name| name.as_string())
                    .unwrap_or_else(|| param.to_string());
                staging_error(
                    span,
                    format!(
                        "cannot deduce comptime argument '{name}' of '{}'",
                        unit.def(def.def()).name()
                    ),
                )
            })
        })
        .collect()
}

pub(crate) fn deduce_static(
    cx: &mut Program<'_>,
    def: DefKey,
    args: Vec<StaticValue>,
    span: &TokenRange,
) -> CXResult<Vec<StaticValue>> {
    let template = template_params(cx, def).len();
    let rest = param_count(cx, def) - template;
    if args.len() < rest {
        return Err(staging_error(span, "too few arguments".into()));
    }
    let explicit_count = (args.len() - rest).min(template);
    let actual = args[explicit_count..]
        .iter()
        .map(|arg| eval_static_type(cx, arg, span).ok())
        .collect::<Vec<_>>();
    let explicit = args[..explicit_count].iter().cloned().map(Some).collect();
    let mut values = deduce_template(cx, def, explicit, &actual, span)?;
    values.extend(args.into_iter().skip(explicit_count));
    Ok(values)
}

fn unify(
    cx: &mut Program<'_>,
    frame: &mut EvalFrame,
    expr: HMIRExprID,
    actual: TypeID,
    template: &[HMIRLocalID],
) {
    let unit = frame.unit().clone();
    let body = def_body(unit.def(frame.def().def())).expect("deduced def has a body");
    match body.expr(expr).kind() {
        HMIRExprKind::Local(local) => {
            if template.contains(local) && frame.local(*local).is_none() {
                frame.bind(*local, StaticValue::Type(actual));
            }
        }
        HMIRExprKind::Comptime(inner) => unify(cx, frame, *inner, actual, template),
        HMIRExprKind::Native(HMIRNativeOp::Type(op)) => match op {
            HMIRTypeOp::Pointer(inner) => {
                let inner_ty = cx
                    .types()
                    .pointer_inner(actual)
                    .or_else(|| cx.types().array_inner(actual));
                if let Some(actual) = inner_ty {
                    unify(cx, frame, *inner, actual, template);
                } else if matches!(cx.types().kind(actual), TypeKind::Str) {
                    let char = cx.types_mut().int(HMIRIntWidth::I8, true);
                    unify(cx, frame, *inner, char, template);
                }
            }
            HMIRTypeOp::Reference(inner) => {
                let inner_ty = cx.types().reference_inner(actual).unwrap_or(actual);
                unify(cx, frame, *inner, inner_ty, template);
            }
            HMIRTypeOp::Array { element, .. } => {
                if let Some(actual) = cx
                    .types()
                    .array_inner(actual)
                    .or_else(|| cx.types().pointer_inner(actual))
                {
                    unify(cx, frame, *element, actual, template);
                }
            }
            HMIRTypeOp::Expr { result, .. } => {
                let actual = match cx.types().kind(actual) {
                    TypeKind::Expr { result, .. } => *result,
                    _ => actual,
                };
                unify(cx, frame, *result, actual, template);
            }
            HMIRTypeOp::Function { params, ret, .. } => {
                let function = cx
                    .types()
                    .function_type(actual)
                    .or_else(|| {
                        cx.types()
                            .pointer_inner(actual)
                            .and_then(|inner| cx.types().function_type(inner))
                    })
                    .cloned();
                if let Some(function) = function {
                    for (param, actual) in params.iter().zip(function.params()) {
                        unify(cx, frame, *param, *actual, template);
                    }
                    unify(cx, frame, *ret, function.ret(), template);
                }
            }
            _ => {}
        },
        HMIRExprKind::Call { callee, args } => {
            let Some(nominal) = cx.types().nominal_of(actual) else {
                return;
            };
            let key = nominal.key().clone();
            let Ok(StaticValue::Function { def, args: bound }) = eval(cx, frame, *callee, None)
            else {
                return;
            };
            if def != key.owner() || bound.len() > key.args().len() {
                return;
            }
            for (arg, value) in args.iter().zip(&key.args()[bound.len()..]) {
                match value {
                    StaticValue::Type(ty) => unify(cx, frame, *arg, *ty, template),
                    value => {
                        if let HMIRExprKind::Local(local) = body.expr(*arg).kind()
                            && template.contains(local)
                            && frame.local(*local).is_none()
                        {
                            frame.bind(*local, value.clone());
                        }
                    }
                }
            }
        }
        _ => {}
    }
}
