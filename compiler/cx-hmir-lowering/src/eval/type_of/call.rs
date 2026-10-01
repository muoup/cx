use cx_hmir::{HMIRDefKind, HMIRExprID, HMIRExprKind};
use cx_log::CXResult;
use cx_tokens::TokenRange;

use crate::{
    eval::{EvalFrame, type_of::type_of},
    program::{Program, def_body},
    staging_error,
    ty::{TypeID, TypeKind},
    value::StaticValue,
};

pub(super) fn result(
    program: &mut Program<'_>,
    frame: &mut EvalFrame,
    callee: HMIRExprID,
    args: &[HMIRExprID],
    span: &TokenRange,
) -> CXResult<TypeID> {
    let unit = frame.unit().clone();
    let body = def_body(unit.def(frame.def().def())).expect("call has a body");
    let function = match body.expr(callee).kind() {
        HMIRExprKind::Def(def) => {
            let key = program.resolve(frame.def().unit(), def, span)?;
            match program.unit(key.unit()).def(key.def()).kind() {
                HMIRDefKind::Function(_) => Some(StaticValue::Function {
                    def: key,
                    args: Vec::new(),
                }),
                _ => None,
            }
        }
        HMIRExprKind::Local(local) => frame.local(*local).cloned(),
        HMIRExprKind::Comptime(_) => Some(program.eval(frame, callee)?),
        _ => None,
    };
    let Some(StaticValue::Function { def, args: bound }) = function else {
        let ty = type_of(program, frame, callee)?;
        let ty = program.types().pointee(ty).unwrap_or(ty);
        return match program.types().kind(ty) {
            TypeKind::Function(function) => Ok(function.ret()),
            _ => Err(staging_error(
                span,
                format!("'{}' is not callable", program.types().display(ty)),
            )),
        };
    };
    let target = program.unit(def.unit());
    let HMIRDefKind::Function(function) = target.def(def.def()).kind() else {
        return Err(staging_error(span, "expected a function".into()));
    };
    let params = function.signature().params();
    let template = program.template_params(def).len();
    let rest = params.len() - template;
    let given = bound.len() + args.len();
    if given < rest || (!function.signature().is_variadic() && given > params.len()) {
        return Err(staging_error(
            span,
            format!(
                "'{}' expects {rest} arguments, found {given}",
                target.def(def.def()).name()
            ),
        ));
    }
    if template == 0
        && params
            .iter()
            .all(|param| !function.body().local(*param).is_comptime())
    {
        return Ok(program.signature(&(def, Vec::new()), span)?.ret());
    }
    let explicit_count = given.saturating_sub(rest).min(template);
    let mut bound = bound.into_iter();
    let mut args = args.iter().copied();
    let mut explicit = Vec::new();
    for _ in 0..explicit_count {
        explicit.push(match bound.next() {
            Some(value) => Some(value),
            None => {
                let arg = args.next().expect("explicit arguments were counted");
                match body.expr(arg).kind() {
                    HMIRExprKind::Hole(_) => None,
                    _ => Some(program.eval(frame, arg)?),
                }
            }
        });
    }
    let mut actual = Vec::new();
    let mut comptime = Vec::new();
    for param in &params[template..] {
        let local = function.body().local(*param);
        let value = bound.next();
        let arg = if value.is_none() { args.next() } else { None };
        let value = match (value, arg, local.is_comptime()) {
            (Some(value), _, _) => Some(value),
            (_, Some(arg), true) => Some(program.eval(frame, arg)?),
            _ => None,
        };
        actual.push(Some(match &value {
            Some(value) => program.static_type(value, span)?,
            None => type_of(
                program,
                frame,
                arg.expect("required arguments were counted"),
            )?,
        }));
        if local.is_comptime() {
            comptime.push(value.expect("comptime arguments were evaluated"));
        }
    }
    let mut instance_args = program.deduce_template(def, explicit, &actual, span)?;
    instance_args.extend(comptime);
    Ok(program.signature(&(def, instance_args), span)?.ret())
}
