use chl_hmir::{
    HMIRBodyContext, HMIRDefID, HMIRDefKind, HMIRFunction,
    body::{HMIRHole, HMIRLocal},
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind},
};
use chl_log::CHLResult;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{call, name},
    lower::declaration,
};

use super::generates_type;

pub(crate) fn elaborate(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    function: &HMIRFunction,
) -> CHLResult<Option<HMIRFunction>> {
    let body = function.body();
    let mut calls = Vec::new();
    for param in function.signature().params() {
        collect_calls(body, *param.ty(), &mut calls);
    }
    if calls.is_empty() {
        return Ok(None);
    }
    calls.sort_unstable();
    calls.dedup();
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(def.brand());
    let mut expanded = None;
    let mut locals = Vec::new();
    for id in calls {
        let expr = body.expr(id);
        let HMIRExprKind::Call {
            callee,
            args,
            named,
        } = expr.kind()
        else {
            unreachable!()
        };
        let Some(def) = callee_definition(&mut context, body.expr(*callee))? else {
            continue;
        };
        let unit = context.unit(def.brand());
        let source = unit.resolve_def(def).expect("a type call has a definition");
        let Some(HMIRDefKind::Function(factory)) = declaration::canonical(source).kind() else {
            continue;
        };
        if !generates_type(factory) {
            continue;
        }
        let prototype = factory.prototype();
        let default_hole = |index: usize| {
            prototype.params()[index]
                .default()
                .is_some_and(|expr| is_hole(prototype.defaults(), expr))
        };
        if !args
            .iter()
            .chain(named.iter().map(|(_, arg)| arg))
            .any(|&arg| is_hole(body, arg))
            && !(0..prototype.params().len()).any(default_hole)
        {
            continue;
        }
        let (bound, partial) = call::bind(
            Some(factory),
            factory.signature(),
            &[],
            body,
            args,
            named,
            &source.name().to_string(),
            expr.span(),
        )?;
        if partial {
            continue;
        }
        let mut implicit = Vec::new();
        for (index, arg) in bound.into_iter().enumerate() {
            if !factory.signature().is_comptime_param(index) {
                continue;
            }
            let param = &prototype.params()[index];
            if !arg.map_or_else(|| default_hole(index), |arg| is_hole(body, arg)) {
                continue;
            }
            let span = arg.map_or_else(|| expr.span(), |arg| body.expr(arg).span());
            let expanded = expanded.get_or_insert_with(|| function.clone());
            let body = expanded.body_mut();
            let hole = body.declare_hole(HMIRHole::new(span.clone()));
            let ty = body.push_expr(HMIRExpr::new(HMIRExprKind::Hole(hole), span.clone()));
            let local = body.declare_local(HMIRLocal::new(None, ty, span.clone(), true));
            locals.push(local);
            if let Some(arg) = arg {
                *body.expr_mut(arg).kind_mut() = HMIRExprKind::Local(local);
            } else if let Some(name) = param.name() {
                let value = body.push_expr(HMIRExpr::new(HMIRExprKind::Local(local), span.clone()));
                implicit.push((name.clone(), value));
            }
        }
        if !implicit.is_empty() {
            let body = expanded.as_mut().unwrap().body_mut();
            let HMIRExprKind::Call { named, .. } = body.expr_mut(id).kind_mut() else {
                unreachable!()
            };
            named.extend(implicit);
        }
    }
    Ok(expanded.map(|mut function| {
        function.prepend_inferred_parameters(&locals);
        function
    }))
}

fn is_hole(body: &HMIRBodyContext, expr: HMIRExprID) -> bool {
    matches!(body.expr(expr).kind(), HMIRExprKind::Hole(_))
}

fn collect_calls(body: &HMIRBodyContext, id: HMIRExprID, calls: &mut Vec<HMIRExprID>) {
    match body.expr(id).kind() {
        HMIRExprKind::Call { args, named, .. } => {
            calls.push(id);
            for &arg in args.iter().chain(named.iter().map(|(_, arg)| arg)) {
                collect_calls(body, arg, calls);
            }
        }
        HMIRExprKind::Type(ty) => {
            let _: Result<_, ()> = ty.kind().map(
                calls,
                |calls, &inner| {
                    collect_calls(body, inner, calls);
                    Ok(())
                },
                |_, _| Ok(()),
            );
        }
        _ => {}
    }
}

fn callee_definition(
    context: &mut LocalLoweringContext,
    callee: &HMIRExpr,
) -> CHLResult<Option<HMIRDefID>> {
    match callee.kind() {
        HMIRExprKind::Def(def) => Ok(Some(*def)),
        HMIRExprKind::Name { name: symbol, tag } => {
            name::resolve(context, callee.span(), symbol, *tag)
        }
        _ => Ok(None),
    }
}
