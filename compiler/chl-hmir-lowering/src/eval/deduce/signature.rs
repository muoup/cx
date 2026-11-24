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
    let mut parameters = Vec::new();
    for param in function.signature().params() {
        collect_calls(body, *param.ty(), true, &mut parameters);
    }
    let mut alias = Vec::new();
    if let Some(root) = function.transparent_body() {
        collect_calls(body, root, true, &mut alias);
    }
    let mut calls = parameters
        .into_iter()
        .map(|id| (id, true))
        .chain(alias.into_iter().map(|id| (id, false)))
        .collect::<Vec<_>>();
    if calls.is_empty() {
        return Ok(None);
    }
    calls.sort_unstable();
    calls.dedup();
    calls.reverse();
    let comptime = FnContext::Comptime(ComptimeContext::new());
    let mut context = LocalLoweringContext::new(env, None, comptime).sourced(def.brand());
    let mut expanded = None;
    let mut locals = Vec::new();
    while let Some((id, parameter_pattern)) = calls.pop() {
        let expr = body.expr(id);
        let (callee, args, named) = match expr.kind() {
            HMIRExprKind::Call {
                callee,
                args,
                named,
            } => (*callee, &args[..], &named[..]),
            _ => (id, &[][..], &[][..]),
        };
        let Some(def) = callee_definition(&mut context, body.expr(callee))? else {
            continue;
        };
        let unit = context.unit(def.brand());
        let source = unit.resolve_def(def).expect("a type call has a definition");
        let Some(HMIRDefKind::Function(factory)) = declaration::canonical(source).kind() else {
            continue;
        };
        let implicit_call = callee == id;
        let generates_type = generates_type(factory);
        let applicable = match implicit_call {
            true => factory.returns_type() && call::accepts_empty(factory, &[]),
            false => generates_type,
        };
        if !applicable {
            continue;
        }
        let normalize_alias = implicit_call && !parameter_pattern;
        let infer_arguments = parameter_pattern && generates_type;
        let prototype = factory.prototype();
        let default_hole = |index: usize| {
            prototype.params()[index]
                .default()
                .is_some_and(|expr| is_hole(prototype.defaults(), expr))
        };
        let mut supplied = args
            .iter()
            .copied()
            .chain(named.iter().map(|(_, arg)| *arg));
        let has_holes = infer_arguments
            && (supplied.clone().any(|arg| is_hole(body, arg))
                || (0..prototype.params().len()).any(default_hole));
        if !has_holes && !normalize_alias && !supplied.any(|arg| is_name(body.expr(arg))) {
            continue;
        }
        let aligned = call::bind(
            Some(factory),
            factory.signature(),
            &[],
            body,
            args,
            named,
            &source.name().to_string(),
            expr.span(),
        );
        let (bound, partial) = match aligned {
            Err(_) if !has_holes => continue,
            aligned => aligned?,
        };
        if partial {
            continue;
        }
        let mut implicit = Vec::new();
        for (index, (arg, parameter)) in bound
            .into_iter()
            .zip(factory.signature().params())
            .enumerate()
        {
            if let Some(arg) = arg
                && is_name(body.expr(arg))
                && factory.body().is_type_kind(*parameter.ty())
            {
                calls.push((arg, parameter_pattern));
            }
            if !infer_arguments || !factory.signature().is_comptime_param(index) {
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
        if normalize_alias || !implicit.is_empty() {
            let body = expanded.get_or_insert_with(|| function.clone()).body_mut();
            if implicit_call {
                let callee = body.push_expr(expr.clone());
                *body.expr_mut(id).kind_mut() = HMIRExprKind::Call {
                    callee,
                    args: Vec::new(),
                    named: implicit,
                };
                continue;
            }
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

fn is_name(expr: &HMIRExpr) -> bool {
    matches!(
        expr.kind(),
        HMIRExprKind::Def(_) | HMIRExprKind::Name { .. }
    )
}

fn collect_calls(
    body: &HMIRBodyContext,
    id: HMIRExprID,
    type_position: bool,
    calls: &mut Vec<HMIRExprID>,
) {
    match body.expr(id).kind() {
        HMIRExprKind::Call { args, named, .. } => {
            calls.push(id);
            for &arg in args.iter().chain(named.iter().map(|(_, arg)| arg)) {
                collect_calls(body, arg, false, calls);
            }
        }
        HMIRExprKind::Type(ty) => {
            let _: Result<_, ()> = ty.kind().map(
                calls,
                |calls, &inner| {
                    collect_calls(body, inner, true, calls);
                    Ok(())
                },
                |_, _| Ok(()),
            );
        }
        HMIRExprKind::Def(_) | HMIRExprKind::Name { .. } if type_position => calls.push(id),
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
