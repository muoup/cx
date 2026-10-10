use cx_hmir::{
    HMIRBody, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind},
};
use cx_log::CXResult;
use cx_mir::{MIRBindable, MIRInstructionKind, MIRPlaceDecl, MIRTarget};
use cx_tokens::TokenRange;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{
        HMIRValue,
        coerce::convert,
        dispatch::lower_expression,
        emit::{diverges, drop_scope, rvalue, void_value},
        shape::{TypeShape, is_nodrop, shape},
    },
};

pub(crate) fn lower_local(
    cx: &mut FnLoweringContext,
    local: HMIRLocalID,
) -> CXResult<HMIRValue> {
    match cx.get_local(&local) {
        Some(value) => Ok(value.clone()),
        None => todo!("local {local} has no runtime binding (comptime local used at runtime)"),
    }
}

pub(crate) fn lower_let(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let decl = body.local(local);
    if decl.is_comptime() {
        todo!("comptime let binding");
    }

    let ty = cx.env().eval_type(body, decl.ty())?;
    if shape(cx, ty) == TypeShape::Void {
        todo!("local of type void");
    }

    let allocates = initializer.is_some_and(|initializer| {
        matches!(
            body.expr(initializer).kind(),
            HMIRExprKind::Op(HMIROp::OwnershipOp(HMIROwnershipOp::Allocate(_)))
        )
    });

    let value = match initializer {
        Some(initializer) if !allocates => {
            let value = lower_expression(cx, body, body.expr(initializer))?;
            if diverges(cx, &value) {
                return Ok(value);
            }

            let init_span = body.expr(initializer).span();
            Some(convert(cx, value, ty, init_span)?)
        }
        _ => None,
    };

    let mir = cx.env().lower_type(ty, span)?;
    let nodrop = is_nodrop(cx.env(), ty);
    let scope = cx.builder().current_scope();
    let place = cx.builder().allocate_place(MIRPlaceDecl::new(
        mir,
        decl.name().cloned(),
        scope,
        nodrop,
        false,
    ));

    if let Some(value) = value {
        let value = cx.materialize(&value, span)?;
        cx.builder().emit(
            MIRInstructionKind::Store {
                target: MIRTarget::Place(place),
                value,
                ty: mir,
                bitfield: None,
            },
            span,
        );
        cx.builder().emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span,
        );
    }

    cx.insert_local(local, HMIRValue::place(ty, place));
    Ok(void_value(cx, span))
}

pub(crate) fn lower_block(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
) -> CXResult<HMIRValue> {
    let scope = cx.builder().push_scope(span);

    let mut result = None;
    for statement in statements {
        let value = lower_expression(cx, body, body.expr(*statement))?;
        let diverged = diverges(cx, &value);
        result = Some(value);

        if diverged {
            break;
        }
    }

    let result = match result {
        Some(value) if diverges(cx, &value) => value,
        Some(value) => rvalue(cx, value, span)?,
        None => void_value(cx, span),
    };

    if !cx.builder().terminated() {
        drop_scope(cx, scope, span);
    }
    cx.builder().pop_scope();

    Ok(result)
}

pub(crate) fn lower_scoped(
    cx: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: &HMIRExpr,
) -> CXResult<HMIRValue> {
    if matches!(expr.kind(), HMIRExprKind::Block { .. }) {
        return lower_expression(cx, body, expr);
    }

    let scope = cx.builder().push_scope(expr.span());
    let value = lower_expression(cx, body, expr)?;
    let value = match diverges(cx, &value) {
        true => value,
        false => rvalue(cx, value, expr.span())?,
    };

    if !cx.builder().terminated() {
        drop_scope(cx, scope, expr.span());
    }
    cx.builder().pop_scope();

    Ok(value)
}
