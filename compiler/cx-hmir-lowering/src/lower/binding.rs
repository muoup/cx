use cx_hmir::{
    HMIRBody, HMIROp, HMIROwnershipOp,
    body::HMIRLocalID,
    expr::{HMIRExpr, HMIRExprID, HMIRExprKind},
    ty::{HMIRTypeKind, context::HMIRTypeContext},
    value::HMIRValue,
};
use cx_log::CXResult;
use cx_mir::{MIRBindable, MIRInstructionKind, MIRPlaceDecl, MIRTarget};
use cx_tokens::TokenRange;

use crate::{
    env::lowering::FnLoweringContext,
    lower::{coerce::convert, constant, dispatch::lower_expression, emit, types},
};

pub(crate) fn lower_local(
    context: &mut FnLoweringContext,
    span: &TokenRange,
    local: HMIRLocalID,
) -> CXResult<HMIRValue> {
    let Some(value) = context.get_local(&local).cloned() else {
        todo!("local {local} has no runtime binding (comptime local used at runtime)")
    };

    emit::referent(context, value, span)
}

pub(crate) fn lower_let(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    local: HMIRLocalID,
    initializer: Option<HMIRExprID>,
) -> CXResult<HMIRValue> {
    let decl = body.local(local);
    if decl.comptime() {
        todo!("comptime let binding");
    }

    let ty = context.env_mut().eval_type(body, decl.ty())?;
    if matches!(context.env().resolve_type_id(ty).kind(), HMIRTypeKind::Void) {
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
            let initializer = body.expr(initializer);
            let value = lower_expression(context, body, initializer)?;
            if emit::terminated(context.builder()) {
                return Ok(value);
            }

            Some(convert(context, value, ty, initializer.span())?)
        }
        _ => None,
    };

    let mir = types::lower_type(context.env_mut(), ty, span)?;
    let scope = *context
        .open_scopes()
        .last()
        .expect("Let binding lowered outside of a scope");
    let place = context.builder_mut().allocate_place(MIRPlaceDecl::new(
        mir,
        decl.name().cloned(),
        scope,
        decl.nodrop(),
        false,
    ));

    if let Some(value) = value {
        let value = emit::materialize(context, &value, span)?;
        emit::store(
            context.builder_mut(),
            MIRTarget::Place(place),
            value,
            mir,
            span.clone(),
        );
        context.builder_mut().emit(
            MIRInstructionKind::Initialize {
                place: MIRBindable::Place(place),
            },
            span.clone(),
        );
    }

    let binding = context.bind(ty, MIRBindable::Place(place));
    context.insert_local(local, binding);
    Ok(constant::unit(context, span))
}

pub(crate) fn lower_block(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    statements: &[HMIRExprID],
) -> CXResult<HMIRValue> {
    emit::open_scope(context, span.clone());

    let mut result = None;
    for statement in statements {
        result = Some(lower_expression(context, body, body.expr(*statement))?);
        if emit::terminated(context.builder()) {
            break;
        }
    }

    let result = match result {
        Some(value) if emit::terminated(context.builder()) => value,
        Some(value) => emit::read(context, value, span)?,
        None => constant::unit(context, span),
    };
    emit::close_scope(context, span.clone());

    Ok(result)
}

pub(crate) fn lower_scoped(
    context: &mut FnLoweringContext,
    body: &HMIRBody,
    expr: &HMIRExpr,
) -> CXResult<HMIRValue> {
    if matches!(expr.kind(), HMIRExprKind::Block { .. }) {
        return lower_expression(context, body, expr);
    }

    emit::open_scope(context, expr.span().clone());

    let value = lower_expression(context, body, expr)?;
    let value = match emit::terminated(context.builder()) {
        true => value,
        false => emit::read(context, value, expr.span())?,
    };
    emit::close_scope(context, expr.span().clone());

    Ok(value)
}
