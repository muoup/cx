use std::collections::HashSet;

use cx_hmir::{
    HMIRBody,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{HMIRTypeKind, context::HMIRTypeContext},
};
use cx_log::catalogue::typecheck;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::function::{FnContext, LocalLoweringContext},
    eval::{
        EvalResult, eval,
        flow::{Evaluation, ready},
        literal,
        safety::require_pure,
        storage, types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn eval_unpack(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: HMIRExprID,
    bindings: &[(CXIdent, HMIRLocalID)],
) -> EvalResult<HMIRValue> {
    require_pure(context, span, "@unpack")?;

    let source = ready!(eval(context, body, value));
    let source_ty = source.ty();
    let fields = match (
        context.env().is_reference(source_ty),
        context.env().resolve_kind(source_ty),
    ) {
        (false, HMIRTypeKind::Struct { fields }) => fields.clone(),
        _ => {
            return Err(hmir_error(
                span,
                &typecheck::TYPE_MISMATCH,
                (
                    "@unpack".into(),
                    "owned structured type".into(),
                    types::name(context.env(), source_ty),
                ),
            )
            .into());
        }
    };

    let mut seen_fields = HashSet::new();
    let mut seen_bindings = HashSet::new();
    let mut projections = Vec::with_capacity(bindings.len());

    for (name, local) in bindings {
        let Some((index, field)) = fields
            .iter()
            .enumerate()
            .find(|(_, field)| field.name() == Some(name))
        else {
            return Err(hmir_error(
                span,
                &typecheck::UNKNOWN_MEMBER,
                (types::name(context.env(), source_ty), name.to_string()),
            )
            .into());
        };

        if !seen_fields.insert(index) {
            return Err(hmir_error(
                span,
                &typecheck::DUPLICATE_ITEM,
                (name.to_string(), "@unpack binding".into()),
            )
            .into());
        }
        let binding = body
            .local(*local)
            .name()
            .expect("an unpack binding is named");
        if !seen_bindings.insert(binding.clone()) {
            return Err(hmir_error(
                span,
                &typecheck::INCOMPATIBLE_DECLARATION,
                binding.to_string(),
            ));
        }
        if field.bit_width().is_some() {
            return Err(hmir_error(
                span,
                &typecheck::UNSUPPORTED_FEATURE,
                "unpacking bitfields".into(),
            )
            .into());
        }

        projections.push((index, binding.clone(), *local, *field.ty()));
    }

    for (index, field) in fields.iter().enumerate() {
        if context.env().is_nodrop(*field.ty()) && !seen_fields.contains(&index) {
            let name = field
                .name()
                .map(ToString::to_string)
                .unwrap_or_else(|| format!("field {index}"));
            return Err(hmir_error(
                span,
                &typecheck::MISSING_ENTITY,
                (
                    format!("drop of @nodrop field {name}"),
                    "@unpack statement".into(),
                ),
            )
            .into());
        }
    }

    let constant = matches!(
        source.kind(),
        HMIRValueKind::Constant(HMIRConstant::Aggregate { .. })
    );
    let comptime = matches!(context.context(), FnContext::Comptime(_));
    let source = if constant && !comptime {
        let storage = emit::scratch(context, source_ty, span)?;
        storage::initialize(context, &storage, &source, span)?;
        storage
    } else {
        source
    };

    for (index, name, local, ty) in projections {
        let projected = emit::field(context, &source, index, ty, span)?;
        let value = if constant && comptime {
            projected
        } else {
            let reference = emit::reference(context, &projected, span)?;
            emit::lift(context, &reference, Some(&source), span)?
        };
        let target = emit::allocate(context, ty, Some(name), span)?;
        let reference = types::reference(context.env_mut(), ty, body.local(local).span());
        let target = target.with_ty(reference);
        storage::initialize(context, &target, &value, span)?;
        emit::storage::bind_local(context, local, target);
    }

    emit::invalidate(context, &source, MIRInvalidationKind::Move, span);

    Ok(Evaluation::Ready(literal::unit(context, span)))
}
