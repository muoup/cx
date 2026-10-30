mod initializer;
pub(crate) mod pattern;
pub(crate) mod variant;

use rustc_hash::FxHashSet;

use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{
        HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::catalogue::typecheck;
use cx_mir::expr::instruction::MIRInvalidationKind;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, coerce, eval, eval_type, expr,
        flow::Evaluation,
        op::{binary_operands, eval_operand},
        place::dereference,
        types,
    },
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRAggregateOp,
) -> EvalResult<Evaluation<HMIRValue>> {
    match op {
        HMIRAggregateOp::Member { base, name } => eval_member(context, body, span, *base, name),
        HMIRAggregateOp::Initialize { ty, fields } => {
            let ty = eval_type(context, body, *ty)?.description()?;
            let fields = fields.clone();
            let span = span.clone();
            ty.and_then(context, body, None, move |context, body, ty, _| {
                initializer::initialize(context, body, &span, ty, &fields)
            })
        }
        HMIRAggregateOp::Index { base, index } => {
            let base = (eval_operand(context, body, span, *base))?.ready()?;
            let index = (eval_operand(context, body, span, *index))?.ready()?;
            let pointer = match context.env().pointer_inner(base.ty()) {
                Some(_) => base.clone(),
                None => index.clone(),
            };
            let element = binary_operands(context, span, HMIRBinaryOp::Add, base, index)?;

            Ok(Evaluation::Ready(dereference(context, element, Some(&pointer), span)?))
        }
        HMIRAggregateOp::Is { value, pattern } => pattern::eval_is(context, body, span, *value, pattern),
        HMIRAggregateOp::Unpack { value, bindings } => unpack(context, body, span, *value, bindings),
    }
}

fn eval_member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    base: HMIRExprID,
    name: &CXIdent,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = (eval(context, body, base))?.ready()?;
    let value = coerce::collapse(context, value, span)?;
    let subject = context.env().referent(value.ty());
    let indirect = context.env().pointer_inner(subject).is_some()
        || matches!(context.env().resolve_kind(subject), HMIRTypeKind::Array { .. });
    let mut value = if indirect {
        dereference(context, value, None, span)?
    } else if context.env().is_reference(value.ty()) {
        value
    } else {
        let storage = emit::temporary(context, value, span)?;
        emit::reference(context, &storage, span)?
    };

    let Some(mut owner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a member base is a reference")
    };
    let Some(path) = member_path(context.env(), owner, name) else {
        let member = (types::name(context.env(), owner), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
    };

    for index in path {
        let mut ty = *member_fields(context.env(), owner)[index].ty();
        let semantics = context.env().semantics(owner);
        if semantics.constant || semantics.volatile {
            let qualifiers = HMIRTypeSemantics {
                constant: semantics.constant,
                volatile: semantics.volatile,
                ..Default::default()
            };
            ty = types::specify(context, ty, qualifiers, span);
        }
        value = emit::field(context, &value, index, ty, span)?;
        owner = ty;
    }

    Ok(Evaluation::Ready(emit::reference(context, &value, span)?))
}

fn unpack(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    value: HMIRExprID,
    bindings: &[(CXIdent, HMIRLocalID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    struct Projection {
        index: usize,
        name: CXIdent,
        local: HMIRLocalID,
        ty: HMIRTypeID,
    }

    expr::require_pure(context, span, "@unpack")?;
    let source = (eval(context, body, value))?.ready()?;
    let source_ty = source.ty();
    let fields = match context.env().resolve_kind(source_ty) {
        HMIRTypeKind::Struct { fields } if !context.env().is_reference(source_ty) => fields.clone(),
        _ => {
            let mismatch = ("@unpack".into(), "owned structured type".into(), types::name(context.env(), source_ty));
            return Err(hmir_error(span, &typecheck::TYPE_MISMATCH, mismatch).into());
        }
    };

    let mut seen_fields = FxHashSet::default();
    let mut seen_bindings = FxHashSet::default();
    let mut projections = Vec::with_capacity(bindings.len());
    for (name, local) in bindings {
        let Some(index) = fields.iter().position(|field| field.name() == Some(name)) else {
            let member = (types::name(context.env(), source_ty), name.to_string());
            return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
        };
        if !seen_fields.insert(index) {
            let item = (name.to_string(), "@unpack binding".into());
            return Err(hmir_error(span, &typecheck::DUPLICATE_ITEM, item).into());
        }
        let binding = body.local(*local).name().expect("an unpack binding is named");
        if !seen_bindings.insert(binding.clone()) {
            return Err(hmir_error(span, &typecheck::INCOMPATIBLE_DECLARATION, binding.to_string()).into());
        }
        projections.push(Projection { index, name: binding.clone(), local: *local, ty: *fields[index].ty() });
    }

    let dropped = fields
        .iter()
        .enumerate()
        .find(|(index, field)| context.env().is_nodrop(*field.ty()) && !seen_fields.contains(index));
    if let Some((index, field)) = dropped {
        let name = field.name().map_or_else(|| format!("field {index}"), ToString::to_string);
        let entity = (format!("drop of @nodrop field {name}"), "@unpack statement".into());
        return Err(hmir_error(span, &typecheck::MISSING_ENTITY, entity).into());
    }

    let constant = matches!(source.kind(), HMIRValueKind::Constant(HMIRConstant::Aggregate { .. }));
    let comptime = !context.is_runtime();
    let source = match constant && !comptime {
        true => emit::temporary(context, source, span)?,
        false => source,
    };
    for Projection { index, name, local, ty } in projections {
        let projected = emit::field(context, &source, index, ty, span)?;
        let value = if constant && comptime {
            projected
        } else {
            let reference = emit::reference(context, &projected, span)?;
            match types::bitfield(context.env(), &reference) {
                Some(_) => emit::copy(context, reference, span)?,
                None => emit::lift(context, &reference, Some(&source), span)?,
            }
        };
        let reference = types::reference(context.env_mut(), ty, None, body.local(local).span());
        let target = emit::allocate(context, ty, Some(name), span)?.with_ty(reference);
        emit::initialize(context, &target, Some(&value), span)?;
        emit::bind_local(context, local, target);
    }
    emit::invalidate(context, &source, MIRInvalidationKind::Move, span);
    Ok(Evaluation::Ready(expr::unit(context, span)))
}

pub(crate) fn member_fields<'a>(env: &'a HMIREnvironment, ty: HMIRTypeID) -> &'a [HMIRFieldDef<HMIRTypeID>] {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields,
        _ => &[],
    }
}

// The field indices that lead to a member, which may sit inside of anonymous members
pub(crate) fn member_path(env: &HMIREnvironment, ty: HMIRTypeID, name: &CXIdent) -> Option<Vec<usize>> {
    member_fields(env, ty)
        .iter()
        .enumerate()
        .find_map(|(index, field)| match field.name() {
            Some(field_name) if field_name == name => Some(vec![index]),
            Some(_) => None,
            None => {
                let mut path = member_path(env, *field.ty(), name)?;
                path.insert(0, index);
                Some(path)
            }
        })
}
