mod initializer;
pub(crate) mod pattern;
pub(crate) mod variant;

use rustc_hash::FxHashSet;

use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBodyContext,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRFieldDef, context::HMIRTypeContext},
};
use cx_intrinsics::AggregateIntrinsic;
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{LocalLoweringContext, memory::placement},
    },
    eval::{
        EvalResult,
        arithmetic::{binary_operands, eval_operand},
        coerce, eval, expr,
        flow::{EvalExit, Evaluation},
        place, types,
    },
    log::hmir_error,
    value::{self, HMIRValue},
};

pub(crate) fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    op: &HMIRAggregateOp,
) -> EvalResult<Evaluation<HMIRValue>> {
    match op {
        HMIRAggregateOp::Member { base, name } => eval_member(context, body, span, *base, name),
        HMIRAggregateOp::Initialize { ty, fields } => {
            initializer::eval_initializer(context, body, span, *ty, fields)
        }
        HMIRAggregateOp::Index { base, index } => eval_index(context, body, span, *base, *index),
        HMIRAggregateOp::Is { value, pattern } => {
            pattern::eval_is(context, body, span, *value, pattern)
        }
        HMIRAggregateOp::Unpack { value, bindings } => {
            eval_unpack(context, body, span, *value, bindings)
        }
    }
}

pub(crate) fn eval_index(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    base: HMIRExprID,
    subscript: HMIRExprID,
) -> EvalResult<Evaluation<HMIRValue>> {
    let base = (eval_operand(context, body, span, base))?.ready()?;
    let subscript = (eval_operand(context, body, span, subscript))?.ready()?;
    Ok(Evaluation::Ready(index(context, base, subscript, span)?))
}

// The element 'base[subscript]' references, where either operand may be the pointer
pub(crate) fn index(
    context: &mut LocalLoweringContext,
    base: HMIRValue,
    subscript: HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let pointer = match context.env().pointer_inner(base.ty()) {
        Some(_) => base.clone(),
        None => subscript.clone(),
    };
    let element = binary_operands(context, span, HMIRBinaryOp::Add, base, subscript)?;
    place::dereference(context, element, Some(&pointer), span)
}

pub(crate) fn eval_member(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
    span: &TokenRange,
    base: HMIRExprID,
    name: &CXIdent,
) -> EvalResult<Evaluation<HMIRValue>> {
    let value = eval(context, body, base)?
        .ready()
        .and_then(|value| coerce::aggregate(context, value, &span).map_err(EvalExit::from))?;

    let Some(aggregate_type) = context.env().reference_inner(value.ty()) else {
        unreachable!("coerce::aggregate did not return a reference type")
    };

    let fields = member_fields(context.env(), aggregate_type);
    let Some(index) = fields.iter().position(|field| field.name() == Some(name)) else {
        let member = (types::name(context.env(), aggregate_type), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member).into());
    };
    let ty = *fields[index].ty();

    Ok(Evaluation::Ready(field(context, &value, index, ty, span)?))
}

// The member 'index' of the aggregate 'base' references, referenced as 'ty&'
pub(crate) fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let owner = context.env().referent(base.ty());
    let bitfield = placement(context.env_mut(), owner, index, span)?;
    let reference = types::reference(context.env_mut(), ty, bitfield, span);
    let base = base.clone();
    let projection = match context.env().resolve_kind(owner) {
        HMIRTypeKind::TaggedUnion { .. } => AggregateIntrinsic::SumVariant {
            value: base,
            variant: index,
            sum: owner,
        },
        _ => AggregateIntrinsic::StructField {
            base,
            field: index,
            aggregate: owner,
        },
    };
    emit::intrinsics::aggregate(context, &projection, reference, span)
}

pub(crate) fn eval_unpack(
    context: &mut LocalLoweringContext,
    body: &HMIRBodyContext,
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

    expr::require_impure(context, span, "@unpack")?;
    let source = (eval(context, body, value))?.ready()?;
    let source_ty = source.ty();
    let fields = match context.env().resolve_kind(source_ty) {
        HMIRTypeKind::Struct { fields } if !context.env().is_reference(source_ty) => fields.clone(),
        _ => {
            let mismatch = (
                "@unpack".into(),
                "owned structured type".into(),
                types::name(context.env(), source_ty),
            );
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
        let binding = body
            .local(*local)
            .name()
            .expect("an unpack binding is named");
        if !seen_bindings.insert(binding.clone()) {
            return Err(hmir_error(
                span,
                &typecheck::INCOMPATIBLE_DECLARATION,
                binding.to_string(),
            )
            .into());
        }
        projections.push(Projection {
            index,
            name: binding.clone(),
            local: *local,
            ty: *fields[index].ty(),
        });
    }

    let dropped = fields.iter().enumerate().find(|(index, field)| {
        context.env().is_nodrop(*field.ty()) && !seen_fields.contains(index)
    });
    if let Some((index, field)) = dropped {
        let name = field
            .name()
            .map_or_else(|| format!("field {index}"), ToString::to_string);
        let entity = (
            format!("drop of @nodrop field {name}"),
            "@unpack statement".into(),
        );
        return Err(hmir_error(span, &typecheck::MISSING_ENTITY, entity).into());
    }

    let source = match value::as_place(&source) {
        Some(_) => source,
        None => coerce::temporary(context, &source, span)?,
    };
    for Projection {
        index,
        name,
        local,
        ty,
    } in projections
    {
        let projected = field(context, &source, index, ty, span)?;
        let value = match types::bitfield(context.env(), projected.ty()) {
            Some(_) => emit::storage::read(context, &projected, span)?,
            None => emit::storage::lift(context, &projected, span)?,
        };
        let target = emit::storage::allocate(context, ty, Some(name), span)?;
        emit::storage::initialize(context, &target, Some(&value), span)?;
        emit::storage::bind_local(context, local, target);
    }
    Ok(Evaluation::Ready(expr::unit(context, span)))
}

pub(crate) fn member_fields<'a>(
    env: &'a HMIREnvironment,
    ty: HMIRTypeID,
) -> &'a [HMIRFieldDef<HMIRTypeID>] {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields,
        _ => &[],
    }
}

// The field indices that lead to a member, which may sit inside of anonymous members
pub(crate) fn member_path(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    name: &CXIdent,
) -> Option<Vec<usize>> {
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
