mod initializer;
pub(crate) mod pattern;
pub(crate) mod variant;

use rustc_hash::FxHashSet;

use chl_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBodyContext,
    body::HMIRLocalID,
    expr::HMIRExprID,
    ty::{
        HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use chl_intrinsics::AggregateIntrinsic;
use chl_log::{CHLResult, catalogue::typecheck};
use chl_tokens::TokenRange;
use chl_util::identifier::CHLIdent;

use crate::{
    emit,
    env::{
        HMIREnvironment,
        function::{LocalLoweringContext, memory::member_layout},
    },
    eval::{
        EvalResult,
        arithmetic::{binary_operands, eval_operand},
        coerce, comptime, deduce, eval, expr,
        flow::Evaluation,
        place, types,
    },
    log::hmir_error,
    value::{self, HMIRValue, HMIRValueKind, immediate::HMIRImmValue},
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
) -> CHLResult<HMIRValue> {
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
    name: &CHLIdent,
) -> EvalResult<Evaluation<HMIRValue>> {
    let base = eval(context, body, base)?.ready()?;
    let base = coerce::collapse(context, base, span)?;
    let owner = context.env().referent(base.ty());
    let opaque = matches!(base.kind(), HMIRValueKind::Immediate(HMIRImmValue::Opaque));
    if context.is_runtime() && types::comptime_only(context.env(), owner) && !opaque {
        let mut child = comptime::child(context);
        let field = member(&mut child, base, name, span)?;
        let value = emit::comptime::storage::contents(&mut child, &field, span)?;
        return Ok(Evaluation::Ready(comptime::converge(
            &mut child, value, span,
        )?));
    }
    Ok(Evaluation::Ready(member(context, base, name, span)?))
}

fn member(
    context: &mut LocalLoweringContext,
    base: HMIRValue,
    name: &CHLIdent,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let opaque = matches!(base.kind(), HMIRValueKind::Immediate(HMIRImmValue::Opaque));
    let mut value = if opaque {
        base
    } else {
        coerce::aggregate(context, base, span)?
    };
    let mut ty = context.env().referent(value.ty());
    let Some(path) = member_path(context.env(), ty, name) else {
        let member = (types::name(context.env(), ty), name.to_string());
        return Err(hmir_error(span, &typecheck::UNKNOWN_MEMBER, member));
    };
    for index in path {
        let field_ty = *member_fields(context.env(), ty)[index].ty();
        if !opaque {
            value = field(context, &value, index, field_ty, span)?;
        }
        ty = field_ty;
    }
    if opaque {
        return deduce::placeholder(context, ty, span);
    }
    Ok(value)
}

// The member 'index' of the aggregate 'base' references, referenced as 'ty&' with the owner's qualifiers
pub(crate) fn field(
    context: &mut LocalLoweringContext,
    base: &HMIRValue,
    index: usize,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CHLResult<HMIRValue> {
    let owner = context.env().referent(base.ty());
    let semantics = context.env().semantics(owner);
    let qualifiers = HMIRTypeSemantics {
        constant: semantics.constant,
        volatile: semantics.volatile,
        ..Default::default()
    };
    let ty = types::qualify::specify(context, ty, qualifiers, span);
    let bitfield = match member_fields(context.env(), owner)
        .get(index)
        .and_then(HMIRFieldDef::bit_width)
    {
        Some(_) => member_layout(context.env_mut(), owner, index, span)?.1,
        None => None,
    };
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
    bindings: &[(CHLIdent, HMIRLocalID)],
) -> EvalResult<Evaluation<HMIRValue>> {
    struct Projection {
        index: usize,
        name: CHLIdent,
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
        let value = if types::bitfield(context.env(), projected.ty()).is_some() {
            emit::storage::read(context, &projected, span)?
        } else {
            emit::storage::lift(context, &projected, span)?
        };
        let target = emit::storage::allocate(context, ty, Some(name), span)?;
        emit::storage::initialize(context, &target, Some(&value), span)?;
        emit::storage::bind_local(context, local, target);
    }
    Ok(Evaluation::Ready(value::unit(context.env())))
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
    name: &CHLIdent,
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
