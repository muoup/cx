pub(crate) mod initializer;
pub(crate) mod pattern;

use initializer::initialize;
use pattern::eval_is;

use cx_hmir::{
    HMIRAggregateOp, HMIRBinaryOp, HMIRBody,
    expr::HMIRExprID,
    ty::{
        HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::catalogue::typecheck;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    emit,
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, eval, eval_type,
        flow::{Evaluation, ready},
        op::{binary_operands, eval_operand, specify},
        place::dereference,
        storage, types,
    },
    log::hmir_error,
    value::HMIRValue,
};

pub(crate) fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    op: &HMIRAggregateOp,
) -> EvalResult<HMIRValue> {
    match op {
        HMIRAggregateOp::Member { base, name } => eval_member(context, body, span, *base, name),
        HMIRAggregateOp::Initialize { ty, fields } => {
            let ty = eval_type(context, body, *ty)?.description()?;
            let fields = fields.clone();
            let span = span.clone();
            ty.and_then(context, body, None, move |context, body, ty, _| {
                initialize(context, body, &span, ty, &fields)
            })
        }
        HMIRAggregateOp::Index { base, index } => {
            let base = ready!(eval_operand(context, body, span, *base));
            let index = ready!(eval_operand(context, body, span, *index));
            let element = binary_operands(context, span, HMIRBinaryOp::Add, base, index)?;

            Ok(Evaluation::Ready(dereference(context, element, span)?))
        }
        HMIRAggregateOp::Is { value, pattern } => eval_is(context, body, span, *value, pattern),
        _ => todo!("HMIROp::AggregateOp({})", op.path()),
    }
}

fn eval_member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    base: HMIRExprID,
    name: &CXIdent,
) -> EvalResult<HMIRValue> {
    let value = ready!(eval(context, body, base));
    let subject = context
        .env()
        .reference_inner(value.ty())
        .unwrap_or(value.ty());
    let indirect = context.env().pointer_inner(subject).is_some()
        || matches!(
            context.env().resolve_kind(subject),
            HMIRTypeKind::Array { .. }
        );
    let mut value = if indirect {
        dereference(context, value, span)?
    } else if context.env().is_reference(value.ty()) {
        value
    } else {
        let storage = storage::temporary(context, value, span)?;
        emit::reference(context, &storage, span)?
    };

    let Some(mut owner) = context.env().reference_inner(value.ty()) else {
        unreachable!("a member base is a reference")
    };
    let Some(path) = member_path(context.env(), owner, name) else {
        return Err(hmir_error(
            span,
            &typecheck::UNKNOWN_MEMBER,
            (types::name(context.env(), owner), name.to_string()),
        )
        .into());
    };

    for index in path {
        let field = &member_fields(context.env(), owner)[index];
        if field.bit_width().is_some() {
            todo!("bitfield member access");
        }

        let mut ty = *field.ty();
        if context.env().semantics(owner).constant {
            ty = specify(
                context,
                ty,
                HMIRTypeSemantics {
                    constant: true,
                    ..Default::default()
                },
                span,
            );
        }
        value = emit::field(context, &value, index, ty, span)?;
        owner = ty;
    }

    Ok(Evaluation::Ready(emit::reference(context, &value, span)?))
}

fn member_fields<'a>(env: &'a HMIREnvironment, ty: HMIRTypeID) -> &'a [HMIRFieldDef<HMIRTypeID>] {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Struct { fields } | HMIRTypeKind::Union { fields } => fields,
        _ => &[],
    }
}

// The field indices that lead to a member, which may sit inside of anonymous members
fn member_path(env: &HMIREnvironment, ty: HMIRTypeID, name: &CXIdent) -> Option<Vec<usize>> {
    let fields = member_fields(env, ty);

    fields
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
