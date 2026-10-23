use cx_hmir::{
    HMIRBody,
    expr::{HMIRExprID, op::HMIRTypeOp},
    ty::{
        HMIRExprType, HMIRExprTypeKind, HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRDeclaredMove, HMIRMoveSemantics, HMIRTypeSemantics},
        context::HMIRTypeContext,
    },
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{eval_comptime, eval_type, flow, types},
    log::hmir_error,
    value::{HMIRValueKind, constant::HMIRConstant, scalar::int},
};

pub(crate) fn eval_type_op(
    _context: &mut LocalLoweringContext,
    _body: &HMIRBody,
    _span: &TokenRange,
    op: &HMIRTypeOp,
) -> CXResult<HMIRTypeID> {
    todo!("HMIRTypeOp: {}", op.path())
}

pub(crate) fn eval_declared_type(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> CXResult<HMIRTypeID> {
    if matches!(
        ty.kind(),
        HMIRExprTypeKind::Struct { .. }
            | HMIRExprTypeKind::Union { .. }
            | HMIRExprTypeKind::TaggedUnion { .. }
    ) {
        return eval_aggregate(context, body, declaration, ty);
    }
    if matches!(ty.kind(), HMIRExprTypeKind::Function(_)) {
        let semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
        let unresolved = context.env_mut().resolver_mut().realize_type(HMIRType::new(
            HMIRTypeKind::Opaque {
                size: 0,
                alignment: 1,
            },
            HMIRTypeSemantics::default(),
            ty.decl_span().clone(),
            None,
        ));
        context.env_mut().resolver_mut().mark_incomplete(unresolved);
        let provisional = ty.map(
            &mut (),
            |_, _| CXResult::Ok(unresolved),
            |_, _| unreachable!(),
            |_, _| Ok(semantics),
        )?;
        let id = context.env_mut().resolver_mut().realize_type(provisional);
        context.env_mut().resolver_mut().mark_incomplete(id);
        context.env_mut().resolver_mut().publish_type(id);
        let complete = evaluate(context, body, ty, semantics)?;
        context.env_mut().resolver_mut().complete_type(id, complete);
        return Ok(id);
    }
    let semantics = ty
        .semantics()
        .map(|declared| declared_move(context, body, declared))?;
    let constructor =
        crate::symbol::recursive::TypeConstructor::new(ty.kind(), semantics, ty.decl_span());
    let outer = context.env().resolver().defining_types.len();
    let held = if let Some(constructor) = constructor {
        for frame in &mut context.env_mut().resolver_mut().defining_types {
            frame.constructors.push(constructor.clone());
        }
        None
    } else {
        Some(std::mem::take(
            &mut context.env_mut().resolver_mut().defining_types,
        ))
    };
    let result = (|| {
        if let HMIRExprTypeKind::Nominal(inner) = ty.kind() {
            let inner = flow::top(eval_type(context, body, *inner)?.description())?;
            return Ok(specify(context, inner, semantics, ty.decl_span()));
        }
        let ty = evaluate(context, body, ty, semantics.move_semantics)?;
        types::validate::declaration(context.env(), &ty)?;
        Ok(context.env_mut().resolver_mut().realize_type(ty))
    })();
    if let Some(held) = held {
        context.env_mut().resolver_mut().defining_types = held;
    } else {
        for frame in context
            .env_mut()
            .resolver_mut()
            .defining_types
            .iter_mut()
            .take(outer)
        {
            frame.constructors.pop();
        }
    }
    result
}

// The fields of an aggregate may refer back to it, which finds it incomplete until they are all evaluated
fn eval_aggregate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declaration: HMIRExprID,
    ty: &HMIRExprType,
) -> CXResult<HMIRTypeID> {
    if let Some(id) = context.env().resolver().declared_type(declaration) {
        return Ok(id);
    }

    let reserved = ty.semantics().map(|declared| {
        CXResult::Ok(match declared {
            HMIRDeclaredMove::Explicit(semantics) => *semantics,
            HMIRDeclaredMove::CopyTraits(_) => HMIRMoveSemantics::default(),
        })
    })?;
    let incomplete = HMIRType::new(
        HMIRTypeKind::Opaque {
            size: 0,
            alignment: 1,
        },
        reserved,
        ty.decl_span().clone(),
        ty.debug_name().map(str::to_string),
    );
    let id = context
        .env_mut()
        .resolver_mut()
        .declare_type(declaration, incomplete);
    context.env_mut().resolver_mut().publish_type(id);
    let move_semantics = declared_move(context, body, &ty.semantics().move_semantics)?;
    let complete = evaluate(context, body, ty, move_semantics)?;
    types::validate::declaration(context.env(), &complete)?;
    context.env_mut().resolver_mut().complete_type(id, complete);

    Ok(id)
}

fn evaluate(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    ty: &HMIRExprType,
    move_semantics: HMIRMoveSemantics,
) -> CXResult<HMIRType> {
    let inline = match ty.kind() {
        HMIRExprTypeKind::Array { .. } => Some(Inline::Element),
        HMIRExprTypeKind::Struct { .. } | HMIRExprTypeKind::Union { .. } => Some(Inline::Member),
        HMIRExprTypeKind::TaggedUnion { .. } => Some(Inline::Variant),
        _ => None,
    };

    ty.map(
        context,
        |context, inner| member(context, body, *inner, inline),
        |context, length| array_length(context, body, *length),
        |_, _| Ok(move_semantics),
    )
}

fn declared_move(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    declared: &HMIRDeclaredMove,
) -> CXResult<HMIRMoveSemantics> {
    match declared {
        HMIRDeclaredMove::Explicit(semantics) => Ok(*semantics),
        HMIRDeclaredMove::CopyTraits(source) => {
            let source = flow::top(eval_type(context, body, *source)?.description())?;
            Ok(types::owned_semantics(context.env(), source).move_semantics)
        }
    }
}

#[derive(Clone, Copy)]
enum Inline {
    Element,
    Member,
    Variant,
}

impl Inline {
    fn subject(self) -> &'static str {
        match self {
            Inline::Element => "array element",
            Inline::Member | Inline::Variant => "member",
        }
    }

    fn admits(self, env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
        let void = matches!(env.resolve_kind(env.underlying(ty)), HMIRTypeKind::Void);
        (void && matches!(self, Inline::Variant)) || types::is_complete(env, ty)
    }
}

fn member(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    expr: HMIRExprID,
    inline: Option<Inline>,
) -> CXResult<HMIRTypeID> {
    let ty = flow::top(eval_type(context, body, expr)?.description())?;

    match inline {
        Some(inline) if !inline.admits(context.env(), ty) => {
            let subject = inline.subject();
            let subject = format!("{subject} of type '{}'", types::name(context.env(), ty));
            Err(hmir_error(body.expr(expr).span(), &typecheck::INCOMPLETE_TYPE, subject).into())
        }
        _ => Ok(ty),
    }
}

// Specifiers only accumulate, so a type that already carries them stands for itself
pub(crate) fn specify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    specifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env_mut();
    let current = env.semantics(ty);
    let semantics = current.apply(specifiers);
    if semantics == current {
        return ty;
    }

    let kind = HMIRTypeKind::Nominal(env.underlying(ty));
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
}

fn array_length(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    length: HMIRExprID,
) -> CXResult<u64> {
    let span = body.expr(length).span();
    let value = eval_comptime(context, body, length, None)?;
    let (HMIRTypeKind::Int(ty), HMIRValueKind::Constant(HMIRConstant::Int { value, .. })) =
        (context.env().resolve_kind(value.ty()), value.kind())
    else {
        let subject = ("array length".into(), "integer".into());
        return Err(hmir_error(span, &mir::EXPECTED_CONSTANT, subject).into());
    };

    let value = int::normalize(*value, *ty);
    let (negative, found) = match int::signed(*ty) {
        true => (value < 0, value.to_string()),
        false => (false, (value as u128).to_string()),
    };
    let requirement = match negative {
        true => "a non-negative value".to_string(),
        false => match u64::try_from(value as u128) {
            Ok(length) => return Ok(length),
            Err(_) => format!("a value of at most {}", u64::MAX),
        },
    };

    let subject = ("array length".into(), requirement, Some(found));
    Err(hmir_error(span, &typecheck::TYPE_REQUIREMENT, subject).into())
}
