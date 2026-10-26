use cx_hir::{
    ast::types::HIRTagKind,
    intrinsic_types::{HIRIntrinsicType, INTRINSIC_TYPES},
};
use cx_hmir::{
    HMIRBody, HMIRDefID,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatType, HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
    },
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_namespace::module::{NamespacePath, QualifiedName};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::function::LocalLoweringContext,
    eval::{
        EvalResult, complete, constructor,
        def::eval_def,
        eval_type,
        flow::Evaluation,
        types,
    },
    log::hmir_error,
    symbol,
    value::{HMIRValue, HMIRValueKind, constant::HMIRConstant},
};

pub(crate) fn eval_name(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    name: &QualifiedName,
    tag: Option<HIRTagKind>,
) -> EvalResult<HMIRValue> {
    if let Some(id) = resolve(context, span, name, tag)? {
        return eval_def(context, span, id);
    }
    if tag.is_none()
        && let Some(value) = qualified_constructor(context, name, span)?
    {
        return Ok(Evaluation::Ready(value));
    }
    if tag.is_none()
        && let Some(name) = name.root_name_ref()
        && let Some(ty) = intrinsic_type(context, name.as_str(), span)
    {
        return complete::type_result(context, ty, span);
    }
    Err(hmir_error(
        span,
        &typecheck::UNKNOWN_SYMBOL,
        name.to_string(),
    ))
}

pub(crate) fn intrinsic_type(
    context: &mut LocalLoweringContext,
    name: &str,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let (_, intrinsic) = INTRINSIC_TYPES
        .iter()
        .find(|(builtin, _)| *builtin == name)?;
    let kind = match intrinsic(context.env().architecture())? {
        HIRIntrinsicType::Void => HMIRTypeKind::Void,
        HIRIntrinsicType::Unreachable => HMIRTypeKind::Unreachable,
        HIRIntrinsicType::Str => HMIRTypeKind::Str,
        HIRIntrinsicType::Bool => HMIRTypeKind::Int(HMIRIntType::new(HMIRIntWidth::I1, false)),
        HIRIntrinsicType::Integer { signed, bytes } => HMIRTypeKind::Int(HMIRIntType::new(
            HMIRIntWidth::from_bytes(bytes as usize).unwrap(),
            signed,
        )),
        HIRIntrinsicType::Float { bytes } => HMIRTypeKind::Float(HMIRFloatType::new(
            HMIRFloatWidth::from_bytes(bytes as usize).unwrap(),
        )),
        HIRIntrinsicType::Opaque { size, alignment } => HMIRTypeKind::Opaque { size, alignment },
    };
    Some(context.env_mut().resolver_mut().realize_type(HMIRType::new(
        kind,
        HMIRTypeSemantics::default(),
        span.clone(),
        Some(name.to_string()),
    )))
}

pub(crate) fn resolve(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    name: &QualifiedName,
    tag: Option<HIRTagKind>,
) -> CXResult<Option<HMIRDefID>> {
    let source = context.source();
    let point = context.visible_through();
    let mut ids = context.env_mut().symbols_mut().lookup(source, name, tag);
    ids.retain(|id| {
        let definition = context.env().symbols().resolve_definition(*id);
        symbol::visible(definition, point).next().is_some()
    });
    if let Some((&first, rest)) = ids.split_first() {
        if let Some(tag) = tag {
            let def = context.env().symbols().resolve_definition(first);
            if def
                .declarations()
                .iter()
                .any(|decl| decl.tag() != Some(tag))
            {
                return Err(hmir_error(
                    span,
                    &typecheck::INCOMPATIBLE_TAG,
                    name.to_string(),
                ));
            }
        }
        let multiple_definitions = ids
            .iter()
            .filter(|id| {
                context
                    .env()
                    .symbols()
                    .resolve_definition(**id)
                    .declarations()
                    .iter()
                    .any(|decl| decl.role() == cx_hmir::HMIRDeclarationRole::Definition)
            })
            .count()
            > 1;
        for other in rest {
            if multiple_definitions || !equivalent(context, first, *other)? {
                let names = ids
                    .iter()
                    .map(|id| {
                        let unit = context
                            .env_mut()
                            .symbols_mut()
                            .owner(id.brand())
                            .expect("candidate owner is available");
                        format!(
                            "{} (module '{}')",
                            unit.resolve_def(*id).unwrap().name(),
                            unit.namespace()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                return Err(hmir_error(span, &typecheck::AMBIGUOUS_SYMBOL, names));
            }
        }
        return Ok(Some(first));
    }
    Ok(None)
}

fn equivalent(context: &mut LocalLoweringContext, a: HMIRDefID, b: HMIRDefID) -> CXResult<bool> {
    use cx_util::linkage::LinkageMode;
    let point = context.visible_through();
    let env = context.env_mut();
    let (a_value, b_value) = (
        symbol::binding(env, a, point)?,
        symbol::binding(env, b, point)?,
    );
    let (Some(a_name), Some(b_name)) = (
        symbol::link_name(env, a, point)?,
        symbol::link_name(env, b, point)?,
    ) else {
        return Ok(false);
    };
    if a_name != b_name
        || symbol::linkage(env, a, point)? == LinkageMode::Static
        || symbol::linkage(env, b, point)? == LinkageMode::Static
    {
        return Ok(false);
    }

    Ok(
        match (
            env.resolve_kind(a_value.ty()),
            env.resolve_kind(b_value.ty()),
        ) {
            (HMIRTypeKind::Function(a), HMIRTypeKind::Function(b)) => {
                symbol::composite::signatures_alike(env, a, b)
            }
            (HMIRTypeKind::ReferenceTo { .. }, HMIRTypeKind::ReferenceTo { .. }) => {
                symbol::composite::alike(env, a_value.ty(), b_value.ty(), true)
            }
            _ => false,
        },
    )
}

fn qualified_constructor(
    context: &mut LocalLoweringContext,
    name: &QualifiedName,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    let segments = name.namespace.segments();
    let Some((parent, namespace)) = segments.split_last() else {
        return Ok(None);
    };
    let parent = QualifiedName::new(NamespacePath::new(namespace.to_vec()), parent.clone());
    for tag in [None, Some(HIRTagKind::Union)] {
        let Some(id) = resolve(context, span, &parent, tag)? else {
            continue;
        };
        let point = context.visible_through();
        let parent = symbol::binding(context.env_mut(), id, point)?;
        let HMIRValueKind::Constant(HMIRConstant::Type(ty)) = parent.kind() else {
            continue;
        };
        if let Some(value) = constructor::reference(context, *ty, &name.name, span)? {
            return Ok(Some(value));
        }
    }
    Ok(None)
}

pub(crate) fn eval_scope_access(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    span: &TokenRange,
    base: HMIRExprID,
    member: &CXIdent,
) -> EvalResult<HMIRValue> {
    let evaluated = eval_type(context, body, base)?;
    let span = span.clone();
    let member = member.clone();
    evaluated.and_then(context, body, None, move |context, _, ty, _| {
        if !matches!(context.env().resolve_kind(ty), HMIRTypeKind::TaggedUnion { .. }) {
            let kind = (types::name(context.env(), ty), "a tagged union".into());
            return Err(hmir_error(&span, &typecheck::UNEXPECTED_KIND, kind));
        }
        match constructor::reference(context, ty, &member, &span)? {
            Some(value) => Ok(Evaluation::Ready(value)),
            None => {
                let member = (types::name(context.env(), ty), member.to_string());
                Err(hmir_error(&span, &typecheck::UNKNOWN_MEMBER, member))
            }
        }
    })
}
