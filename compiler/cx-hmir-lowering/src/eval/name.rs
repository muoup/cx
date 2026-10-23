use cx_hir::{
    ast::types::HIRTagKind,
    intrinsic_types::{HIRIntrinsicType, INTRINSIC_TYPES},
};
use cx_hmir::{
    HMIRDefID,
    ty::{
        HMIRType, HMIRTypeKind,
        aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatType, HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
    },
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_namespace::module::QualifiedName;
use cx_tokens::TokenRange;

use crate::{
    env::function::LocalLoweringContext,
    eval::{EvalResult, complete, def::eval_def},
    log::hmir_error,
    symbol,
    value::HMIRValueKind,
};

pub(crate) fn eval_name(
    context: &mut LocalLoweringContext,
    span: &TokenRange,
    name: &QualifiedName,
    tag: Option<HIRTagKind>,
) -> EvalResult<crate::value::HMIRValue> {
    if let Some(id) = resolve(context, span, name, tag)? {
        return eval_def(context, span, id);
    }
    if tag.is_none()
        && let Some(value) = constructor(context, name, span)?
    {
        return Ok(super::flow::Evaluation::Ready(value));
    }
    if tag.is_none()
        && let Some(name) = name.root_name_ref()
        && let Some((_, intrinsic)) = INTRINSIC_TYPES
            .iter()
            .find(|(builtin, _)| *builtin == name.as_str())
        && let Some(intrinsic) = intrinsic(context.env().architecture())
    {
        let kind = match intrinsic {
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
            HIRIntrinsicType::Opaque { size, alignment } => {
                HMIRTypeKind::Opaque { size, alignment }
            }
        };
        let ty = context.env_mut().resolver_mut().realize_type(HMIRType::new(
            kind,
            HMIRTypeSemantics::default(),
            span.clone(),
            Some(name.to_string()),
        ));
        return complete::type_result(context, ty, span);
    }
    Err(hmir_error(
        span,
        &typecheck::UNKNOWN_SYMBOL,
        name.to_string(),
    ))
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
            (HMIRTypeKind::ReferenceTo(a), HMIRTypeKind::ReferenceTo(b)) => {
                symbol::composite::alike(env, *a, *b, true)
            }
            _ => false,
        },
    )
}

fn constructor(
    context: &mut LocalLoweringContext,
    name: &QualifiedName,
    span: &TokenRange,
) -> CXResult<Option<crate::value::HMIRValue>> {
    use crate::value::{HMIRValue, constant::HMIRConstant};
    use cx_hmir::{HMIRContract, HMIRFnSignature, HMIRFunctionStage, unit::function::HMIRFnParam};
    let segments = name.namespace.segments();
    let Some((parent, namespace)) = segments.split_last() else {
        return Ok(None);
    };
    let parent = QualifiedName::new(
        cx_namespace::module::NamespacePath::new(namespace.to_vec()),
        parent.clone(),
    );
    let Some(id) = resolve(context, span, &parent, None)? else {
        return Ok(None);
    };
    let point = context.visible_through();
    let parent = symbol::binding(context.env_mut(), id, point)?;
    let HMIRValueKind::Constant(HMIRConstant::Type(ty)) = parent.kind() else {
        return Ok(None);
    };
    let ty = *ty;
    let HMIRTypeKind::TaggedUnion { fields } = context.env().resolve_kind(ty) else {
        return Ok(None);
    };
    let Some(field) = fields.iter().find(|field| field.name() == Some(&name.name)) else {
        return Ok(None);
    };
    let payload = *field.ty();
    if matches!(context.env().resolve_kind(payload), HMIRTypeKind::Void) {
        todo!("tagged union initializers")
    }
    let signature = HMIRFnSignature::new(
        HMIRFunctionStage::Runtime,
        vec![HMIRFnParam::new(false, payload)],
        ty,
        false,
        HMIRContract::default(),
    );
    let callable = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Function(signature),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    Ok(Some(HMIRValue::constant(
        callable,
        HMIRConstant::Constructor {
            ty,
            variant: name.name.clone(),
        },
    )))
}
