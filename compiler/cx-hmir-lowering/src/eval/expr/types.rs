mod construct;
mod layout;

pub(crate) use construct::{construct_type, eval_declared_type};
pub(crate) use layout::eval_type_op;

use cx_hmir::{
    HMIRBody, HMIRFunction, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRBitfield, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::{HMIRIntType, HMIRIntWidth},
    },
    unit::function::HMIRFnParam,
    HMIRFnSignature,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, eval_type, flow::{self, Evaluation}, staged},
    log::hmir_error,
    value::{HMIRValue, HMIRValueKind},
};

// A slot naming a parameter is only fixed by the arguments of a call, so the shape leaves it dependent
pub(crate) fn eval_signature(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
) -> EvalResult<Evaluation<HMIRFnSignature<HMIRTypeID>>> {
    let (body, signature) = (function.body(), function.signature());
    let slot = |context: &mut LocalLoweringContext, expr: HMIRExprID| {
        let references = staged::references(body, expr);
        match function.params().iter().any(|param| references.contains(param)) {
            true => Ok(realize(context.env_mut(), HMIRTypeKind::Dependent, body.expr(expr).span())),
            false => flow::top(eval_type(context, body, expr).and_then(|ty| ty.description())),
        }
    };
    let params = signature
        .params()
        .iter()
        .map(|param| {
            let ty = slot(context, *param.ty())?;
            let ty = parameter(context, ty, param.comptime(), body.expr(*param.ty()).span())?;
            Ok(HMIRFnParam::new(param.comptime(), ty))
        })
        .collect::<CXResult<_>>()?;
    let return_type = slot(context, *signature.return_type())?;
    Ok(Evaluation::Ready(HMIRFnSignature::new(
        signature.stage(),
        params,
        return_type,
        signature.is_variadic(),
        signature.contract().clone(),
    )))
}

pub(crate) fn adjust_signature(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    source: &HMIRFnSignature<HMIRExprID>,
    evaluated: &HMIRFnSignature<HMIRTypeID>,
) -> CXResult<HMIRFnSignature<HMIRTypeID>> {
    let params = source
        .params()
        .iter()
        .zip(evaluated.params())
        .map(|(source, param)| {
            let span = body.expr(*source.ty()).span();
            let ty = parameter(context, *param.ty(), param.comptime(), span)?;
            Ok(HMIRFnParam::new(param.comptime(), ty))
        })
        .collect::<CXResult<_>>()?;

    Ok(HMIRFnSignature::new(
        evaluated.stage(),
        params,
        *evaluated.return_type(),
        evaluated.is_variadic(),
        evaluated.contract().clone(),
    ))
}

pub(crate) fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    comptime: bool,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    context.force_type(ty, span)?;
    let pointee = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Type if !comptime => {
            let kind = (
                "an '@type' parameter".into(),
                "a function signature unless explicitly marked 'comptime'".into(),
            );
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, kind).into());
        }
        HMIRTypeKind::Array { .. } => element(context, ty, span).unwrap(),
        HMIRTypeKind::Function(_) if !comptime_only(context.env(), ty) => ty,
        _ => return Ok(ty),
    };

    Ok(pointer(context.env_mut(), pointee, span))
}

pub(crate) fn element(
    context: &mut LocalLoweringContext,
    array: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let HMIRTypeKind::Array { element, .. } = *context.env().resolve_kind(array) else {
        return None;
    };
    match context.env().semantics(array).constant {
        true => Some(specify(context, element, HMIRTypeSemantics { constant: true, ..Default::default() }, span)),
        false => Some(element),
    }
}

pub(crate) fn realize(env: &mut HMIREnvironment, kind: HMIRTypeKind, span: &TokenRange) -> HMIRTypeID {
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, HMIRTypeSemantics::default(), span.clone(), None))
}

pub(crate) fn int(
    env: &mut HMIREnvironment,
    width: HMIRIntWidth,
    signed: bool,
    span: &TokenRange,
) -> HMIRTypeID {
    realize(env, HMIRTypeKind::Int(HMIRIntType::new(width, signed)), span)
}

pub(crate) fn universe(env: &mut HMIREnvironment, span: &TokenRange) -> HMIRTypeID {
    realize(env, HMIRTypeKind::Type, span)
}

pub(crate) fn pointer(env: &mut HMIREnvironment, inner: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    realize(env, HMIRTypeKind::PointerTo(inner), span)
}

pub(crate) fn reference(
    env: &mut HMIREnvironment,
    inner: HMIRTypeID,
    bitfield: Option<HMIRBitfield>,
    span: &TokenRange,
) -> HMIRTypeID {
    realize(env, HMIRTypeKind::ReferenceTo { inner, bitfield }, span)
}

pub(crate) fn array(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    length: u64,
    span: &TokenRange,
) -> HMIRTypeID {
    let HMIRTypeKind::Array { element, .. } = *env.resolve_kind(ty) else {
        unreachable!("only an array has its length completed")
    };
    let kind = HMIRTypeKind::Array { element, length: Some(length) };
    let semantics = env.semantics(ty);
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
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

pub(crate) fn unspecify(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    qualifiers: HMIRTypeSemantics,
    span: &TokenRange,
) -> HMIRTypeID {
    let env = context.env();
    let current = env.semantics(ty);
    let semantics = HMIRTypeSemantics {
        constant: current.constant && !qualifiers.constant,
        volatile: current.volatile && !qualifiers.volatile,
        ..current
    };
    if semantics == current {
        return ty;
    }

    let underlying = env.underlying(ty);
    let resolved = env.resolve_type_id(underlying);
    let base = resolved.semantics();
    let kind = match resolved.kind() {
        _ if (!base.constant || semantics.constant) && (!base.volatile || semantics.volatile) => {
            HMIRTypeKind::Nominal(underlying)
        }
        kind @ (HMIRTypeKind::Int(_) | HMIRTypeKind::Float(_) | HMIRTypeKind::PointerTo(_)) => {
            kind.clone()
        }
        _ => return ty,
    };
    context
        .env_mut()
        .resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
}

pub(crate) fn owned_semantics(env: &HMIREnvironment, ty: HMIRTypeID) -> HMIRTypeSemantics {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. } => owned_semantics(env, *element),
        HMIRTypeKind::Struct { .. }
        | HMIRTypeKind::Union { .. }
        | HMIRTypeKind::TaggedUnion { .. } => env.semantics(ty),
        _ => HMIRTypeSemantics::default(),
    }
}

pub(crate) fn is_complete(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    let ty = env.underlying(ty);
    !env.resolver().is_incomplete(ty)
        && match env.resolve_kind(ty) {
            HMIRTypeKind::Array { element, length } => length.is_some() && is_complete(env, *element),
            HMIRTypeKind::Void | HMIRTypeKind::Unreachable => false,
            _ => true,
        }
}

// The bitfield behind a reference, or behind the reference an indirect place was reached through
pub(crate) fn bitfield(env: &HMIREnvironment, value: &HMIRValue) -> Option<HMIRBitfield> {
    let reference = match value.kind() {
        HMIRValueKind::Indirect(pointer) => pointer.ty(),
        _ => value.ty(),
    };
    match env.resolve_kind(reference) {
        HMIRTypeKind::ReferenceTo { bitfield, .. } => *bitfield,
        _ => None,
    }
}

pub(crate) fn comptime_only(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Type | HMIRTypeKind::Staged(_) | HMIRTypeKind::Dependent => true,
        HMIRTypeKind::Function(signature) => {
            signature.stage() == HMIRFunctionStage::Comptime
                || signature
                    .params()
                    .iter()
                    .any(|param| param.comptime() || comptime_only(env, *param.ty()))
                || comptime_only(env, *signature.return_type())
        }
        _ => false,
    }
}

pub(crate) fn name(env: &HMIREnvironment, ty: HMIRTypeID) -> String {
    let constant = env.semantics(ty).constant;
    let ty = env.resolve_type_id(env.underlying(ty));
    let base = match ty.kind() {
        HMIRTypeKind::Nominal(_) => unreachable!("underlying type is not nominal"),
        _ if let Some(name) = ty.debug_name() => name.to_owned(),
        HMIRTypeKind::Void => "void".into(),
        HMIRTypeKind::Unreachable => "unreachable".into(),
        HMIRTypeKind::Type => "@type".into(),
        HMIRTypeKind::Str => "str".into(),
        HMIRTypeKind::Int(int) => format!(
            "{}{}",
            if int.signed() { "i" } else { "u" },
            int.width().bytes() * 8
        ),
        HMIRTypeKind::Float(float) => format!("f{}", float.width().bytes() * 8),
        HMIRTypeKind::PointerTo(inner) => format!("{}*", name(env, *inner)),
        HMIRTypeKind::ReferenceTo { inner, bitfield: Some(bitfield) } => {
            format!("{}&[{}:{}]", name(env, *inner), bitfield.offset, bitfield.width)
        }
        HMIRTypeKind::ReferenceTo { inner, bitfield: None } => format!("{}&", name(env, *inner)),
        HMIRTypeKind::Array { element, length } => format!(
            "{}[{}]",
            name(env, *element),
            length.map(|n| n.to_string()).unwrap_or_default()
        ),
        HMIRTypeKind::Struct { .. } => "struct".into(),
        HMIRTypeKind::Union { .. } => "union".into(),
        HMIRTypeKind::TaggedUnion { .. } => "enum union".into(),
        HMIRTypeKind::Function(_) => "function".into(),
        HMIRTypeKind::Staged(_) => "expr".into(),
        HMIRTypeKind::Dependent => "_".into(),
        HMIRTypeKind::Opaque { .. } => "opaque".into(),
    };
    match constant {
        true => format!("const {base}"),
        false => base,
    }
}

pub(crate) fn validate(env: &HMIREnvironment, ty: &HMIRType) -> CXResult<()> {
    let fields = match ty.kind() {
        HMIRTypeKind::Struct { fields }
        | HMIRTypeKind::Union { fields }
        | HMIRTypeKind::TaggedUnion { fields } => fields,
        HMIRTypeKind::PointerTo(inner)
            if matches!(env.resolve_kind(*inner), HMIRTypeKind::Unreachable) =>
        {
            let requirement = ("pointer target".into(), "a reachable type".into(), None);
            return Err(hmir_error(ty.decl_span(), &typecheck::TYPE_REQUIREMENT, requirement).into());
        }
        _ => return Ok(()),
    };
    let semantics = ty.semantics();
    for field in fields {
        let owned = owned_semantics(env, *field.ty());
        let required = if owned.move_semantics.is_nodrop() && !semantics.move_semantics.is_nodrop() {
            "@nodrop"
        } else if owned.move_semantics.is_nocopy() && !semantics.move_semantics.is_nocopy() {
            "@nocopy"
        } else if owned.unsafe_move && !semantics.unsafe_move {
            "@unsafe_move"
        } else {
            continue;
        };
        let name = field.name().map(|name| name.as_str()).unwrap_or("<anonymous>");
        let trait_ = (name.into(), required.into(), required.into());
        return Err(hmir_error(ty.decl_span(), &typecheck::FIELD_TRAIT, trait_).into());
    }
    Ok(())
}
