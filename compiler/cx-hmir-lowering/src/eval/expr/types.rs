mod construct;
mod layout;

pub(crate) mod qualify;

pub(crate) use construct::{construct_type, hole_bounded, undefined_array};
pub(crate) use layout::eval_type_op;

use cx_hmir::{
    HMIRBodyContext, HMIRFnSignature, HMIRFunction, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, HMIRUndefined,
        aggregate::{HMIRBitfield, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatType, HMIRIntType, HMIRIntWidth},
    },
    unit::function::HMIRFnParam,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{
        EvalResult, eval_type,
        flow::{self, Evaluation},
        staged,
    },
    log::hmir_error,
    value::immediate::HMIRImmediate,
};

// A slot naming a parameter is only fixed by the arguments of a call, so the shape leaves it dependent
pub(crate) fn eval_signature(
    context: &mut LocalLoweringContext,
    function: &HMIRFunction,
) -> EvalResult<Evaluation<HMIRFnSignature<HMIRTypeID>>> {
    let (body, signature) = (function.body(), function.signature());
    let slot = |context: &mut LocalLoweringContext, expr: HMIRExprID| {
        let references = staged::references(body, expr);
        match function
            .params()
            .iter()
            .any(|param| references.contains(param))
        {
            true => Ok(realize(
                context.env_mut(),
                HMIRTypeKind::Dependent,
                body.expr(expr).span(),
            )),
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
    body: &HMIRBodyContext,
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

// 'signature' without the parameters that 'bound' holds of, by index
pub(crate) fn residual(
    signature: &HMIRFnSignature<HMIRTypeID>,
    bound: impl Fn(usize) -> bool,
) -> HMIRFnSignature<HMIRTypeID> {
    let params = signature
        .params()
        .iter()
        .enumerate()
        .filter(|(index, _)| !bound(*index))
        .map(|(_, param)| param.clone())
        .collect();
    HMIRFnSignature::new(
        signature.stage(),
        params,
        *signature.return_type(),
        signature.is_variadic(),
        signature.contract().clone(),
    )
}

pub(crate) fn parameter(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    comptime: bool,
    span: &TokenRange,
) -> CXResult<HMIRTypeID> {
    let pointee = match context.env().resolve_kind(ty) {
        HMIRTypeKind::Type if !comptime => {
            let kind = (
                "an '@type' parameter".into(),
                "a function signature unless explicitly marked 'comptime'".into(),
            );
            return Err(hmir_error(span, &typecheck::INVALID_CONTEXT, kind));
        }
        HMIRTypeKind::Array { .. } | HMIRTypeKind::Undefined(HMIRUndefined::Array { .. }) => {
            element(context, ty, span).unwrap()
        }
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
    let element = context.env().array_inner(array)?;
    match context.env().semantics(array).constant {
        true => Some(qualify::specify(
            context,
            element,
            HMIRTypeSemantics {
                constant: true,
                ..Default::default()
            },
            span,
        )),
        false => Some(element),
    }
}

pub(crate) fn realize(
    env: &mut HMIREnvironment,
    kind: HMIRTypeKind,
    span: &TokenRange,
) -> HMIRTypeID {
    env.resolver_mut().realize_type(HMIRType::new(
        kind,
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
}

pub(crate) fn int(
    env: &mut HMIREnvironment,
    width: HMIRIntWidth,
    signed: bool,
    span: &TokenRange,
) -> HMIRTypeID {
    realize(
        env,
        HMIRTypeKind::Int(HMIRIntType::new(width, signed)),
        span,
    )
}

pub(crate) fn unit(_: &mut HMIREnvironment) -> HMIRTypeID {
    HMIRTypeID::new(0)
}

pub(crate) fn int_type(env: &HMIREnvironment, ty: HMIRTypeID) -> HMIRIntType {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Int(int) => *int,
        _ => unreachable!("'{}' is not an integer type", name(env, ty)),
    }
}

pub(crate) fn float_type(env: &HMIREnvironment, ty: HMIRTypeID) -> HMIRFloatType {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Float(float) => *float,
        _ => unreachable!("'{}' is not a floating type", name(env, ty)),
    }
}

pub(crate) fn universe(env: &mut HMIREnvironment, span: &TokenRange) -> HMIRTypeID {
    realize(env, HMIRTypeKind::Type, span)
}

pub(crate) fn pointer(
    env: &mut HMIREnvironment,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
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
    let HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) = *env.resolve_kind(ty) else {
        unreachable!("only an undefined array has its length completed")
    };
    let kind = HMIRTypeKind::Array { element, length };
    let semantics = env.semantics(ty);
    env.resolver_mut()
        .realize_type(HMIRType::new(kind, semantics, span.clone(), None))
}

pub(crate) fn owned_semantics(env: &HMIREnvironment, ty: HMIRTypeID) -> HMIRTypeSemantics {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. }
        | HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
            owned_semantics(env, *element)
        }
        HMIRTypeKind::Struct { .. }
        | HMIRTypeKind::Union { .. }
        | HMIRTypeKind::TaggedUnion { .. } => env.semantics(ty),
        _ => HMIRTypeSemantics::default(),
    }
}

pub(crate) fn zero(env: &HMIREnvironment, ty: HMIRTypeID) -> Option<HMIRImmediate> {
    Some(match env.resolve_kind(ty) {
        HMIRTypeKind::Int(_) | HMIRTypeKind::PointerTo(_) => HMIRImmediate::Int(0),
        HMIRTypeKind::Float(_) => HMIRImmediate::Float(0.0.into()),
        HMIRTypeKind::Void => HMIRImmediate::Unit,
        HMIRTypeKind::Array { element, length } => HMIRImmediate::Aggregate {
            fields: vec![zero(env, *element)?; *length as usize],
        },
        HMIRTypeKind::Struct { fields } => HMIRImmediate::Aggregate {
            fields: fields
                .iter()
                .map(|field| zero(env, *field.ty()))
                .collect::<Option<_>>()?,
        },
        HMIRTypeKind::Union { fields } => zero(env, *fields.first()?.ty())?,
        HMIRTypeKind::TaggedUnion { fields } => HMIRImmediate::TaggedUnion {
            variant: 0,
            payload: Box::new(zero(env, *fields.first()?.ty())?),
        },
        _ => return None,
    })
}

pub(crate) fn is_complete(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    match env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, .. } => is_complete(env, *element),
        HMIRTypeKind::Undefined(_) | HMIRTypeKind::Unreachable => false,
        _ => true,
    }
}

// Types are interned, so two are one type when they qualify one underlying type alike; constness is only
// compared when 'qualified'
pub(crate) fn alike(env: &HMIREnvironment, a: HMIRTypeID, b: HMIRTypeID, qualified: bool) -> bool {
    let (mut left, mut right) = (env.semantics(a), env.semantics(b));
    if !qualified {
        left.constant = false;
        right.constant = false;
    }
    left == right && env.underlying(a) == env.underlying(b)
}

// Declarations of one function agree apart from the qualifiers of their parameters and result
pub(crate) fn signatures_alike(
    env: &HMIREnvironment,
    a: &HMIRFnSignature<HMIRTypeID>,
    b: &HMIRFnSignature<HMIRTypeID>,
) -> bool {
    a.stage() == b.stage()
        && a.contract().is_safe() == b.contract().is_safe()
        && a.is_variadic() == b.is_variadic()
        && a.params().len() == b.params().len()
        && a.params()
            .iter()
            .zip(b.params())
            .all(|(a, b)| a.comptime() == b.comptime() && alike(env, *a.ty(), *b.ty(), false))
        && alike(env, *a.return_type(), *b.return_type(), false)
}

pub(crate) fn bitfield(env: &HMIREnvironment, reference: HMIRTypeID) -> Option<HMIRBitfield> {
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
        HMIRTypeKind::ReferenceTo {
            inner,
            bitfield: Some(bitfield),
        } => {
            format!(
                "{}&[{}:{}]",
                name(env, *inner),
                bitfield.offset,
                bitfield.width
            )
        }
        HMIRTypeKind::ReferenceTo {
            inner,
            bitfield: None,
        } => format!("{}&", name(env, *inner)),
        HMIRTypeKind::Array { element, length } => format!("{}[{length}]", name(env, *element)),
        HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => {
            format!("{}[]", name(env, *element))
        }
        HMIRTypeKind::Undefined(HMIRUndefined::Tagged(tag)) => tag.prefix().into(),
        HMIRTypeKind::Builtin { name, .. } => name.clone(),
        HMIRTypeKind::Struct { .. } => "struct".into(),
        HMIRTypeKind::Union { .. } => "union".into(),
        HMIRTypeKind::TaggedUnion { .. } => "enum union".into(),
        HMIRTypeKind::Function(_) => "function".into(),
        HMIRTypeKind::Staged(_) => "expr".into(),
        HMIRTypeKind::Dependent => "_".into(),
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
            return Err(hmir_error(
                ty.decl_span(),
                &typecheck::TYPE_REQUIREMENT,
                requirement,
            ));
        }
        _ => return Ok(()),
    };
    let semantics = ty.semantics();
    for field in fields {
        let owned = owned_semantics(env, *field.ty());
        let required = if owned.move_semantics.is_nodrop() && !semantics.move_semantics.is_nodrop()
        {
            "@nodrop"
        } else if owned.move_semantics.is_nocopy() && !semantics.move_semantics.is_nocopy() {
            "@nocopy"
        } else if owned.unsafe_move && !semantics.unsafe_move {
            "@unsafe_move"
        } else {
            continue;
        };
        let name = field
            .name()
            .map(|name| name.as_str())
            .unwrap_or("<anonymous>");
        let trait_ = (name.into(), required.into(), required.into());
        return Err(hmir_error(ty.decl_span(), &typecheck::FIELD_TRAIT, trait_));
    }
    Ok(())
}
