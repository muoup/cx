pub(crate) mod validate;

use cx_hmir::{
    HMIRBody, HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunctionStage,
    expr::HMIRExprID,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::{HMIRFieldDef, HMIRTypeSemantics},
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
    },
    unit::{def::HMIRDeclarationPoint, function::HMIRFnParam},
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{
    MIRField, MIRFloatType, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRIntType, MIRType,
    MIRTypeID, MIRTypeKind,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::{EvalResult, eval_type, flow, op::specify, types},
    log::{hmir_error, log_hmir_error},
    symbol,
};

pub(crate) fn eval_signature(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    signature: &HMIRFnSignature<HMIRExprID>,
) -> EvalResult<HMIRFnSignature<HMIRTypeID>> {
    let evaluated = signature.map(|ty| flow::top(eval_type(context, body, *ty)?.description()))?;
    Ok(flow::Evaluation::Ready(adjust_signature(
        context, body, signature, &evaluated,
    )?))
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
            context.force_type(*param.ty(), span)?;
            let ty = parameter(context, *param.ty(), span);
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
    span: &TokenRange,
) -> HMIRTypeID {
    let pointee = match element(context, ty, span) {
        Some(element) => element,
        None if matches!(context.env().resolve_kind(ty), HMIRTypeKind::Function(_)) => ty,
        None => return ty,
    };

    pointer(context.env_mut(), pointee, span)
}

pub(crate) fn element(
    context: &mut LocalLoweringContext,
    array: HMIRTypeID,
    span: &TokenRange,
) -> Option<HMIRTypeID> {
    let HMIRTypeKind::Array { element, .. } = *context.env().resolve_kind(array) else {
        return None;
    };
    if !context.env().semantics(array).constant {
        return Some(element);
    }

    let constant = HMIRTypeSemantics {
        constant: true,
        ..Default::default()
    };
    Some(specify(context, element, constant, span))
}

pub(crate) fn lower_signature(
    env: &mut HMIREnvironment,
    signature: &HMIRFnSignature<HMIRTypeID>,
    span: &TokenRange,
) -> CXResult<MIRFnSignature> {
    let params = signature
        .params()
        .iter()
        .map(|param| Ok(MIRFnParam::new(None, lower_type(env, *param.ty(), span)?)))
        .collect::<CXResult<_>>()?;
    let return_type = lower_type(env, *signature.return_type(), span)?;

    Ok(MIRFnSignature::new(
        params,
        return_type,
        signature.is_variadic(),
        signature.contract().is_safe(),
    ))
}

pub(crate) fn lower_prototype(
    env: &mut HMIREnvironment,
    def: HMIRDefID,
    point: Option<HMIRDeclarationPoint>,
    span: &TokenRange,
) -> CXResult<MIRFnPrototype> {
    let symbol_name = symbol::link_name(env, def, point)?.expect("a prototype names a function");
    let linkage = symbol::linkage(env, def, point)?;
    let value = symbol::binding(env, def, point)?;
    if matches!(env.resolve_kind(value.ty()), HMIRTypeKind::FunctionItem(_)) {
        let source = env.symbols().resolve_definition(def);
        let selected = symbol::select(source, point)?;
        if let Some(HMIRDefKind::Function(function)) = source.declarations()[selected].kind()
            && function.signature().stage() == HMIRFunctionStage::Runtime
        {
            return Err(hmir_error(
                span,
                &typecheck::UNSUPPORTED_FEATURE,
                "runtime function specialization".into(),
            ));
        }
        return Err(hmir_error(
            span,
            &mir::COMPTIME_FUNCTION_AT_RUNTIME,
            symbol_name.to_string(),
        ));
    }
    let signature = symbol::signature(env, def, point)?;
    Ok(MIRFnPrototype::new(
        lower_signature(env, &signature, span)?,
        linkage,
        symbol_name,
        None,
    ))
}

pub(crate) fn lower_type(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    {
        let mut context = LocalLoweringContext::new(
            env,
            None,
            FnContext::Comptime(ComptimeContext::new()),
        );
        context.prepare_type(ty, span)?;
    }
    let ty = env.underlying(ty);
    if let Some(id) = env.resolver().lowered_type(ty) {
        return Ok(id);
    }

    let kind = match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Struct { fields } => {
            let kind = |fields| MIRTypeKind::Structured { fields };
            return lower_aggregate(env, ty, &fields, kind, span);
        }
        HMIRTypeKind::Union { fields } => {
            let kind = |variants| MIRTypeKind::Union { variants };
            return lower_aggregate(env, ty, &fields, kind, span);
        }
        HMIRTypeKind::TaggedUnion { fields } => {
            let kind = |variants| MIRTypeKind::TaggedUnion { variants };
            return lower_aggregate(env, ty, &fields, kind, span);
        }
        HMIRTypeKind::Nominal(_) => unreachable!("an underlying type is not nominal"),

        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => MIRTypeKind::Void,
        HMIRTypeKind::Type | HMIRTypeKind::StagedExpr { .. } | HMIRTypeKind::FunctionItem(_) => {
            return log_hmir_error(span, &typecheck::COMPTIME_ONLY_TYPE, types::name(env, ty));
        }
        HMIRTypeKind::Str => MIRTypeKind::Str,
        HMIRTypeKind::Int(int) => MIRTypeKind::Integer {
            ty: mir_int(int.width()),
        },
        HMIRTypeKind::Float(float) => MIRTypeKind::Float {
            ty: mir_float(float.width()),
        },
        HMIRTypeKind::PointerTo(inner) => MIRTypeKind::PointerTo {
            inner: lower_type(env, inner, span)?,
        },
        HMIRTypeKind::ReferenceTo(inner) => MIRTypeKind::MemoryReference {
            inner: lower_type(env, inner, span)?,
        },
        HMIRTypeKind::Array { element, length } => {
            let inner = lower_type(env, element, span)?;
            match length {
                Some(length) => MIRTypeKind::Array {
                    length: length as usize,
                    inner,
                },
                None => MIRTypeKind::IncompleteArray { inner },
            }
        }
        HMIRTypeKind::Function(signature) => MIRTypeKind::Function {
            signature: lower_signature(env, &signature, span)?,
        },
        HMIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },
    };

    let id = env.module_mut().intern_type(MIRType::new(kind));
    env.resolver_mut().cache_lowered(ty, id);
    Ok(id)
}

fn lower_aggregate(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    fields: &[HMIRFieldDef<HMIRTypeID>],
    kind: impl FnOnce(Vec<MIRField>) -> MIRTypeKind,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    let id = env.module_mut().reserve_type();
    env.resolver_mut().cache_lowered(ty, id);

    let fields = fields
        .iter()
        .map(|field| {
            let name = field.name().map(CXIdent::as_string);
            let ty = lower_type(env, *field.ty(), span)?;

            Ok(match field.bit_width() {
                Some(width) => MIRField::Bitfield {
                    name,
                    integer_type_id: ty,
                    width,
                },
                None => MIRField::Standard { name, type_id: ty },
            })
        })
        .collect::<CXResult<_>>()?;

    let debug_name = env.resolve_type_id(ty).debug_name().map(str::to_string);
    env.module_mut()
        .define_type(id, MIRType::new(kind(fields)), debug_name);
    Ok(id)
}

pub(crate) fn int(
    env: &mut HMIREnvironment,
    width: HMIRIntWidth,
    signed: bool,
    span: &TokenRange,
) -> HMIRTypeID {
    let kind = HMIRTypeKind::Int(HMIRIntType::new(width, signed));
    env.resolver_mut().realize_type(HMIRType::new(
        kind,
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
}

pub(crate) fn universe(env: &mut HMIREnvironment, span: &TokenRange) -> HMIRTypeID {
    env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Type,
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
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
    let semantics = env.semantics(ty);
    env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Array { element, length: Some(length) },
        semantics,
        span.clone(),
        None,
    ))
}

pub(crate) fn unsized_array(env: &HMIREnvironment, ty: HMIRTypeID) -> bool {
    matches!(env.resolve_kind(ty), HMIRTypeKind::Array { length: None, .. })
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

    if env.resolver().is_incomplete(ty) {
        return false;
    }

    match env.resolve_kind(ty) {
        HMIRTypeKind::Array { element, length } => length.is_some() && is_complete(env, *element),
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => false,
        _ => true,
    }
}

pub(crate) fn reference(env: &mut HMIREnvironment, inner: HMIRTypeID, span: &TokenRange) -> HMIRTypeID {
    env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::ReferenceTo(inner),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
}

pub(crate) fn pointer(
    env: &mut HMIREnvironment,
    inner: HMIRTypeID,
    span: &TokenRange,
) -> HMIRTypeID {
    env.resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::PointerTo(inner),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ))
}

pub(crate) fn name(env: &HMIREnvironment, ty: HMIRTypeID) -> String {
    let constant = env.semantics(ty).constant;
    let ty = env.resolve_type_id(env.underlying(ty));
    let base = match ty.kind() {
        HMIRTypeKind::Nominal(_) => unreachable!("underlying type is not nominal"),
        _ if ty.debug_name().is_some() => ty.debug_name().unwrap().to_owned(),
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
        HMIRTypeKind::ReferenceTo(inner) => format!("{}&", name(env, *inner)),
        HMIRTypeKind::Array { element, length } => format!(
            "{}[{}]",
            name(env, *element),
            length.map(|n| n.to_string()).unwrap_or_default()
        ),
        HMIRTypeKind::Struct { .. } => "struct".into(),
        HMIRTypeKind::Union { .. } => "union".into(),
        HMIRTypeKind::TaggedUnion { .. } => "enum union".into(),
        HMIRTypeKind::Function(_) => "function".into(),
        HMIRTypeKind::FunctionItem(_) => "comptime callable".into(),
        HMIRTypeKind::StagedExpr { .. } => "expr".into(),
        HMIRTypeKind::Opaque { .. } => "opaque".into(),
    };
    if constant {
        format!("const {base}")
    } else {
        base
    }
}

pub(crate) fn mir_int(width: HMIRIntWidth) -> MIRIntType {
    match width {
        HMIRIntWidth::I1 => MIRIntType::I1,
        HMIRIntWidth::I8 => MIRIntType::I8,
        HMIRIntWidth::I16 => MIRIntType::I16,
        HMIRIntWidth::I32 => MIRIntType::I32,
        HMIRIntWidth::I64 => MIRIntType::I64,
        HMIRIntWidth::I128 => MIRIntType::I128,
    }
}

pub(crate) fn mir_float(width: HMIRFloatWidth) -> MIRFloatType {
    match width {
        HMIRFloatWidth::F32 => MIRFloatType::F32,
        HMIRFloatWidth::F64 => MIRFloatType::F64,
    }
}
