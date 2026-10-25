use cx_hmir::{
    HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind, aggregate::HMIRTypeSemantics, context::HMIRTypeContext,
    },
    unit::function::HMIRFnParam,
};
use cx_log::CXResult;
use cx_mir::{
    MIRFnPrototype, MIRFunction, MIRFunctionID, MIRInstructionKind,
    expr::instruction::MIRInvalidationKind,
};
use cx_tokens::TokenRange;
use cx_util::{identifier::CXIdent, linkage::LinkageMode};

use crate::{
    emit::{self, runtime},
    env::{
        HMIREnvironment,
        function::{FnContext, LocalLoweringContext, RuntimeContext},
        generation::Constructor,
    },
    eval::{literal, types},
    value::{
        HMIRValue,
        constant::{HMIRConstant, HMIRFunctionRef},
    },
};

pub(crate) fn variant(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    name: &CXIdent,
) -> Option<(usize, HMIRTypeID)> {
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        return None;
    };
    let index = fields.iter().position(|field| field.name() == Some(name))?;
    Some((index, *fields[index].ty()))
}

pub(crate) fn name(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> String {
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        unreachable!("a constructor builds a tagged union")
    };
    let variant = fields[variant].name().expect("a variant is named");
    format!("{}::{variant}", types::name(env, ty))
}

fn payload(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> Option<HMIRTypeID> {
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        unreachable!("a constructor builds a tagged union")
    };
    let payload = *fields[variant].ty();
    (!matches!(env.resolve_kind(payload), HMIRTypeKind::Void)).then_some(payload)
}

fn signature(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> HMIRFnSignature<HMIRTypeID> {
    let params = payload(env, ty, variant)
        .map(|payload| HMIRFnParam::new(false, payload))
        .into_iter()
        .collect();
    HMIRFnSignature::new(
        HMIRFunctionStage::Runtime,
        params,
        ty,
        false,
        HMIRContract::default(),
    )
}

pub(crate) fn reference(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: &CXIdent,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    context.prepare_type(ty, span)?;
    let ty = context.env().underlying(ty);
    let Some((variant, _)) = self::variant(context.env(), ty, name) else {
        return Ok(None);
    };
    let signature = signature(context.env(), ty, variant);
    let callable = context.env_mut().resolver_mut().realize_type(HMIRType::new(
        HMIRTypeKind::Function(signature),
        HMIRTypeSemantics::default(),
        span.clone(),
        None,
    ));
    let function = HMIRFunctionRef::Constructor { ty, variant };
    Ok(Some(HMIRValue::constant(
        callable,
        HMIRConstant::Function(function),
    )))
}

pub(crate) fn construct(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    variant: usize,
    payload: Option<HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let payload = payload.unwrap_or_else(|| literal::unit(context, span));
    emit::aggregate(context, ty, &[(variant, payload)], span)
}

pub(crate) fn declare(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    variant: usize,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    let function = HMIRFunctionRef::Constructor { ty, variant };
    if let Some(id) = env.generation().generated(function, &[]) {
        return Ok(id);
    }
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        unreachable!("a constructor builds a tagged union")
    };
    let name = fields[variant].name().expect("a variant is named");
    let name = CXIdent::new(format!(
        "{name}.__cx_variant.{}",
        env.generation().generated_count()
    ));
    let signature = types::lower_signature(env, &signature(env, ty, variant), span)?;
    let prototype = MIRFnPrototype::new(signature, LinkageMode::Static, name, None);
    let id = env.module_mut().declare_function(prototype);
    env.generation_mut()
        .construct(Constructor::new(ty, variant, id));
    Ok(id)
}

pub(crate) fn lower(env: &mut HMIREnvironment, constructor: &Constructor) -> CXResult<MIRFunction> {
    let (ty, variant) = (constructor.ty(), constructor.variant());
    let span = TokenRange::internal();
    let prototype = env
        .module()
        .function(constructor.function())
        .prototype()
        .clone();
    let runtime = FnContext::Runtime(RuntimeContext::new(prototype));
    let mut context = LocalLoweringContext::new(env, None, runtime);

    let scope = emit::open_scope(&mut context, span.clone());
    let payload = match payload(context.env(), ty, variant) {
        Some(payload) => {
            let param = runtime::parameter(&mut context, payload, None, &span)?;
            let reference = types::reference(context.env_mut(), payload, None, &span);
            let param = param.with_ty(reference);
            let moved = emit::lift(&mut context, &param, None, &span)?;
            emit::invalidate(&mut context, &param, MIRInvalidationKind::Move, &span);
            Some(moved)
        }
        None => None,
    };
    let value = construct(&mut context, ty, variant, payload, &span)?;
    let value = Some(runtime::lower_value(&mut context, &value, &span)?);
    runtime::drop_scope(&mut context, scope, &span);
    context
        .runtime_mut()
        .emit(MIRInstructionKind::Return { value }, span.clone());
    emit::close_scope(&mut context, span);

    match context.into_context() {
        FnContext::Runtime(runtime) => Ok(runtime.finish()),
        _ => unreachable!("a constructor is lowered into a runtime context"),
    }
}
