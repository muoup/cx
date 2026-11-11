use cx_hmir::{
    HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    ty::{HMIRTypeID, HMIRTypeKind, aggregate::HMIRFieldDef, context::HMIRTypeContext},
    unit::function::HMIRFnParam,
};
use cx_log::CXResult;
use cx_mir::MIRFunctionID;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::{
        HMIREnvironment, function::LocalLoweringContext, generation::Generation, resolver::demand,
    },
    eval::{expr, helpers, types},
    lower,
    value::{HMIRValue, immediate::HMIRImmediate},
};

pub(crate) struct Variant {
    pub index: usize,
    pub payload: HMIRTypeID,
}

pub(crate) fn lookup(env: &HMIREnvironment, ty: HMIRTypeID, name: &CXIdent) -> Option<Variant> {
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        return None;
    };
    let index = fields.iter().position(|field| field.name() == Some(name))?;
    Some(Variant { index, payload: *fields[index].ty() })
}

pub(crate) fn field<'env>(env: &'env HMIREnvironment, ty: HMIRTypeID, variant: usize) -> &'env HMIRFieldDef<HMIRTypeID> {
    let HMIRTypeKind::TaggedUnion { fields } = env.resolve_kind(ty) else {
        unreachable!("a constructor builds a tagged union")
    };
    &fields[variant]
}

pub(crate) fn name(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> String {
    let variant = field(env, ty, variant).name().expect("a variant is named");
    format!("{}::{variant}", types::name(env, ty))
}

pub(crate) fn payload(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> Option<HMIRTypeID> {
    let payload = *field(env, ty, variant).ty();
    (!env.is_void(payload)).then_some(payload)
}

pub(crate) fn signature(env: &HMIREnvironment, ty: HMIRTypeID, variant: usize) -> HMIRFnSignature<HMIRTypeID> {
    let params = payload(env, ty, variant)
        .map(|payload| HMIRFnParam::new(false, payload))
        .into_iter()
        .collect();
    HMIRFnSignature::new(HMIRFunctionStage::Runtime, params, ty, false, HMIRContract::default())
}

pub(crate) fn reference(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    name: &CXIdent,
    span: &TokenRange,
) -> CXResult<Option<HMIRValue>> {
    demand::prepare(context.env_mut(), ty)?;
    let ty = context.env().underlying(ty);
    let Some(Variant { index, .. }) = lookup(context.env(), ty, name) else {
        return Ok(None);
    };
    let signature = signature(context.env(), ty, index);
    let callable = types::realize(context.env_mut(), HMIRTypeKind::Function(signature), span);
    let constructor = HMIRImmediate::Constructor { ty, variant: index };
    Ok(Some(HMIRValue::new(callable, constructor)))
}

// The generated function that constructs the variant, for a constructor lowered as a runtime operand
pub(crate) fn function(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    variant: usize,
    span: &TokenRange,
) -> CXResult<MIRFunctionID> {
    let signature = signature(env, ty, variant);
    lower::declare(env, Generation::Constructor { ty, variant }, &signature, span)
}

pub(crate) fn construct(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    variant: usize,
    payload: Option<HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let payload = payload.unwrap_or_else(|| expr::unit(context, span));
    helpers::aggregate(context, ty, &[(variant, payload)], span)
}
