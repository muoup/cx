use cx_hmir::{
    HMIRContract, HMIRFnSignature, HMIRFunctionStage,
    ty::{
        HMIRTypeID, HMIRTypeKind, aggregate::HMIRFieldDef, context::HMIRTypeContext,
        intrinsic::HMIRIntWidth,
    },
    unit::function::HMIRFnParam,
};
use cx_intrinsics::AggregateIntrinsic;
use cx_log::CXResult;
use cx_mir::MIRFunctionID;
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use super::initializer;
use crate::{
    emit,
    env::{
        HMIREnvironment, function::LocalLoweringContext, generation::Generation, resolver::demand,
    },
    eval::types,
    lower,
    value::{self, HMIRValue, HMIRValueKind, immediate::HMIRImmediate},
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
    Some(Variant {
        index,
        payload: *fields[index].ty(),
    })
}

pub(crate) fn field<'env>(
    env: &'env HMIREnvironment,
    ty: HMIRTypeID,
    variant: usize,
) -> &'env HMIRFieldDef<HMIRTypeID> {
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

pub(crate) fn signature(
    env: &HMIREnvironment,
    ty: HMIRTypeID,
    variant: usize,
) -> HMIRFnSignature<HMIRTypeID> {
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
    demand::prepare(context.env_mut(), ty)?;
    let ty = context.env().underlying(ty);
    let Some(Variant { index, .. }) = lookup(context.env(), ty, name) else {
        return Ok(None);
    };
    let signature = signature(context.env(), ty, index);
    let callable = types::callable(context.env_mut(), signature, span);
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
    lower::declare(
        env,
        Generation::Constructor { ty, variant },
        &signature,
        span,
    )
}

pub(crate) fn construct(
    context: &mut LocalLoweringContext,
    ty: HMIRTypeID,
    variant: usize,
    payload: Option<HMIRValue>,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let payload = payload.unwrap_or_else(|| value::unit(context.env()));
    initializer::initialize(context, ty, &[(variant, payload)], span)
}

// The index of the variant a tagged union holds
pub(crate) fn tag(
    context: &mut LocalLoweringContext,
    value: &HMIRValue,
    span: &TokenRange,
) -> CXResult<HMIRValue> {
    let ty = types::int(context.env_mut(), HMIRIntWidth::I8, false, span);
    if let HMIRValueKind::Immediate(HMIRImmediate::TaggedUnion { variant, .. }) = value.kind() {
        return Ok(value::int(context.env(), *variant as i128, ty));
    }
    let sum = context.env().referent(value.ty());
    let index = AggregateIntrinsic::SumIndex {
        value: value.clone(),
        sum,
    };
    emit::intrinsics::aggregate(context, &index, ty, span)
}
