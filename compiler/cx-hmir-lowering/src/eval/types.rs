use cx_hmir::{
    HMIRBody,
    ty::{
        HMIRType, HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRTypeSemantics,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatWidth, HMIRIntType, HMIRIntWidth},
    },
    unit::function::HMIRFnPrototype,
};
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{
    MIRFloatType, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRIntType, MIRType, MIRTypeID,
    MIRTypeKind,
};
use cx_tokens::TokenRange;

use crate::{
    env::{HMIREnvironment, function::LocalLoweringContext},
    eval::{EvalResult, eval_type},
    log::log_hmir_error,
};

pub(crate) fn lower_prototype(
    context: &mut LocalLoweringContext,
    body: &HMIRBody,
    prototype: &HMIRFnPrototype,
) -> EvalResult<MIRFnPrototype> {
    let signature = prototype.signature();

    let mut params = Vec::with_capacity(signature.params().len());
    for param in signature.params() {
        let ty = eval_type(context, body, *param.ty())?;
        let ty = lower_type(context.env_mut(), ty, body.expr(*param.ty()).span())?;

        params.push(MIRFnParam::new(None, ty, false));
    }

    let return_type = eval_type(context, body, *signature.return_type())?;
    let return_type = lower_type(
        context.env_mut(),
        return_type,
        body.expr(*signature.return_type()).span(),
    )?;

    Ok(MIRFnPrototype::new(
        MIRFnSignature::new(
            params,
            return_type,
            signature.is_variadic(),
            signature.contract().is_safe(),
        ),
        prototype.linkage(),
        prototype.symbol_name().clone(),
        None,
    ))
}

pub(crate) fn lower_type(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    if let Some(id) = env.resolver().lowered_type(ty) {
        return Ok(id);
    }

    let kind = match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => MIRTypeKind::Void,
        HMIRTypeKind::Type | HMIRTypeKind::StagedExpr { .. } => {
            return log_hmir_error(span, &typecheck::COMPTIME_ONLY_TYPE, todo!("type display"));
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
        HMIRTypeKind::Function(_) => {
            todo!("function types")
        }
        HMIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },

        _ => todo!(),
    };

    let id = env.module_mut().intern_type(MIRType::new(kind));
    env.resolver_mut().cache_lowered(ty, id);
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
