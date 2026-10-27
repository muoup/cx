use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunctionStage,
    ty::{
        HMIRTypeID, HMIRTypeKind,
        aggregate::HMIRFieldDef,
        context::HMIRTypeContext,
        intrinsic::{HMIRFloatWidth, HMIRIntWidth},
    },
    unit::def::HMIRDeclarationPoint,
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
    eval::types::{comptime_only, name},
    log::{hmir_error, log_hmir_error},
    symbol,
};

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
    if comptime_only(env, value.ty()) {
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
    if let Some(id) = env.resolver().lowered_type(env.underlying(ty)) {
        return Ok(id);
    }
    {
        let mut context =
            LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()));
        context.prepare_type(ty, span)?;
    }
    let ty = env.underlying(ty);
    if let Some(id) = env.resolver().lowered_type(ty) {
        return Ok(id);
    }
    if comptime_only(env, ty) {
        return log_hmir_error(span, &typecheck::COMPTIME_ONLY_TYPE, name(env, ty));
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
        HMIRTypeKind::Type | HMIRTypeKind::Staged(_) | HMIRTypeKind::Dependent => {
            unreachable!("comptime-only types are not lowered")
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
        HMIRTypeKind::ReferenceTo { inner, .. } => MIRTypeKind::MemoryReference {
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
