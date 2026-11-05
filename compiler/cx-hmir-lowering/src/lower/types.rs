use cx_hir::ast::types::HIRTagKind;
use cx_hmir::{
    HMIRDefID, HMIRDefKind, HMIRFnSignature, HMIRFunctionStage, ty::{
        HMIRTypeID, HMIRTypeKind, HMIRUndefined, aggregate::HMIRFieldDef, context::HMIRTypeContext, intrinsic::{HMIRFloatWidth, HMIRIntWidth},
    }, unit::def::HMIRDeclarationPoint,
};
use cx_log::{
    CXResult,
    catalogue::{mir, typecheck},
};
use cx_mir::{
    MIRField, MIRFloatType, MIRFnParam, MIRFnPrototype, MIRFnSignature, MIRIntType, MIRType,
    MIRTypeLayout, MIRUndefined,
    MIRTypeID, MIRTypeKind, MIRTypeMetadata, ty::interface::MTRegistry,
};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    env::{
        HMIREnvironment,
        function::{ComptimeContext, FnContext, LocalLoweringContext},
    },
    eval::types::{comptime_only, name, validate},
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
    let underlying = settle(env, ty, span)?;
    let metadata = lower_metadata(env, ty);
    let base = match env.resolver().lowered_type(underlying) {
        Some(id) => id,
        None => lower_underlying(env, underlying, span)?,
    };

    Ok(qualify(env, base, metadata))
}

// Evaluates a pending definition of 'ty', which may alias it to another type
fn settle(env: &mut HMIREnvironment, ty: HMIRTypeID, span: &TokenRange) -> CXResult<HMIRTypeID> {
    LocalLoweringContext::new(env, None, FnContext::Comptime(ComptimeContext::new()))
        .force_type(ty, span)?;

    Ok(env.underlying(ty))
}

// C qualifies the elements of a qualified array rather than the array itself
fn qualify(env: &mut HMIREnvironment, base: MIRTypeID, metadata: MIRTypeMetadata) -> MIRTypeID {
    if metadata.is_empty() {
        return base;
    }

    let resolved = env.module().resolve_type(base);
    match *resolved.kind() {
        MIRTypeKind::Array { length, inner } => {
            let inner = qualify(env, inner, metadata);
            env.module_mut()
                .intern_type(MIRType::new(MIRTypeKind::Array { length, inner }))
        }
        
        MIRTypeKind::Undefined(MIRUndefined::Array { element }) => {
            let element = qualify(env, element, metadata);
            env.module_mut()
                .intern_type(MIRType::new(MIRTypeKind::Undefined(MIRUndefined::Array { element })))
        }

        _ => {
            // Specifiers are monotonic, thus if the to-be-added is a subset of the already added, we can
            // just reuse the base.
            if resolved.metadata().contains(metadata) {
                return base;
            }
            
            if let Some(id) = env.module().qualified(base, metadata) {
                return id;
            }

            let ty = qualified_copy(env, base, metadata);
            let debug_name = env.module().debug_name(base).map(str::to_string);
            let id = env.module_mut().add_type(ty, debug_name);
            env.module_mut().cache_qualified(base, metadata, id);

            id
        }
    }
}

// A copy of an aggregate still being lowered is incomplete, and is redefined alongside it
fn qualified_copy(env: &HMIREnvironment, base: MIRTypeID, metadata: MIRTypeMetadata) -> MIRType {
    let resolved = env.module().resolve_type(base);
    MIRType::qualified(resolved.kind().clone(), resolved.metadata().with(metadata))
}

fn lower_metadata(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
) -> MIRTypeMetadata {
    let semantics = env.semantics(ty);
    let mut metadata = MIRTypeMetadata::NONE;

    if semantics.constant {
        metadata = metadata.with(MIRTypeMetadata::CONST);
    }
    if semantics.volatile {
        metadata = metadata.with(MIRTypeMetadata::VOLATILE);
    }

    metadata
}

fn lower_underlying(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    if comptime_only(env, ty) {
        return log_hmir_error(span, &typecheck::COMPTIME_ONLY_TYPE, name(env, ty));
    }
    validate(env, env.resolve_type_id(ty))?;

    let kind = match env.resolve_kind(ty).clone() {
        HMIRTypeKind::Struct { fields } => {
            let kind = |fields| MIRTypeKind::Structured { fields };
            return lower_aggregate(env, ty, &fields, MIRUndefined::Struct, kind, span);
        }
        HMIRTypeKind::Union { fields } => {
            let kind = |variants| MIRTypeKind::Union { variants };
            return lower_aggregate(env, ty, &fields, MIRUndefined::Union, kind, span);
        }
        HMIRTypeKind::TaggedUnion { fields } => {
            let kind = |variants| MIRTypeKind::TaggedUnion { variants };
            return lower_aggregate(env, ty, &fields, MIRUndefined::TaggedUnion, kind, span);
        }
        HMIRTypeKind::Undefined(HMIRUndefined::Tagged(tag)) => {
            let undefined = MIRType::new(MIRTypeKind::Undefined(mir_tag(tag)));
            let debug_name = env.resolve_type_id(ty).debug_name().map(str::to_string);
            return Ok(lower_named(env, ty, undefined, debug_name));
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
        HMIRTypeKind::Array { element, length } => MIRTypeKind::Array {
            length: length as usize,
            inner: lower_type(env, element, span)?,
        },
        HMIRTypeKind::Undefined(HMIRUndefined::Array { element }) => MIRTypeKind::Undefined(MIRUndefined::Array {
            element: lower_type(env, element, span)?,
        }),
        HMIRTypeKind::Builtin { name, size, alignment } => MIRTypeKind::Builtin {
            name,
            layout: MIRTypeLayout::new(size, alignment),
        },
        HMIRTypeKind::Function(signature) => MIRTypeKind::Function {
            signature: lower_signature(env, &signature, span)?,
        },
    };

    let id = env.module_mut().intern_type(MIRType::new(kind));
    env.resolver_mut().cache_lowered(ty, id);
    Ok(id)
}

// Named types keep their identity, so two with the same shape are never interned together
fn lower_named(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    lowered: MIRType,
    debug_name: Option<String>,
) -> MIRTypeID {
    let id = env.module_mut().add_type(lowered, debug_name);
    env.resolver_mut().cache_lowered(ty, id);
    id
}

fn lower_aggregate(
    env: &mut HMIREnvironment,
    ty: HMIRTypeID,
    fields: &[HMIRFieldDef<HMIRTypeID>],
    tag: MIRUndefined,
    kind: impl FnOnce(Vec<MIRField>) -> MIRTypeKind,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    // An aggregate may refer to itself, so it is undefined while its fields lower
    let undefined = MIRType::new(MIRTypeKind::Undefined(tag));
    let debug_name = env.resolve_type_id(ty).debug_name().map(str::to_string);
    let id = lower_named(env, ty, undefined, debug_name);

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

    env.module_mut().define_type(id, MIRType::new(kind(fields)));
    for (metadata, qualified) in env.module().qualifications(id).to_vec() {
        let ty = qualified_copy(env, id, metadata);
        env.module_mut().define_type(qualified, ty);
    }
    Ok(id)
}

fn mir_tag(tag: HIRTagKind) -> MIRUndefined {
    match tag {
        HIRTagKind::Struct => MIRUndefined::Struct,
        HIRTagKind::Union => MIRUndefined::Union,
        HIRTagKind::Enum => MIRUndefined::Enum,
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
