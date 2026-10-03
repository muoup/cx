use cx_hmir::HMIRAggregateKind;
use cx_log::{CXResult, catalogue::typecheck};
use cx_mir::{MIRField, MIRFnParam, MIRFnSignature, MIRType, MIRTypeID, MIRTypeKind};
use cx_tokens::TokenRange;
use cx_util::identifier::CXIdent;

use crate::{
    staging_error,
    ty::{FunctionType, NominalID, TypeID, TypeKind, TypeTable},
};

pub(super) fn lower_type(
    types: &mut TypeTable,
    ty: TypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    let ty = types.unqualified(ty);
    if let Some(id) = types.lowered.get(&ty) {
        return Ok(*id);
    }
    let kind = match types.kind(ty).clone() {
        TypeKind::Void | TypeKind::Unreachable => MIRTypeKind::Void,
        TypeKind::Type | TypeKind::Expr { .. } => {
            return Err(staging_error(
                span,
                &typecheck::COMPTIME_ONLY_TYPE,
                types.display(ty),
            ));
        }
        TypeKind::Str => MIRTypeKind::Str,
        TypeKind::Int { width, .. } => MIRTypeKind::Integer {
            ty: TypeTable::mir_int(width),
        },
        TypeKind::Float { width } => MIRTypeKind::Float {
            ty: TypeTable::mir_float(width),
        },
        TypeKind::Pointer(inner) => MIRTypeKind::PointerTo {
            inner: lower_type(types, inner, span)?,
        },
        TypeKind::Reference(inner) => MIRTypeKind::MemoryReference {
            inner: lower_type(types, inner, span)?,
        },
        TypeKind::Array { element, length } => {
            let inner = lower_type(types, element, span)?;
            match length {
                Some(length) => MIRTypeKind::Array {
                    length: length as usize,
                    inner,
                },
                None => MIRTypeKind::IncompleteArray { inner },
            }
        }
        TypeKind::Function(function) => MIRTypeKind::Function {
            signature: lower_signature(types, &function, span)?,
        },
        TypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },
        TypeKind::Nominal(nominal) => return lower_nominal_type(types, ty, nominal, span),
        TypeKind::Const(_) => unreachable!("'kind' looks through qualifiers"),
    };
    let id = types.mir.intern(MIRType::new(kind));
    types.lowered.insert(ty, id);
    Ok(id)
}

fn lower_signature(
    types: &mut TypeTable,
    function: &FunctionType,
    span: &TokenRange,
) -> CXResult<MIRFnSignature> {
    let params = function
        .params()
        .iter()
        .map(|param| {
            let ty = lower_type(types, *param, span)?;
            Ok(MIRFnParam::new(None, ty, false))
        })
        .collect::<CXResult<Vec<_>>>()?;
    let ret = lower_type(types, function.ret(), span)?;
    Ok(MIRFnSignature::new(
        params,
        ret,
        function.is_variadic(),
        false,
    ))
}

fn lower_nominal_type(
    types: &mut TypeTable,
    ty: TypeID,
    nominal: NominalID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    let nominal = types.nominal(nominal).clone();
    let Some(fields) = nominal.fields else {
        let id = types.mir.intern(MIRType::new(MIRTypeKind::Opaque {
            size: 0,
            alignment: 1,
        }));
        types.lowered.insert(ty, id);
        return Ok(id);
    };

    let id = types.mir.reserve();
    types.lowered.insert(ty, id);
    let fields = fields
        .iter()
        .map(|field| {
            let name = field.name().map(CXIdent::as_string);
            let ty = lower_type(types, field.ty(), span)?;
            Ok(match field.bit_width() {
                Some(width) => MIRField::Bitfield {
                    name,
                    integer_type_id: ty,
                    width,
                },
                None => MIRField::Standard { name, type_id: ty },
            })
        })
        .collect::<CXResult<Vec<_>>>()?;
    let kind = match nominal.kind {
        HMIRAggregateKind::Struct => MIRTypeKind::Structured { fields },
        HMIRAggregateKind::Union => MIRTypeKind::Union { variants: fields },
        HMIRAggregateKind::TaggedUnion => MIRTypeKind::TaggedUnion { variants: fields },
    };
    types.mir.define(id, MIRType::new(kind));
    types.mir.set_debug_name(id, nominal.name);
    Ok(id)
}
