use cx_hmir::ty::{HMIRTypeID, HMIRTypeKind};
use cx_log::CXResult;
use cx_mir::{MIRTypeID, MIRTypeKind};
use cx_tokens::TokenRange;

use crate::{env::lowering::FnLoweringContext, log::hmir_error};

pub(super) fn lower_type(
    env: &mut FnLoweringContext,
    ty: HMIRTypeID,
    span: &TokenRange,
) -> CXResult<MIRTypeID> {
    let ty = types.unqualified(ty);

    if let Some(id) = types.lowered.get(&ty) {
        return Ok(*id);
    }

    let kind = match types.kind(ty).clone() {
        HMIRTypeKind::Void | HMIRTypeKind::Unreachable => MIRTypeKind::Void,
        HMIRTypeKind::Type | HMIRTypeKind::StagedExpr { .. } => {
            return Err(hmir_error(
                span,
                &typecheck::COMPTIME_ONLY_TYPE,
                types.display(ty),
            ));
        }
        HMIRTypeKind::Str => MIRTypeKind::Str,
        HMIRTypeKind::Int { width, .. } => MIRTypeKind::Integer {
            ty: TypeTable::mir_int(width),
        },
        HMIRTypeKind::Float { width } => MIRTypeKind::Float {
            ty: TypeTable::mir_float(width),
        },
        HMIRTypeKind::PointerTo(inner) => MIRTypeKind::PointerTo {
            inner: lower_type(types, inner, span)?,
        },
        HMIRTypeKind::ReferenceTo(inner) => MIRTypeKind::MemoryReference {
            inner: lower_type(types, inner, span)?,
        },
        HMIRTypeKind::Array { element, length } => {
            let inner = lower_type(types, element, span)?;
            match length {
                Some(length) => MIRTypeKind::Array {
                    length: length as usize,
                    inner,
                },
                None => MIRTypeKind::IncompleteArray { inner },
            }
        }
        HMIRTypeKind::Function(function) => MIRTypeKind::Function {
            signature: lower_signature(types, &function, span)?,
        },
        HMIRTypeKind::Opaque { size, alignment } => MIRTypeKind::Opaque { size, alignment },

        _ => todo!(),
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
