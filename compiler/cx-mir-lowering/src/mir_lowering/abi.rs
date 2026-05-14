use cx_lmir::types::{LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRABIArgKind, LMIRABIParameter, LMIRABIReturnKind, LMIRABISignature, LMIRABISlot,
    LMIRParameter,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum LMIRABIMode {
    Internal,
    C,
}

pub(crate) fn classify_signature(
    return_type: LMIRType,
    params: Vec<LMIRParameter>,
    var_args: bool,
    mode: LMIRABIMode,
) -> LMIRABISignature {
    let return_kind = classify_return(return_type, mode);
    let params = params
        .into_iter()
        .map(|param| classify_param(param, mode))
        .collect();

    LMIRABISignature {
        return_kind,
        params,
        var_args,
    }
}

fn classify_return(return_type: LMIRType, mode: LMIRABIMode) -> LMIRABIReturnKind {
    if return_type.is_void() {
        return LMIRABIReturnKind::Void;
    }

    if !return_type.is_memory_resident() {
        return LMIRABIReturnKind::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: return_type,
            }],
        };
    }

    if mode == LMIRABIMode::C {
        if let Some(slots) = direct_aggregate_slots(&return_type) {
            return LMIRABIReturnKind::Direct { slots };
        }
    }

    LMIRABIReturnKind::IndirectSret {
        alignment: return_type.alignment(),
        returns_pointer: mode == LMIRABIMode::Internal,
        pointee: return_type,
    }
}

fn classify_param(param: LMIRParameter, mode: LMIRABIMode) -> LMIRABIParameter {
    let kind = if !param._type.is_memory_resident() {
        LMIRABIArgKind::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: param._type.clone(),
            }],
        }
    } else if mode == LMIRABIMode::C {
        if let Some(slots) = direct_aggregate_slots(&param._type) {
            LMIRABIArgKind::Direct { slots }
        } else {
            LMIRABIArgKind::Indirect {
                alignment: param._type.alignment(),
                pointee: param._type.clone(),
                byval: true,
            }
        }
    } else if param._type.is_structure() {
        LMIRABIArgKind::Indirect {
            alignment: param._type.alignment(),
            pointee: param._type.clone(),
            byval: false,
        }
    } else {
        LMIRABIArgKind::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: param._type.clone(),
            }],
        }
    };

    LMIRABIParameter {
        name: param.name,
        semantic_type: param._type,
        kind,
    }
}

pub(crate) fn direct_integer_aggregate_type(ty: &LMIRType) -> Option<LMIRType> {
    if !ty.is_structure() && !matches!(ty.kind, LMIRTypeKind::Union { .. }) {
        return None;
    }

    match ty.size() {
        1 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I8).into()),
        2 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I16).into()),
        4 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I32).into()),
        8 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I64).into()),
        _ => None,
    }
}

fn direct_aggregate_slots(ty: &LMIRType) -> Option<Vec<LMIRABISlot>> {
    if let Some(_type) = direct_sse_aggregate_type(ty) {
        return Some(vec![LMIRABISlot { _type, offset: 0 }]);
    }

    if let Some(fields) = homogeneous_float_fields(ty) {
        if fields.len() == 4 && fields.iter().all(|field| field.size() == 4) {
            let lane = LMIRTypeKind::Float(cx_lmir::types::LMIRFloatType::F32).into();
            let vector: LMIRType = LMIRTypeKind::Vector {
                element: Box::new(lane),
                count: 2,
            }
            .into();
            return Some(vec![
                LMIRABISlot {
                    _type: vector.clone(),
                    offset: 0,
                },
                LMIRABISlot {
                    _type: vector,
                    offset: 8,
                },
            ]);
        }
    }

    direct_integer_aggregate_type(ty).map(|_type| vec![LMIRABISlot { _type, offset: 0 }])
}

fn direct_sse_aggregate_type(ty: &LMIRType) -> Option<LMIRType> {
    let fields = homogeneous_float_fields(ty)?;
    let first = fields.first()?;

    if fields.len() == 1 {
        return Some(first.clone());
    }

    if fields.len() == 2 && fields.iter().all(|field| field.size() == 4) {
        return Some(
            LMIRTypeKind::Vector {
                element: Box::new(first.clone()),
                count: 2,
            }
            .into(),
        );
    }

    None
}

fn homogeneous_float_fields(ty: &LMIRType) -> Option<Vec<LMIRType>> {
    let LMIRTypeKind::Struct { fields, .. } = &ty.kind else {
        return None;
    };

    let fields = fields
        .iter()
        .map(|(_, field)| field.clone())
        .collect::<Vec<_>>();

    if fields.is_empty()
        || fields
            .iter()
            .any(|field| !matches!(field.kind, LMIRTypeKind::Float(_)))
    {
        return None;
    }

    if fields.iter().map(|field| field.size()).sum::<usize>() != ty.size() {
        return None;
    }

    Some(fields)
}
