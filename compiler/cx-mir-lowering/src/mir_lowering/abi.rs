use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRABISlot, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI, LMIRReturnABI};

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
) -> LMIRFunctionSignature {
    let return_abi = classify_return(return_type.clone(), mode);
    let params = params
        .into_iter()
        .map(|param| classify_param(param, mode))
        .collect();

    LMIRFunctionSignature {
        return_type,
        return_abi,
        params,
        var_args,
    }
}

fn classify_return(return_type: LMIRType, mode: LMIRABIMode) -> LMIRReturnABI {
    if return_type.is_void() {
        return LMIRReturnABI::Void;
    }

    if !return_type.is_memory_resident() {
        return LMIRReturnABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: return_type,
            }],
        };
    }

    if mode == LMIRABIMode::C {
        if let Some(slots) = direct_aggregate_slots(&return_type) {
            return LMIRReturnABI::Direct { slots };
        }
    }

    LMIRReturnABI::IndirectSret {
        alignment: return_type.alignment(),
        returns_pointer: mode == LMIRABIMode::Internal,
    }
}

fn classify_param(mut param: LMIRParameter, mode: LMIRABIMode) -> LMIRParameter {
    param.abi = if !param._type.is_memory_resident() {
        LMIRParameterABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: param._type.clone(),
            }],
        }
    } else if mode == LMIRABIMode::C {
        if let Some(slots) = direct_aggregate_slots(&param._type) {
            LMIRParameterABI::Direct { slots }
        } else {
            LMIRParameterABI::Indirect {
                alignment: param._type.alignment(),
                byval: true,
            }
        }
    } else if param._type.is_structure() {
        LMIRParameterABI::Indirect {
            alignment: param._type.alignment(),
            byval: false,
        }
    } else {
        LMIRParameterABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: param._type.clone(),
            }],
        }
    };

    param
}

fn integer_slot_type(size: usize) -> Option<LMIRType> {
    match size {
        1 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I8).into()),
        2 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I16).into()),
        3 | 4 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I32).into()),
        5..=8 => Some(LMIRTypeKind::Integer(LMIRIntegerType::I64).into()),
        _ => None,
    }
}

fn direct_aggregate_slots(ty: &LMIRType) -> Option<Vec<LMIRABISlot>> {
    if let Some(_type) = direct_sse_aggregate_type(ty) {
        return Some(vec![LMIRABISlot { _type, offset: 0 }]);
    }

    if let Some((fields, ftype)) = homogeneous_float_fields(ty) {
        match (fields, ftype) {
            (size @ (2 | 4), LMIRFloatType::F32) => {
                let vector: LMIRType = LMIRTypeKind::Vector {
                    element: LMIRFloatType::F32,
                    count: 2,
                }
                .into();

                return Some(if size == 2 {
                    vec![LMIRABISlot {
                        _type: vector,
                        offset: 0,
                    }]
                } else {
                    vec![
                        LMIRABISlot {
                            _type: vector.clone(),
                            offset: 0,
                        },
                        LMIRABISlot {
                            _type: vector,
                            offset: 8,
                        },
                    ]
                });
            }

            (1, _) => {
                return Some(vec![LMIRABISlot {
                    _type: LMIRTypeKind::Float(ftype).into(),
                    offset: 0,
                }])
            }

            _ => {}
        }
    }

    direct_integer_aggregate_slots(ty)
}

fn direct_integer_aggregate_slots(ty: &LMIRType) -> Option<Vec<LMIRABISlot>> {
    if !ty.is_structure() && !matches!(ty.kind, LMIRTypeKind::Opaque { .. }) {
        return None;
    }

    match ty.size() {
        0 => None,
        size @ 1..=8 => Some(vec![LMIRABISlot {
            _type: integer_slot_type(size)?,
            offset: 0,
        }]),
        size @ 9..=16 => Some(vec![
            LMIRABISlot {
                _type: LMIRTypeKind::Integer(LMIRIntegerType::I64).into(),
                offset: 0,
            },
            LMIRABISlot {
                _type: integer_slot_type(size - 8)?,
                offset: 8,
            },
        ]),
        _ => None,
    }
}

fn direct_sse_aggregate_type(ty: &LMIRType) -> Option<LMIRType> {
    let (len, ftype) = homogeneous_float_fields(ty)?;

    match (len, ftype) {
        (1, _) => Some(LMIRTypeKind::Float(ftype).into()),
        (2, LMIRFloatType::F32) => Some(
            LMIRTypeKind::Vector {
                element: ftype,
                count: 2,
            }
            .into(),
        ),
        _ => None,
    }
}

fn homogeneous_float_fields(ty: &LMIRType) -> Option<(usize, LMIRFloatType)> {
    let LMIRTypeKind::Struct { fields, .. } = &ty.kind else {
        return None;
    };

    let LMIRTypeKind::Float(_fty) = &fields.get(0)?.1.kind else {
        return None;
    };

    for (_, _ty) in fields.iter().skip(1) {
        let LMIRTypeKind::Float(_fty2) = &_ty.kind else {
            return None;
        };

        if *_fty2 != *_fty {
            return None;
        }
    }

    Some((fields.len(), *_fty))
}
