use super::types::convert_type;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRABISlot, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI, LMIRReturnABI};
use cx_mir::mir::data::{MIRParameter, MIRType};
use cx_mir::registry::MIRDecomposedRegistry;
use cx_mir::type_context::MIRTypeContext;
use cx_target::ArchitectureConfig;

pub(crate) fn classify_signature(
    mir_return_type: &MIRType,
    params: &[MIRParameter],
    var_args: bool,
    definitions: &MIRDecomposedRegistry,
) -> LMIRFunctionSignature {
    let return_type = convert_type(mir_return_type, definitions);
    let return_layout = definitions
        .type_layout(mir_return_type)
        .unwrap_or_else(|err| panic!("Failed to calculate return type layout: {}", err.message()));
    let return_abi = classify_return(
        definitions.architecture(),
        return_type.clone(),
        return_layout.alignment as u8,
        return_layout.size,
    );
    let params = params
        .iter()
        .map(|param| classify_param(definitions.architecture(), param, definitions))
        .collect();

    LMIRFunctionSignature {
        return_type,
        return_abi,
        params,
        var_args,
    }
}

fn classify_return(
    architecture: &ArchitectureConfig,
    return_type: LMIRType,
    alignment: u8,
    size: usize,
) -> LMIRReturnABI {
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

    if let Some(slots) = direct_aggregate_slots(architecture, &return_type, size) {
        return LMIRReturnABI::Direct { slots };
    }

    LMIRReturnABI::IndirectSret { alignment }
}

fn classify_param(
    architecture: &ArchitectureConfig,
    param: &MIRParameter,
    definitions: &MIRDecomposedRegistry,
) -> LMIRParameter {
    let _type = convert_type(&param._type, definitions);
    let layout = definitions
        .type_layout(&param._type)
        .unwrap_or_else(|err| panic!("Failed to calculate parameter layout: {}", err.message()));
    let abi = if !_type.is_memory_resident() {
        LMIRParameterABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: _type.clone(),
            }],
        }
    } else if let Some(slots) = direct_aggregate_slots(architecture, &_type, layout.size) {
        LMIRParameterABI::Direct { slots }
    } else {
        LMIRParameterABI::Indirect {
            alignment: layout.alignment as u8,
        }
    };

    LMIRParameter {
        name: param.name.clone(),
        _type,
        abi,
    }
}

fn integer_slot_type(architecture: &ArchitectureConfig, size: usize) -> Option<LMIRType> {
    Some(LMIRType::with_implicit_abi(
        architecture,
        match size {
            1 => LMIRTypeKind::Integer(LMIRIntegerType::I8),
            2 => LMIRTypeKind::Integer(LMIRIntegerType::I16),
            3 | 4 => LMIRTypeKind::Integer(LMIRIntegerType::I32),
            5..=8 => LMIRTypeKind::Integer(LMIRIntegerType::I64),

            _ => return None,
        },
    ))
}

fn direct_aggregate_slots(
    architecture: &ArchitectureConfig,
    ty: &LMIRType,
    size: usize,
) -> Option<Vec<LMIRABISlot>> {
    if let Some(_type) = direct_sse_aggregate_type(architecture, ty) {
        return Some(vec![LMIRABISlot { _type, offset: 0 }]);
    }

    if let Some((fields, ftype)) = homogeneous_float_fields(ty) {
        match (fields, ftype) {
            (size @ (2 | 4), LMIRFloatType::F32) => {
                let vector: LMIRType = LMIRType::with_implicit_abi(
                    architecture,
                    LMIRTypeKind::Vector {
                        element: LMIRFloatType::F32,
                        count: 2,
                    },
                );

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
                    _type: LMIRType::with_implicit_abi(architecture, LMIRTypeKind::Float(ftype)),
                    offset: 0,
                }])
            }

            _ => {}
        }
    }

    direct_integer_aggregate_slots(architecture, ty, size)
}

fn direct_integer_aggregate_slots(
    architecture: &ArchitectureConfig,
    ty: &LMIRType,
    size: usize,
) -> Option<Vec<LMIRABISlot>> {
    if !ty.is_structure() && !matches!(ty.kind, LMIRTypeKind::Opaque { .. }) {
        return None;
    }

    match size {
        0 => None,
        size @ 1..=8 => Some(vec![LMIRABISlot {
            _type: integer_slot_type(architecture, size)?,
            offset: 0,
        }]),
        size @ 9..=16 => Some(vec![
            LMIRABISlot {
                _type: LMIRType::with_implicit_abi(
                    architecture,
                    LMIRTypeKind::Integer(LMIRIntegerType::I64),
                ),
                offset: 0,
            },
            LMIRABISlot {
                _type: integer_slot_type(architecture, size - 8)?,
                offset: 8,
            },
        ]),
        _ => None,
    }
}

fn direct_sse_aggregate_type(architecture: &ArchitectureConfig, ty: &LMIRType) -> Option<LMIRType> {
    let (len, ftype) = homogeneous_float_fields(ty)?;

    match (len, ftype) {
        (1, _) => Some(LMIRType::with_implicit_abi(
            architecture,
            LMIRTypeKind::Float(ftype),
        )),
        (2, LMIRFloatType::F32) => Some(LMIRType::with_implicit_abi(
            architecture,
            LMIRTypeKind::Vector {
                element: ftype,
                count: 2,
            },
        )),
        _ => None,
    }
}

fn homogeneous_float_fields(ty: &LMIRType) -> Option<(usize, LMIRFloatType)> {
    let LMIRTypeKind::Struct { fields, .. } = &ty.kind else {
        return None;
    };

    let LMIRTypeKind::Float(_fty) = &fields.first()?.1.kind else {
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
