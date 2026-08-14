use cx_ast::ast::modifiers::CXLinkageMode;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRABISlot, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI,
    LMIRReturnABI, LinkageType,
};
use cx_mir::{
    MIRField, MIRFloatType, MIRFnPrototype, MIRFnSignature, MIRIntType, MIRTypeID, MIRTypeKind,
    MIRTypeRegistry,
};
use cx_target::ArchitectureConfig;

pub(crate) fn convert_prototype(
    prototype: &MIRFnPrototype,
    types: &MIRTypeRegistry,
) -> LMIRFunctionPrototype {
    LMIRFunctionPrototype {
        name: prototype.signature.symbol_name.clone(),
        linkage: convert_linkage(prototype.linkage),
        signature: classify_signature(&prototype.signature, types),
    }
}

pub(crate) fn classify_signature(
    signature: &MIRFnSignature,
    types: &MIRTypeRegistry,
) -> LMIRFunctionSignature {
    let return_type = signature
        .return_type
        .map(|ty| convert_type(ty, types))
        .unwrap_or_else(LMIRType::unit);
    let return_layout = signature.return_type.map(|ty| layout(types, ty));
    let return_abi = match return_layout {
        Some(layout) => classify_return(
            types.architecture(),
            return_type.clone(),
            layout.alignment as u8,
            layout.size,
        ),
        None => LMIRReturnABI::Void,
    };
    let params = signature
        .params
        .iter()
        .map(|param| classify_param(types.architecture(), param.name.clone(), param.ty, types))
        .collect();

    LMIRFunctionSignature {
        return_type,
        return_abi,
        params,
        var_args: signature.variadic,
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
    name: Option<cx_util::identifier::CXIdent>,
    ty: MIRTypeID,
    types: &MIRTypeRegistry,
) -> LMIRParameter {
    let lowered = convert_type(ty, types);
    let layout = layout(types, ty);
    let abi = if !lowered.is_memory_resident() {
        LMIRParameterABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                _type: lowered.clone(),
            }],
        }
    } else if let Some(slots) = direct_aggregate_slots(architecture, &lowered, layout.size) {
        LMIRParameterABI::Direct { slots }
    } else {
        LMIRParameterABI::Indirect {
            alignment: layout.alignment as u8,
        }
    };
    LMIRParameter {
        name,
        _type: lowered,
        abi,
    }
}

pub(crate) fn convert_linkage(linkage: CXLinkageMode) -> LinkageType {
    match linkage {
        CXLinkageMode::Standard => LinkageType::Standard,
        CXLinkageMode::Extern => LinkageType::External,
        CXLinkageMode::Static => LinkageType::Static,
    }
}

pub(crate) fn convert_integer_type(ty: MIRIntType) -> LMIRIntegerType {
    match ty {
        MIRIntType::I1 => LMIRIntegerType::I1,
        MIRIntType::I8 => LMIRIntegerType::I8,
        MIRIntType::I16 => LMIRIntegerType::I16,
        MIRIntType::I32 => LMIRIntegerType::I32,
        MIRIntType::I64 => LMIRIntegerType::I64,
        MIRIntType::I128 => LMIRIntegerType::I128,
    }
}

pub(crate) fn convert_float_type(ty: MIRFloatType) -> LMIRFloatType {
    match ty {
        MIRFloatType::F32 => LMIRFloatType::F32,
        MIRFloatType::F64 => LMIRFloatType::F64,
    }
}

pub(crate) fn convert_type(ty: MIRTypeID, types: &MIRTypeRegistry) -> LMIRType {
    let definition = types
        .definition(ty)
        .unwrap_or_else(|| panic!("invalid MIR type {ty}"));
    let layout = layout(types, ty);
    let kind = match &definition.kind {
        MIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },
        MIRTypeKind::Integer { ty, .. } => LMIRTypeKind::Integer(convert_integer_type(*ty)),
        MIRTypeKind::Float { ty } => LMIRTypeKind::Float(convert_float_type(*ty)),
        MIRTypeKind::Function { .. }
        | MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. } => LMIRTypeKind::Pointer {
            nullable: matches!(definition.kind, MIRTypeKind::Function { .. }),
            dereferenceable: 0,
            bytes: types.architecture().pointer_size() as u8,
        },
        MIRTypeKind::TaggedUnion { variants } => LMIRTypeKind::Struct {
            name: format!("mir_type_{}", ty.index()),
            fields: vec![
                ("data".into(), lower_union(variants, types)),
                (
                    "tag".into(),
                    LMIRType::with_implicit_abi(
                        types.architecture(),
                        LMIRTypeKind::Integer(LMIRIntegerType::I8),
                    ),
                ),
            ],
        },
        MIRTypeKind::Array { inner, length } => LMIRTypeKind::Array {
            element: Box::new(convert_type(*inner, types)),
            size: *length,
        },
        MIRTypeKind::Structured { fields } => LMIRTypeKind::Struct {
            name: format!("mir_type_{}", ty.index()),
            fields: fields
                .iter()
                .enumerate()
                .map(|(index, field)| {
                    (
                        field
                            .name()
                            .map(str::to_owned)
                            .unwrap_or_else(|| format!("field_{index}")),
                        convert_type(field.ty(), types),
                    )
                })
                .collect(),
        },
        MIRTypeKind::Union { .. } => LMIRTypeKind::Opaque { bytes: layout.size },
        MIRTypeKind::Void => LMIRTypeKind::Void,
        MIRTypeKind::Str => LMIRTypeKind::Integer(LMIRIntegerType::I8),
        MIRTypeKind::Undefined => panic!("cannot lower undefined MIR type {ty}"),
    };
    LMIRType {
        kind,
        alignment: layout.alignment as u8,
    }
}

fn layout(types: &MIRTypeRegistry, ty: MIRTypeID) -> cx_mir::MIRTypeLayout {
    types
        .layout(ty)
        .unwrap_or_else(|err| panic!("failed to calculate MIR layout for {ty}: {err}"))
}

fn lower_union(variants: &[MIRField], types: &MIRTypeRegistry) -> LMIRType {
    let (size, alignment) = variants
        .iter()
        .map(|variant| layout(types, variant.ty()))
        .fold((0, 1), |(size, alignment), layout| {
            (size.max(layout.size), alignment.max(layout.alignment))
        });
    LMIRType::new(LMIRTypeKind::Opaque { bytes: size }, alignment as u8)
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
    if let Some(slot) = direct_sse_aggregate_type(architecture, ty) {
        return Some(vec![LMIRABISlot {
            _type: slot,
            offset: 0,
        }]);
    }
    if let Some((fields, float)) = homogeneous_float_fields(ty) {
        match (fields, float) {
            (size @ (2 | 4), LMIRFloatType::F32) => {
                let vector = LMIRType::with_implicit_abi(
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
                    _type: LMIRType::with_implicit_abi(architecture, LMIRTypeKind::Float(float)),
                    offset: 0,
                }]);
            }
            _ => {}
        }
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
    let (length, float) = homogeneous_float_fields(ty)?;
    match (length, float) {
        (1, _) => Some(LMIRType::with_implicit_abi(
            architecture,
            LMIRTypeKind::Float(float),
        )),
        (2, LMIRFloatType::F32) => Some(LMIRType::with_implicit_abi(
            architecture,
            LMIRTypeKind::Vector {
                element: float,
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
    let LMIRTypeKind::Float(first) = fields.first()?.1.kind else {
        return None;
    };
    fields
        .iter()
        .all(|(_, field)| matches!(field.kind, LMIRTypeKind::Float(value) if value == first))
        .then_some((fields.len(), first))
}
