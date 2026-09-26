use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRABISlot, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI,
    LMIRReturnABI, LinkageType,
};
use cx_mir::ty::interface::MTRegistry;
use cx_mir::ty::layout::{calculate_field_layouts, calculate_type_layout};
use cx_mir::ty::registry::MIRTypeRegistry;
use cx_mir::{
    MIRField, MIRFieldLayout, MIRFloatType, MIRFnPrototype, MIRFnSignature, MIRIntType, MIRTypeID,
    MIRTypeKind,
};
use cx_target::ArchitectureConfig;
use cx_util::identifier::CXIdent;
use cx_util::linkage::LinkageMode;

pub(crate) fn convert_prototype(
    prototype: &MIRFnPrototype,
    types: &MIRTypeRegistry,
) -> LMIRFunctionPrototype {
    LMIRFunctionPrototype {
        name: prototype.symbol_name.clone(),
        linkage: convert_linkage(prototype.linkage),
        signature: classify_signature(&prototype.signature, types),
    }
}

pub(crate) fn classify_signature(
    signature: &MIRFnSignature,
    types: &MIRTypeRegistry,
) -> LMIRFunctionSignature {
    let return_type = convert_type(signature.return_type(), types);
    let return_layout =
        (!return_type.is_void()).then(|| calculate_type_layout(types, signature.return_type()));
    let return_abi = match return_layout {
        Some(layout) => classify_return(
            types.architecture(),
            return_type.clone(),
            layout.alignment() as u8,
            layout.size(),
        ),
        None => LMIRReturnABI::Void,
    };
    let params = signature
        .params()
        .iter()
        .map(|param| {
            classify_param(
                types.architecture(),
                param.name().cloned(),
                param.ty(),
                types,
            )
        })
        .collect();

    LMIRFunctionSignature {
        return_type,
        return_abi,
        params,
        var_args: signature.variadic(),
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
                ty: return_type,
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
    name: Option<CXIdent>,
    ty: MIRTypeID,
    types: &MIRTypeRegistry,
) -> LMIRParameter {
    let lowered = convert_type(ty, types);
    let aggregate_value = matches!(
        types.definition(ty).unwrap().kind(),
        MIRTypeKind::Structured { .. } | MIRTypeKind::Union { .. }
    );
    let abi = if lowered.is_void() {
        LMIRParameterABI::Direct { slots: Vec::new() }
    } else if !lowered.is_memory_resident() {
        LMIRParameterABI::Direct {
            slots: vec![LMIRABISlot {
                offset: 0,
                ty: lowered.clone(),
            }],
        }
    } else {
        let layout = calculate_type_layout(types, ty);

        if let Some(slots) = direct_aggregate_slots(architecture, &lowered, layout.size()) {
            LMIRParameterABI::Direct { slots }
        } else if aggregate_value {
            LMIRParameterABI::ByValue {
                alignment: layout.alignment() as u8,
            }
        } else {
            LMIRParameterABI::Indirect {
                alignment: layout.alignment() as u8,
            }
        }
    };
    LMIRParameter {
        name,
        ty: lowered,
        abi,
    }
}

pub(crate) fn convert_linkage(linkage: LinkageMode) -> LinkageType {
    match linkage {
        LinkageMode::Standard => LinkageType::Standard,
        LinkageMode::Extern => LinkageType::External,
        LinkageMode::Static => LinkageType::Static,
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

    if let MIRTypeKind::IncompleteArray { inner } = definition.kind() {
        let element = convert_type(*inner, types);
        return LMIRType {
            alignment: element.alignment,
            kind: LMIRTypeKind::Array {
                element: Box::new(element),
                size: 0,
            },
        };
    }

    let kind = match &definition.kind() {
        MIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },
        MIRTypeKind::Integer { ty, .. } => LMIRTypeKind::Integer(convert_integer_type(*ty)),
        MIRTypeKind::Float { ty } => LMIRTypeKind::Float(convert_float_type(*ty)),
        MIRTypeKind::Function { .. }
        | MIRTypeKind::PointerTo { .. }
        | MIRTypeKind::MemoryReference { .. } => LMIRTypeKind::Pointer {
            nullable: true,
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
        MIRTypeKind::IncompleteArray { .. } => unreachable!(),
        MIRTypeKind::Structured { fields } => LMIRTypeKind::Struct {
            name: format!("mir_type_{}", ty.index()),
            fields: struct_members(ty, fields, types).fields,
        },
        MIRTypeKind::Union { .. } => LMIRTypeKind::Opaque {
            bytes: calculate_type_layout(types, ty).size(),
        },
        MIRTypeKind::Void => LMIRTypeKind::Void,
        MIRTypeKind::Str => LMIRTypeKind::Integer(LMIRIntegerType::I8),
        MIRTypeKind::Undefined => panic!("cannot lower undefined MIR type {ty}"),
    };

    LMIRType {
        kind,
        alignment: calculate_type_layout(types, ty).alignment() as u8,
    }
}

/// The LMIR fields of a MIR struct and where each MIR field landed among them. Bitfields sharing a
/// storage unit share one LMIR field, zero-width bitfields have none, and padding fields are
/// inserted wherever the MIR layout places a field past its natural LMIR offset.
pub(crate) struct StructMembers {
    pub fields: Vec<(String, LMIRType)>,
    pub members: Vec<Option<StructMember>>,
}

#[derive(Clone, Copy)]
pub(crate) struct StructMember {
    pub field: usize,
    pub bitfield: Option<(usize, usize)>,
}

pub(crate) fn struct_members(
    ty: MIRTypeID,
    fields: &[MIRField],
    types: &MIRTypeRegistry,
) -> StructMembers {
    let layouts = calculate_field_layouts(types, ty).expect("struct has no field layout");
    let mut lowered: Vec<(String, LMIRType)> = Vec::new();
    let mut members = Vec::with_capacity(fields.len());
    let mut end = 0usize;
    // The LMIR field holding the open bitfield storage unit, with its byte offset
    let mut unit: Option<(usize, usize)> = None;

    for (index, (field, layout)) in fields.iter().zip(layouts).enumerate() {
        let bitfield = match layout {
            MIRFieldLayout::Bitfield { bit_width: 0, .. } => {
                members.push(None);
                continue;
            }
            MIRFieldLayout::Bitfield {
                bit_offset,
                bit_width,
                ..
            } => Some((bit_offset, bit_width)),
            MIRFieldLayout::Standard { .. } => None,
        };
        if let (Some(_), Some((field, offset))) = (bitfield, unit) {
            if offset == layout.offset() {
                members.push(Some(StructMember { field, bitfield }));
                continue;
            }
        }

        let lowered_type = convert_type(layout.ty(), types);
        let alignment = usize::from(lowered_type.alignment()).max(1);
        if end.next_multiple_of(alignment) != layout.offset() {
            lowered.push((
                format!("padding_{index}"),
                LMIRType::new(
                    LMIRTypeKind::Opaque {
                        bytes: layout.offset() - end,
                    },
                    1,
                ),
            ));
        }
        end = layout.offset() + usize::from(lowered_type.size());
        let name = match bitfield {
            Some(_) => format!("bitfield_{index}"),
            None => field
                .name()
                .map(str::to_owned)
                .unwrap_or_else(|| format!("field_{index}")),
        };
        unit = bitfield.map(|_| (lowered.len(), layout.offset()));
        members.push(Some(StructMember {
            field: lowered.len(),
            bitfield,
        }));
        lowered.push((name, lowered_type));
    }

    StructMembers {
        fields: lowered,
        members,
    }
}

fn lower_union(variants: &[MIRField], types: &MIRTypeRegistry) -> LMIRType {
    let (size, alignment) = variants
        .iter()
        .map(|variant| calculate_type_layout(types, variant.ty()))
        .fold((0, 1), |(size, alignment), layout| {
            (size.max(layout.size()), alignment.max(layout.alignment()))
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
            ty: slot,
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
                        ty: vector,
                        offset: 0,
                    }]
                } else {
                    vec![
                        LMIRABISlot {
                            ty: vector.clone(),
                            offset: 0,
                        },
                        LMIRABISlot {
                            ty: vector,
                            offset: 8,
                        },
                    ]
                });
            }
            (1, _) => {
                return Some(vec![LMIRABISlot {
                    ty: LMIRType::with_implicit_abi(architecture, LMIRTypeKind::Float(float)),
                    offset: 0,
                }]);
            }
            _ => {}
        }
    }
    match size {
        0 => None,
        size @ 1..=8 => Some(vec![LMIRABISlot {
            ty: integer_slot_type(architecture, size)?,
            offset: 0,
        }]),
        size @ 9..=16 => Some(vec![
            LMIRABISlot {
                ty: LMIRType::with_implicit_abi(
                    architecture,
                    LMIRTypeKind::Integer(LMIRIntegerType::I64),
                ),
                offset: 0,
            },
            LMIRABISlot {
                ty: integer_slot_type(architecture, size - 8)?,
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
