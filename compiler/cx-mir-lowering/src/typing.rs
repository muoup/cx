use cx_ast::ast::modifiers::CXLinkageMode;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{
    LMIRABISlot, LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameter, LMIRParameterABI,
    LMIRReturnABI, LinkageType,
};
use cx_mir::{MIRFnPrototype, MIRFnSignature, MIRType};
use cx_target::ArchitectureConfig;
use cx_thir::registry::THIRDecomposedRegistry;
use cx_thir::thir::r#type::{THIRFloatType, THIRIntType, THIRType, THIRTypeKind};
use cx_thir::type_context::THIRTypeContext;

pub(crate) fn convert_prototype(
    prototype: &MIRFnPrototype,
    registry: &THIRDecomposedRegistry,
) -> LMIRFunctionPrototype {
    LMIRFunctionPrototype {
        name: prototype.signature.name.clone(),
        linkage: convert_linkage(prototype.linkage),
        signature: classify_signature(&prototype.signature, registry),
    }
}

pub(crate) fn classify_signature(
    signature: &MIRFnSignature,
    registry: &THIRDecomposedRegistry,
) -> LMIRFunctionSignature {
    let return_type = signature
        .return_type
        .as_ref()
        .map(|ty| convert_type(ty.as_thir(), registry))
        .unwrap_or_else(LMIRType::unit);
    let return_layout = signature.return_type.as_ref().map(|ty| {
        registry.type_layout(ty.as_thir()).unwrap_or_else(|err| {
            panic!("Failed to calculate return type layout: {}", err.message())
        })
    });
    let return_abi = match return_layout {
        Some(layout) => classify_return(
            registry.architecture(),
            return_type.clone(),
            layout.alignment as u8,
            layout.size,
        ),
        None => LMIRReturnABI::Void,
    };
    let params = signature
        .params
        .iter()
        .map(|param| {
            classify_param(
                registry.architecture(),
                param.name.clone(),
                &param.ty,
                registry,
            )
        })
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
    ty: &MIRType,
    registry: &THIRDecomposedRegistry,
) -> LMIRParameter {
    let lowered = convert_type(ty.as_thir(), registry);
    let layout = registry
        .type_layout(ty.as_thir())
        .unwrap_or_else(|err| panic!("Failed to calculate parameter layout: {}", err.message()));
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

pub(crate) fn convert_integer_type(ty: THIRIntType) -> LMIRIntegerType {
    match ty {
        THIRIntType::I1 => LMIRIntegerType::I1,
        THIRIntType::I8 => LMIRIntegerType::I8,
        THIRIntType::I16 => LMIRIntegerType::I16,
        THIRIntType::I32 => LMIRIntegerType::I32,
        THIRIntType::I64 => LMIRIntegerType::I64,
        THIRIntType::I128 => LMIRIntegerType::I128,
    }
}

pub(crate) fn convert_float_type(ty: THIRFloatType) -> LMIRFloatType {
    match ty {
        THIRFloatType::F32 => LMIRFloatType::F32,
        THIRFloatType::F64 => LMIRFloatType::F64,
    }
}

pub(crate) fn convert_type(ty: &THIRType, registry: &THIRDecomposedRegistry) -> LMIRType {
    let layout = match ty.kind {
        THIRTypeKind::Function { .. } => cx_thir::layout::THIRTypeLayout {
            size: registry.architecture().pointer_size(),
            alignment: registry.architecture().pointer_alignment(),
        },
        THIRTypeKind::Str => cx_thir::layout::THIRTypeLayout {
            size: 1,
            alignment: 1,
        },
        _ => registry
            .type_layout(ty)
            .unwrap_or_else(|err| panic!("Failed to calculate type layout: {}", err.message())),
    };
    let kind = match &ty.kind {
        THIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },
        THIRTypeKind::Integer { _type, .. } => LMIRTypeKind::Integer(convert_integer_type(*_type)),
        THIRTypeKind::Float { _type } => LMIRTypeKind::Float(convert_float_type(*_type)),
        THIRTypeKind::Function { .. }
        | THIRTypeKind::PointerTo { .. }
        | THIRTypeKind::MemoryReference { .. } => LMIRTypeKind::Pointer {
            nullable: matches!(ty.kind, THIRTypeKind::Function { .. }),
            dereferenceable: 0,
            bytes: registry.architecture().pointer_size() as u8,
        },
        THIRTypeKind::TaggedUnion { variants } => LMIRTypeKind::Struct {
            name: ty.strong_identifier().unwrap_or_default().to_owned(),
            fields: vec![
                (
                    "data".into(),
                    lower_union(
                        variants
                            .iter()
                            .map(|field| registry.resolve_type_id(field.ty())),
                        registry,
                    ),
                ),
                (
                    "tag".into(),
                    LMIRType::with_implicit_abi(
                        registry.architecture(),
                        LMIRTypeKind::Integer(LMIRIntegerType::I8),
                    ),
                ),
            ],
        },
        THIRTypeKind::Array { inner_type, length } => LMIRTypeKind::Array {
            element: Box::new(convert_type(
                registry.resolve_type_id(*inner_type),
                registry,
            )),
            size: *length,
        },
        THIRTypeKind::Structured { .. } => LMIRTypeKind::Struct {
            name: ty.strong_identifier().unwrap_or_default().to_owned(),
            fields: ty
                .aggregate_fields(registry)
                .expect("structured type has invalid fields")
                .into_iter()
                .map(|(name, field)| (name, convert_type(&field, registry)))
                .collect(),
        },
        THIRTypeKind::Union { .. } => LMIRTypeKind::Opaque { bytes: layout.size },
        THIRTypeKind::Unit => LMIRTypeKind::Unit,
        THIRTypeKind::Str => LMIRTypeKind::Integer(LMIRIntegerType::I8),
        THIRTypeKind::Undefined => panic!("Cannot lower undefined type"),
    };
    LMIRType {
        kind,
        alignment: layout.alignment as u8,
    }
}

fn lower_union<'a>(
    variants: impl Iterator<Item = &'a THIRType>,
    registry: &THIRDecomposedRegistry,
) -> LMIRType {
    let (size, alignment) = variants
        .map(|variant| {
            registry
                .type_layout(variant)
                .unwrap_or_else(|err| panic!("invalid union member: {}", err.message()))
        })
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
