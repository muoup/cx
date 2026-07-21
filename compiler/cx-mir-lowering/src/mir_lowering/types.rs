use crate::builder::LMIRBuilder;
use crate::mir_lowering::abi::classify_signature;
use cx_ast::ast::modifiers::CXLinkageMode;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRFunctionPrototype, LinkageType};
use cx_mir::mir::data::{MIRFloatType, MIRFunctionPrototype, MIRIntegerType, MIRType, MIRTypeKind};
use cx_mir::registry::MIRDecomposedRegistry;
use cx_mir::type_context::MIRTypeContext;
use cx_util::identifier::CXIdent;

impl LMIRBuilder {
    pub(crate) fn convert_cx_type(&self, cx_type: &MIRType) -> LMIRType {
        convert_type(cx_type, &self.registry)
    }

    #[allow(dead_code)]
    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &MIRFunctionPrototype,
    ) -> LMIRFunctionPrototype {
        convert_cx_prototype(cx_proto, &self.registry)
    }

    pub(crate) fn convert_cx_parameter_type(&self, cx_type: &MIRType) -> LMIRType {
        convert_parameter_type(cx_type, &self.registry)
    }

    pub(crate) fn convert_integer_type(&self, cx_itype: &MIRIntegerType) -> LMIRIntegerType {
        convert_integer_type(cx_itype)
    }

    #[allow(dead_code)]
    pub(crate) fn convert_float_type(&self, cx_ftype: &MIRFloatType) -> LMIRFloatType {
        convert_float_type(cx_ftype)
    }

    pub fn convert_linkage(&self, linkage: CXLinkageMode) -> LinkageType {
        convert_linkage(linkage)
    }
}

fn convert_parameter_type(param_type: &MIRType, definitions: &MIRDecomposedRegistry) -> LMIRType {
    let bc_type = convert_type(param_type, definitions);

    if bc_type.is_structure() {
        LMIRType::default_pointer(definitions.architecture())
    } else {
        bc_type
    }
}

pub(crate) fn convert_cx_prototype(
    cx_proto: &MIRFunctionPrototype,
    definitions: &MIRDecomposedRegistry,
) -> LMIRFunctionPrototype {
    let signature = classify_signature(
        &cx_proto.signature().return_type,
        &cx_proto.signature().params,
        cx_proto.signature().var_args,
        definitions,
    );

    LMIRFunctionPrototype {
        name: CXIdent::from(cx_proto.name()),
        linkage: convert_linkage(cx_proto.linkage()),
        signature,
    }
}

fn convert_integer_type(itype: &MIRIntegerType) -> LMIRIntegerType {
    match itype {
        MIRIntegerType::I1 => LMIRIntegerType::I1,
        MIRIntegerType::I8 => LMIRIntegerType::I8,
        MIRIntegerType::I16 => LMIRIntegerType::I16,
        MIRIntegerType::I32 => LMIRIntegerType::I32,
        MIRIntegerType::I64 => LMIRIntegerType::I64,
        MIRIntegerType::I128 => LMIRIntegerType::I128,
    }
}

fn convert_float_type(ftype: &MIRFloatType) -> LMIRFloatType {
    match ftype {
        MIRFloatType::F32 => LMIRFloatType::F32,
        MIRFloatType::F64 => LMIRFloatType::F64,
    }
}

fn convert_linkage(linkage: CXLinkageMode) -> LinkageType {
    match linkage {
        CXLinkageMode::Standard => LinkageType::Standard,
        CXLinkageMode::Extern => LinkageType::External,
        CXLinkageMode::Static => LinkageType::Static,
    }
}

pub(crate) fn convert_type(
    cx_type: &MIRType,
    definitions: &MIRDecomposedRegistry,
) -> LMIRType {
    let layout = definitions
        .type_layout(cx_type)
        .unwrap_or_else(|err| panic!("Failed to calculate type layout: {}", err.message()));

    let kind = match &cx_type.kind {
        MIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },

        MIRTypeKind::Integer {
            signed: _,
            _type: itype,
        } => LMIRTypeKind::Integer(convert_integer_type(itype)),

        MIRTypeKind::Float { _type } => LMIRTypeKind::Float(convert_float_type(_type)),

        MIRTypeKind::Function { .. } => LMIRTypeKind::Pointer {
            nullable: true,
            dereferenceable: 0,
            bytes: definitions.architecture().pointer_size() as u8,
        },

        MIRTypeKind::PointerTo { .. } => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
            bytes: definitions.architecture().pointer_size() as u8,
        },

        MIRTypeKind::TaggedUnion { variants } => LMIRTypeKind::Struct {
            name: cx_type.strong_identifier().unwrap().to_owned(),
            fields: vec![
                (
                    "data".to_string(),
                    lower_union(
                        variants.iter().map(|f| definitions.resolve_type_id(f.ty())),
                        definitions,
                    ),
                ),
                (
                    "tag".to_string(),
                    LMIRType::with_implicit_abi(
                        definitions.architecture(),
                        LMIRTypeKind::Integer(LMIRIntegerType::I8),
                    ),
                ),
            ],
        },

        MIRTypeKind::Array {
            inner_type: _type,
            length: size,
        } => LMIRTypeKind::Array {
            element: Box::new(convert_type(
                definitions.resolve_type_id(*_type),
                definitions,
            )),
            size: *size,
        },

        MIRTypeKind::MemoryReference { .. } => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
            bytes: definitions.architecture().pointer_size() as u8,
        },

        MIRTypeKind::Structured { .. } => LMIRTypeKind::Struct {
            name: cx_type
                .strong_identifier()
                .map(str::to_owned)
                .unwrap_or("".to_owned()),
            fields: cx_type
                .aggregate_fields(definitions)
                .unwrap()
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type, definitions)))
                .collect::<Vec<_>>(),
        },

        MIRTypeKind::Union { .. } => LMIRTypeKind::Opaque { bytes: layout.size },

        MIRTypeKind::Unit => LMIRTypeKind::Unit,

        // Str is unsized; behind a reference it's just i8 for pointer element purposes
        MIRTypeKind::Undefined => {
            unreachable!(
                "Cannot lower incomplete type {}",
                cx_type.display_with(definitions)
            )
        }

        MIRTypeKind::Str => LMIRTypeKind::Integer(LMIRIntegerType::I8),
    };

    LMIRType {
        kind,
        alignment: layout.alignment as u8,
    }
}

fn lower_union<'a>(
    variants: impl Iterator<Item = &'a MIRType>,
    definitions: &MIRDecomposedRegistry,
) -> LMIRType {
    let (size, alignment) = variants
        .map(|f| {
            definitions.type_layout(f).unwrap_or_else(|err| {
                panic!("Failed to calculate union member layout: {}", err.message())
            })
        })
        .fold((0, 1), |(size, alignment), layout| {
            (size.max(layout.size), alignment.max(layout.alignment))
        });

    LMIRType::new(LMIRTypeKind::Opaque { bytes: size }, alignment as u8)
}
