use crate::builder::LMIRBuilder;
use cx_ast::data::CXLinkageMode;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRFunctionPrototype, LMIRFunctionSignature, LMIRParameter, LinkageType};
use cx_mir::mir::data::{
    MIRFloatType, MIRFunctionPrototype, MIRFunctionSignature, MIRIntegerType, MIRType,
    MIRTypeContext, MIRTypeKind,
};

impl LMIRBuilder {
    pub(crate) fn convert_cx_type(&self, cx_type: &MIRType) -> LMIRType {
        convert_type(cx_type, &self.type_definitions)
    }

    #[allow(dead_code)]
    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &MIRFunctionPrototype,
    ) -> LMIRFunctionPrototype {
        convert_cx_prototype(cx_proto, &self.type_definitions)
    }

    pub(crate) fn convert_cx_signature(
        &self,
        cx_sig: &MIRFunctionSignature,
    ) -> LMIRFunctionSignature {
        convert_cx_signature(cx_sig, &self.type_definitions)
    }

    pub(crate) fn convert_integer_type(&self, cx_itype: &MIRIntegerType) -> LMIRIntegerType {
        convert_integer_type(cx_itype)
    }

    pub(crate) fn convert_float_type(&self, cx_ftype: &MIRFloatType) -> LMIRFloatType {
        convert_float_type(cx_ftype)
    }

    pub fn convert_linkage(&self, linkage: CXLinkageMode) -> LinkageType {
        convert_linkage(linkage)
    }
}

fn convert_type(cx_type: &MIRType, definitions: &MIRTypeContext) -> LMIRType {
    LMIRType {
        kind: convert_type_kind(cx_type, definitions),
    }
}

fn convert_parameter_type(param_type: &MIRType, definitions: &MIRTypeContext) -> LMIRType {
    let bc_type = convert_type(param_type, definitions);

    if bc_type.is_structure() {
        LMIRType::default_pointer()
    } else {
        bc_type
    }
}

pub(crate) fn convert_cx_signature(
    cx_sig: &MIRFunctionSignature,
    definitions: &MIRTypeContext,
) -> LMIRFunctionSignature {
    let mut params = cx_sig
        .params
        .iter()
        .map(|param| LMIRParameter {
            name: param.name.as_ref().map(|name| name.as_string()),
            _type: convert_parameter_type(&param._type, definitions),
        })
        .collect::<Vec<_>>();

    let mut return_type = convert_type(&cx_sig.return_type, definitions);

    if cx_sig.return_type.is_memory_resident() {
        params.insert(
            0,
            LMIRParameter {
                name: Some("__internal_buffer".to_string()),
                _type: LMIRType::default_pointer(),
            },
        );

        return_type = LMIRType::default_pointer();
    }

    LMIRFunctionSignature {
        return_type,
        params,
        var_args: cx_sig.var_args,
    }
}

pub(crate) fn convert_cx_prototype(
    cx_proto: &MIRFunctionPrototype,
    definitions: &MIRTypeContext,
) -> LMIRFunctionPrototype {
    let signature = convert_cx_signature(&cx_proto.signature(), definitions);
    let temp_buffer = if cx_proto.return_type.is_memory_resident() {
        Some(convert_type(&cx_proto.return_type, definitions))
    } else {
        None
    };

    LMIRFunctionPrototype {
        name: cx_proto.name.to_string(),
        return_type: signature.return_type.clone(),
        params: signature.params.clone(),
        var_args: signature.var_args,
        linkage: LinkageType::Standard,
        temp_buffer,
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

pub(crate) fn convert_type_kind(cx_type: &MIRType, definitions: &MIRTypeContext) -> LMIRTypeKind {
    match &cx_type.kind {
        MIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },

        MIRTypeKind::Integer {
            signed: _,
            _type: itype,
        } => LMIRTypeKind::Integer(convert_integer_type(itype)),

        MIRTypeKind::Float { _type } => LMIRTypeKind::Float(convert_float_type(_type)),

        MIRTypeKind::Function { .. } => LMIRTypeKind::Pointer {
            nullable: true,
            dereferenceable: 0,
        },

        MIRTypeKind::PointerTo { .. } => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        MIRTypeKind::TaggedUnion { .. } => LMIRTypeKind::Struct {
            name: cx_type
                .get_name()
                .map(|name| name.as_string())
                .unwrap_or_default(),
            fields: vec![
                (
                    "data".to_string(),
                    LMIRTypeKind::Union {
                        name: String::new(),
                        fields: cx_type
                            .aggregate_fields(definitions)
                            .unwrap()
                            .iter()
                            .map(|(name, _type)| (name.clone(), convert_type(_type, definitions)))
                            .collect::<Vec<_>>(),
                    }
                    .into(),
                ),
                (
                    "tag".to_string(),
                    LMIRTypeKind::Integer(LMIRIntegerType::I8).into(),
                ),
            ],
        },

        MIRTypeKind::Array {
            inner_type: _type,
            length: size,
        } => LMIRTypeKind::Array {
            element: Box::new(convert_type(
                definitions
                    .get(*_type)
                    .unwrap_or_else(|| panic!("Unknown type id {}", _type.0)),
                definitions,
            )),
            size: *size,
        },

        MIRTypeKind::MemoryReference { .. } => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        MIRTypeKind::Structured { .. } => LMIRTypeKind::Struct {
            name: cx_type
                .get_name()
                .map(|name| name.as_string())
                .unwrap_or_default(),
            fields: cx_type
                .aggregate_fields(definitions)
                .unwrap()
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type, definitions)))
                .collect::<Vec<_>>(),
        },
        MIRTypeKind::Union { .. } => LMIRTypeKind::Union {
            name: cx_type
                .get_name()
                .map(|name| name.as_string())
                .unwrap_or_default(),
            fields: cx_type
                .aggregate_fields(definitions)
                .unwrap()
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type, definitions)))
                .collect::<Vec<_>>(),
        },

        MIRTypeKind::Unit => LMIRTypeKind::Unit,

        // Str is unsized; behind a reference it's just i8 for pointer element purposes
        MIRTypeKind::Undefined { .. } => {
            unreachable!("Cannot lower incomplete type {}", cx_type)
        }
        MIRTypeKind::Str => LMIRTypeKind::Integer(LMIRIntegerType::I8),
    }
}
