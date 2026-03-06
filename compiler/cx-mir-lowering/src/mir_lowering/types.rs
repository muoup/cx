use crate::builder::LMIRBuilder;
use cx_lmir::types::{LMIRFloatType, LMIRIntegerType, LMIRType, LMIRTypeKind};
use cx_lmir::{LMIRFunctionPrototype, LMIRParameter, LinkageType};
use cx_ast::data::CXLinkageMode;
use cx_mir::mir::types::{
    MIRFloatType, MIRFunctionPrototype, MIRIntegerType, MIRType, MIRTypeKind,
};

impl LMIRBuilder {
    pub(crate) fn convert_cx_type(&self, cx_type: &MIRType) -> LMIRType {
        convert_type(cx_type)
    }

    #[allow(dead_code)]
    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &MIRFunctionPrototype,
    ) -> LMIRFunctionPrototype {
        convert_cx_prototype(cx_proto)
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

fn convert_type(cx_type: &MIRType) -> LMIRType {
    LMIRType {
        kind: convert_type_kind(&cx_type.kind),
    }
}

fn convert_parameter_type(param_type: &MIRType) -> LMIRType {
    let bc_type = convert_type(param_type);
    
    if bc_type.is_structure() {
        LMIRType::default_pointer()
    } else {
        bc_type
    }
}

pub(crate) fn convert_cx_prototype(cx_proto: &MIRFunctionPrototype) -> LMIRFunctionPrototype {
    let mut params = cx_proto
        .params
        .iter()
        .map(|param| LMIRParameter {
            name: param.name.as_ref().map(|name| name.as_string()),
            _type: convert_parameter_type(&param._type),
        })
        .collect::<Vec<_>>();

    let mut return_type = convert_type(&cx_proto.return_type);
    let mut buffer_type = None;

    if cx_proto.return_type.is_memory_resident() {
        params.insert(
            0,
            LMIRParameter {
                name: Some("__internal_buffer".to_string()),
                _type: LMIRType::default_pointer(),
            },
        );

        return_type = LMIRType::default_pointer();
        buffer_type = Some(convert_type(&cx_proto.return_type));
    }

    LMIRFunctionPrototype {
        name: cx_proto.name.to_string(),
        return_type: return_type.clone(),
        params: params.clone(),
        var_args: cx_proto.var_args,
        linkage: LinkageType::Standard,
        temp_buffer: buffer_type,
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

pub(crate) fn convert_type_kind(cx_type_kind: &MIRTypeKind) -> LMIRTypeKind {
    match cx_type_kind {
        MIRTypeKind::Opaque { size, .. } => LMIRTypeKind::Opaque { bytes: *size },

        MIRTypeKind::Integer {
            signed: _,
            _type: itype,
        } => LMIRTypeKind::Integer(convert_integer_type(itype)),

        MIRTypeKind::Float { _type } => LMIRTypeKind::Float(convert_float_type(_type)),

        MIRTypeKind::PointerTo {
            nullable: false, ..
        } => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        MIRTypeKind::Function { .. } | MIRTypeKind::PointerTo { nullable: true, .. } => {
            LMIRTypeKind::Pointer {
                nullable: true,
                dereferenceable: 0,
            }
        }

        MIRTypeKind::TaggedUnion { name, variants } => LMIRTypeKind::Struct {
            name: name.as_string(),
            fields: vec![
                (
                    "data".to_string(),
                    LMIRTypeKind::Union {
                        name: String::new(),
                        fields: variants
                            .iter()
                            .map(|(name, _type)| (name.clone(), convert_type(_type)))
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
            size,
        } => LMIRTypeKind::Array {
            element: Box::new(convert_type(_type)),
            size: *size,
        },

        MIRTypeKind::MemoryReference(..) => LMIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        MIRTypeKind::Structured { fields, name, .. } => LMIRTypeKind::Struct {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type)))
                .collect::<Vec<_>>(),
        },
        MIRTypeKind::Union {
            variants: fields,
            name,
        } => LMIRTypeKind::Union {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type)))
                .collect::<Vec<_>>(),
        },

        MIRTypeKind::Unit => LMIRTypeKind::Unit,
    }
}
