use crate::builder::BCBuilder;
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCType, BCTypeKind};
use cx_bytecode_data::{LinkageType, BCFunctionPrototype, BCParameter};
use cx_typechecker_data::mir::types::{
    CXFloatType, CXFunctionPrototype, CXIntegerType, CXType, CXTypeKind,
};

impl BCBuilder {
    pub(crate) fn convert_cx_type(&self, cx_type: &CXType) -> BCType {
        convert_type(cx_type)
    }

    #[allow(dead_code)]
    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &CXFunctionPrototype,
    ) -> BCFunctionPrototype {
        convert_cx_prototype(cx_proto)
    }
    
    pub(crate) fn convert_integer_type(&self, cx_itype: &CXIntegerType) -> BCIntegerType {
        convert_integer_type(cx_itype)
    }
    
    pub(crate) fn convert_float_type(&self, cx_ftype: &CXFloatType) -> BCFloatType {
        convert_float_type(cx_ftype)
    }
}

fn convert_type(cx_type: &CXType) -> BCType {
    BCType {
        kind: convert_type_kind(&cx_type.kind),
    }
}

fn convert_argument_type(cx_type: &CXType) -> BCType {
    let bc_type = convert_type(cx_type);

    match &bc_type.kind {
        BCTypeKind::Struct { .. } | BCTypeKind::Union { .. } => BCType::default_pointer(),

        _ => bc_type,
    }
}

pub(crate) fn convert_cx_prototype(cx_proto: &CXFunctionPrototype) -> BCFunctionPrototype {
    let mut params = cx_proto
        .params
        .iter()
        .map(|param| BCParameter {
            name: param.name.as_ref().map(|name| name.as_string()),
            _type: convert_argument_type(&param._type),
        })
        .collect::<Vec<_>>();

    let mut return_type = convert_type(&cx_proto.return_type);

    if cx_proto.return_type.is_structured() {
        params.insert(
            0,
            BCParameter {
                name: Some("__internal_buffer".to_string()),
                _type: BCType::default_pointer(),
            },
        );

        return_type = BCType::default_pointer();
    }

    let prototype = BCFunctionPrototype {
        name: cx_proto.mangle_name(),
        return_type: return_type.clone(),
        params: params.clone(),
        var_args: cx_proto.var_args,
        linkage: LinkageType::Standard,
    };

    prototype
}

fn convert_integer_type(itype: &CXIntegerType) -> BCIntegerType {
    match itype {
        CXIntegerType::I8 => BCIntegerType::I8,
        CXIntegerType::I16 => BCIntegerType::I16,
        CXIntegerType::I32 => BCIntegerType::I32,
        CXIntegerType::I64 => BCIntegerType::I64,
        CXIntegerType::I128 => BCIntegerType::I128,
    }
}

fn convert_float_type(ftype: &CXFloatType) -> BCFloatType {
    match ftype {
        CXFloatType::F32 => BCFloatType::F32,
        CXFloatType::F64 => BCFloatType::F64,
    }
}

pub(crate) fn convert_type_kind(cx_type_kind: &CXTypeKind) -> BCTypeKind {
    match cx_type_kind {
        CXTypeKind::Opaque { size, .. } => BCTypeKind::Opaque { bytes: *size },

        CXTypeKind::Bool => BCTypeKind::Bool,

        CXTypeKind::Integer {
            signed: _,
            _type: itype,
        } => BCTypeKind::Integer(convert_integer_type(itype)),

        CXTypeKind::Float { _type } => BCTypeKind::Float(convert_float_type(_type)),

        CXTypeKind::StrongPointer { is_array: true, .. }
        | CXTypeKind::PointerTo {
            nullable: false, ..
        } => BCTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        CXTypeKind::StrongPointer {
            is_array: false,
            inner_type: inner,
            ..
        } => {
            let inner_as_bc = convert_type(inner);

            BCTypeKind::Pointer {
                nullable: false,
                dereferenceable: inner_as_bc.size() as u32,
            }
        }

        CXTypeKind::Function { .. } | CXTypeKind::PointerTo { nullable: true, .. } => {
            BCTypeKind::Pointer {
                nullable: true,
                dereferenceable: 0,
            }
        }

        CXTypeKind::TaggedUnion { name, variants } => BCTypeKind::Struct {
            name: name.as_string(),
            fields: vec![
                (
                    "data".to_string(),
                    BCTypeKind::Union {
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
                    BCTypeKind::Integer(BCIntegerType::I32).into(),
                ),
            ],
        },

        CXTypeKind::Array {
            inner_type: _type,
            size,
        } => BCTypeKind::Array {
            element: Box::new(convert_type(_type)),
            size: *size,
        },

        CXTypeKind::MemoryReference(..) => BCTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        CXTypeKind::Structured { fields, name, .. } => BCTypeKind::Struct {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type)))
                .collect::<Vec<_>>(),
        },
        CXTypeKind::Union {
            variants: fields,
            name,
        } => BCTypeKind::Union {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| (_name.clone(), convert_type(_type)))
                .collect::<Vec<_>>(),
        },

        CXTypeKind::Unit => BCTypeKind::Unit,
    }
}
