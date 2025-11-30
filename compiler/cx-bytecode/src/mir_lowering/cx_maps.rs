use crate::builder::MIRBuilder;
use crate::instruction_gen::generate_instruction;
use cx_bytecode_data::types::{BCFloatType, BCIntegerType, BCTypeKind, BCType, MIRTypeSize};
use cx_bytecode_data::{
    BCFunctionMap, LinkageType, MIRFloatBinOp, MIRFunctionPrototype, MIRIntBinOp, MIRParameter,
    MIRPtrBinOp,
};
use cx_parsing_data::ast::CXBinOp;
use cx_typechecker_data::function_map::CXFnMap;
use cx_typechecker_data::mir::types::{CXFloatType, CXFunctionPrototype, CXIntegerType, CXType, CXTypeKind};

impl MIRBuilder {
    pub(crate) fn convert_cx_type(&self, cx_type: &CXType) -> Option<BCType> {
        convert_type(cx_type)
    }

    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &CXFunctionPrototype,
    ) -> Option<MIRFunctionPrototype> {
        convert_cx_prototype(cx_proto)
    }

    pub(crate) fn cx_ptr_binop(&self, op: &CXBinOp) -> Option<MIRPtrBinOp> {
        Some(match op {
            CXBinOp::Add => MIRPtrBinOp::ADD,
            CXBinOp::Subtract => MIRPtrBinOp::SUB,

            CXBinOp::Less => MIRPtrBinOp::LT,
            CXBinOp::Greater => MIRPtrBinOp::GT,
            CXBinOp::LessEqual => MIRPtrBinOp::LE,
            CXBinOp::GreaterEqual => MIRPtrBinOp::GE,

            CXBinOp::Equal => MIRPtrBinOp::EQ,
            CXBinOp::NotEqual => MIRPtrBinOp::NE,

            _ => return None,
        })
    }

    pub(crate) fn cx_u_binop(&self, op: &CXBinOp) -> Option<MIRIntBinOp> {
        Some(match op {
            CXBinOp::Divide => MIRIntBinOp::UDIV,
            CXBinOp::Modulus => MIRIntBinOp::UREM,

            CXBinOp::Less => MIRIntBinOp::ULT,
            CXBinOp::Greater => MIRIntBinOp::UGT,
            CXBinOp::LessEqual => MIRIntBinOp::ULE,
            CXBinOp::GreaterEqual => MIRIntBinOp::UGE,

            _ => self.cx_i_binop(op)?,
        })
    }

    pub(crate) fn cx_i_binop(&self, op: &CXBinOp) -> Option<MIRIntBinOp> {
        Some(match op {
            CXBinOp::Add => MIRIntBinOp::ADD,
            CXBinOp::Subtract => MIRIntBinOp::SUB,
            CXBinOp::Multiply => MIRIntBinOp::MUL,
            CXBinOp::Divide => MIRIntBinOp::IDIV,
            CXBinOp::Modulus => MIRIntBinOp::IREM,

            CXBinOp::BitAnd => MIRIntBinOp::BAND,
            CXBinOp::BitOr => MIRIntBinOp::BOR,
            CXBinOp::BitXor => MIRIntBinOp::BXOR,

            CXBinOp::LShift => MIRIntBinOp::SHL,
            CXBinOp::RShift => MIRIntBinOp::ASHR,

            CXBinOp::LAnd => MIRIntBinOp::LAND,
            CXBinOp::LOr => MIRIntBinOp::LOR,

            CXBinOp::Less => MIRIntBinOp::ILT,
            CXBinOp::Greater => MIRIntBinOp::IGT,
            CXBinOp::LessEqual => MIRIntBinOp::ILE,
            CXBinOp::GreaterEqual => MIRIntBinOp::IGE,
            CXBinOp::Equal => MIRIntBinOp::EQ,
            CXBinOp::NotEqual => MIRIntBinOp::NE,

            _ => return None,
        })
    }

    pub(crate) fn cx_float_binop(&self, op: &CXBinOp) -> Option<MIRFloatBinOp> {
        Some(match op {
            CXBinOp::Add => MIRFloatBinOp::ADD,
            CXBinOp::Subtract => MIRFloatBinOp::SUB,
            CXBinOp::Multiply => MIRFloatBinOp::FMUL,
            CXBinOp::Divide => MIRFloatBinOp::FDIV,

            _ => todo!("Unsupported binary operation: {:?}", op),
        })
    }
}

fn convert_type(cx_type: &CXType) -> Option<BCType> {
    Some(BCType {
        kind: convert_type_kind(&cx_type.kind)?,
    })
}

fn convert_argument_type(cx_type: &CXType) -> Option<BCType> {
    let bc_type = convert_type(cx_type)?;

    match &bc_type.kind {
        BCTypeKind::Struct { .. } | BCTypeKind::Union { .. } => Some(BCType::default_pointer()),

        _ => Some(bc_type),
    }
}

pub(crate) fn convert_cx_prototype(cx_proto: &CXFunctionPrototype) -> Option<MIRFunctionPrototype> {
    let mut params = cx_proto
        .params
        .iter()
        .map(|param| MIRParameter {
            name: param.name.as_ref().map(|name| name.as_string()),
            _type: convert_argument_type(&param._type).unwrap(),
        })
        .collect::<Vec<_>>();

    let mut return_type = convert_type(&cx_proto.return_type).unwrap();

    if cx_proto.return_type.is_structured() {
        params.insert(
            0,
            MIRParameter {
                name: Some("__internal_buffer".to_string()),
                _type: BCType::default_pointer(),
            },
        );

        return_type = BCType::default_pointer();
    }

    let prototype = MIRFunctionPrototype {
        name: cx_proto.mangle_name(),
        return_type: return_type.clone(),
        params: params.clone(),
        var_args: cx_proto.var_args,
        linkage: LinkageType::Standard,
    };

    Some(prototype)
}

pub(crate) fn convert_cx_func_map(cx_proto: &CXFnMap) -> BCFunctionMap {
    cx_proto
        .values()
        .map(|cx_proto| {
            (
                cx_proto.mangle_name(),
                convert_cx_prototype(cx_proto).unwrap(),
            )
        })
        .collect::<BCFunctionMap>()
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

pub(crate) fn convert_type_kind(cx_type_kind: &CXTypeKind) -> Option<BCTypeKind> {
    Some(match cx_type_kind {
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
            let inner_as_bc = convert_type(inner)?;

            BCTypeKind::Pointer {
                nullable: false,
                dereferenceable: inner_as_bc.fixed_size() as u32,
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
                            .map(|(name, _type)| Some((name.clone(), convert_type(_type)?)))
                            .collect::<Option<Vec<_>>>()?,
                    }
                    .into(),
                ),
                ("tag".to_string(), BCTypeKind::Integer(BCIntegerType::I32).into()),
            ],
        },

        CXTypeKind::Array {
            inner_type: _type,
            size,
        } => BCTypeKind::Array {
            element: Box::new(convert_type(_type)?),
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
                .map(|(_name, _type)| Some((_name.clone(), convert_type(_type)?)))
                .collect::<Option<Vec<_>>>()?,
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
                .map(|(_name, _type)| Some((_name.clone(), convert_type(_type)?)))
                .collect::<Option<Vec<_>>>()?,
        },

        CXTypeKind::Unit => BCTypeKind::Unit,
    })
}
