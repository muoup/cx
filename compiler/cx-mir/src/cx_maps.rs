use crate::builder::MIRBuilder;
use crate::instruction_gen::generate_instruction;
use cx_parsing_data::parse::ast::CXBinOp;
use cx_mir_data::types::{MIRType, MIRTypeKind, MIRTypeSize};
use cx_mir_data::{
    BCFloatBinOp, BCFunctionMap, BCPtrBinOp, LinkageType, MIRFunctionPrototype, MIRIntBinOp,
    MIRParameter, VirtualInstruction,
};
use cx_typechecker_data::cx_types::{CXFunctionPrototype, CXType, CXTypeKind};
use cx_typechecker_data::function_map::CXFnMap;

impl MIRBuilder {
    pub(crate) fn convert_cx_type(&mut self, cx_type: &CXType) -> Option<MIRType> {
        convert_type(self, cx_type)
    }

    pub(crate) fn convert_fixed_cx_type(&self, cx_type: &CXType) -> Option<MIRType> {
        convert_fixed_type(cx_type)
    }

    pub(crate) fn convert_cx_prototype(
        &self,
        cx_proto: &CXFunctionPrototype,
    ) -> Option<MIRFunctionPrototype> {
        convert_cx_prototype(cx_proto)
    }

    pub(crate) fn cx_ptr_binop(&self, op: &CXBinOp) -> Option<BCPtrBinOp> {
        Some(match op {
            CXBinOp::Add => BCPtrBinOp::ADD,
            CXBinOp::Subtract => BCPtrBinOp::SUB,

            CXBinOp::Less => BCPtrBinOp::LT,
            CXBinOp::Greater => BCPtrBinOp::GT,
            CXBinOp::LessEqual => BCPtrBinOp::LE,
            CXBinOp::GreaterEqual => BCPtrBinOp::GE,

            CXBinOp::Equal => BCPtrBinOp::EQ,
            CXBinOp::NotEqual => BCPtrBinOp::NE,

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

    pub(crate) fn cx_float_binop(&self, op: &CXBinOp) -> Option<BCFloatBinOp> {
        Some(match op {
            CXBinOp::Add => BCFloatBinOp::ADD,
            CXBinOp::Subtract => BCFloatBinOp::SUB,
            CXBinOp::Multiply => BCFloatBinOp::FMUL,
            CXBinOp::Divide => BCFloatBinOp::FDIV,

            _ => todo!("Unsupported binary operation: {:?}", op),
        })
    }
}

fn convert_type(builder: &mut MIRBuilder, cx_type: &CXType) -> Option<MIRType> {
    Some(MIRType {
        kind: convert_type_kind(builder, &cx_type.kind)?,
    })
}

fn convert_fixed_type(cx_type: &CXType) -> Option<MIRType> {
    Some(MIRType {
        kind: convert_fixed_type_kind(&cx_type.kind)?,
    })
}

fn convert_argument_type(cx_type: &CXType) -> Option<MIRType> {
    let bc_type = convert_fixed_type(cx_type)?;

    match &bc_type.kind {
        MIRTypeKind::Struct { .. } | MIRTypeKind::Union { .. } => Some(MIRType::default_pointer()),

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

    let mut return_type = convert_fixed_type(&cx_proto.return_type).unwrap();

    if cx_proto.return_type.is_structured() {
        params.insert(
            0,
            MIRParameter {
                name: Some("__internal_buffer".to_string()),
                _type: MIRType::default_pointer(),
            },
        );
        return_type = MIRType::default_pointer();
    }

    Some(MIRFunctionPrototype {
        name: cx_proto.mangle_name(),
        return_type,
        params,
        var_args: cx_proto.var_args,
        linkage: LinkageType::Standard,
    })
}

pub(crate) fn convert_cx_func_map(cx_proto: &CXFnMap) -> BCFunctionMap {
    cx_proto
        .iter()
        .map(|(_, cx_proto)| (cx_proto.mangle_name(), convert_cx_prototype(cx_proto).unwrap()))
        .collect::<BCFunctionMap>()
}

pub(crate) fn convert_type_kind(
    builder: &mut MIRBuilder,
    cx_type_kind: &CXTypeKind,
) -> Option<MIRTypeKind> {
    Some(match cx_type_kind {
        CXTypeKind::VariableLengthArray { _type, size } => {
            let bc_type = builder.convert_cx_type(_type)?;
            let size_id = generate_instruction(builder, size.as_ref())?;

            let type_size = match bc_type.size() {
                MIRTypeSize::Fixed(size) => builder.int_const(size as i32, 8, false),
                MIRTypeSize::Variable(id) => id,
            };

            let total_size = builder.add_instruction(
                VirtualInstruction::IntegerBinOp {
                    left: size_id,
                    right: type_size,
                    op: MIRIntBinOp::MUL,
                },
                MIRTypeKind::Unsigned { bytes: 8 }.into(),
            )?;

            MIRTypeKind::VariableSized {
                size: Box::new(total_size),
                alignment: bc_type.alignment(),
            }
        }

        _ => convert_fixed_type_kind(cx_type_kind)?,
    })
}

pub(crate) fn convert_fixed_type_kind(cx_type_kind: &CXTypeKind) -> Option<MIRTypeKind> {
    Some(match cx_type_kind {
        CXTypeKind::Opaque { size, .. } => MIRTypeKind::Opaque { bytes: *size },

        CXTypeKind::Bool => MIRTypeKind::Bool,

        CXTypeKind::Integer {
            signed: true,
            bytes,
        } => MIRTypeKind::Signed { bytes: *bytes },
        CXTypeKind::Integer {
            signed: false,
            bytes,
        } => MIRTypeKind::Unsigned { bytes: *bytes },
        CXTypeKind::Float { bytes } => MIRTypeKind::Float { bytes: *bytes },

        CXTypeKind::StrongPointer { is_array: true, .. }
        | CXTypeKind::PointerTo {
            nullable: false, ..
        } => MIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        CXTypeKind::StrongPointer {
            is_array: false,
            inner_type: inner,
            ..
        } => {
            let inner_as_bc = convert_fixed_type(inner)?;

            MIRTypeKind::Pointer {
                nullable: false,
                dereferenceable: inner_as_bc.fixed_size() as u32,
            }
        }

        CXTypeKind::Function { .. } | CXTypeKind::PointerTo { nullable: true, .. } => {
            MIRTypeKind::Pointer {
                nullable: true,
                dereferenceable: 0,
            }
        }

        CXTypeKind::TaggedUnion { name, variants } => MIRTypeKind::Struct {
            name: name.as_string(),
            fields: vec![
                (
                    "data".to_string(),
                    MIRTypeKind::Union {
                        name: String::new(),
                        fields: variants
                            .iter()
                            .map(|(name, _type)| Some((name.clone(), convert_fixed_type(_type)?)))
                            .collect::<Option<Vec<_>>>()?,
                    }
                    .into(),
                ),
                ("tag".to_string(), MIRTypeKind::Unsigned { bytes: 4 }.into()),
            ],
        },

        CXTypeKind::Array {
            inner_type: _type,
            size,
        } => MIRTypeKind::Array {
            element: Box::new(convert_fixed_type(_type)?),
            size: *size,
        },

        CXTypeKind::MemoryReference(..) => MIRTypeKind::Pointer {
            nullable: false,
            dereferenceable: 0,
        },

        CXTypeKind::Structured { fields, name, .. } => MIRTypeKind::Struct {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| Some((_name.clone(), convert_fixed_type(_type)?)))
                .collect::<Option<Vec<_>>>()?,
        },
        CXTypeKind::Union {
            variants: fields,
            name,
        } => MIRTypeKind::Union {
            name: match name {
                Some(name) => name.as_string(),
                None => "".to_string(),
            },
            fields: fields
                .iter()
                .map(|(_name, _type)| Some((_name.clone(), convert_fixed_type(_type)?)))
                .collect::<Option<Vec<_>>>()?,
        },

        CXTypeKind::Unit => MIRTypeKind::Unit,

        CXTypeKind::VariableLengthArray { .. } => {
            panic!("Variable length arrays are not supported in bytecode generation")
        }
    })
}
