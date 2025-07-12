use cx_data_ast::parse::ast::{CXBinOp, CXFunctionMap, CXFunctionPrototype, CXTypeMap, CXUnOp};
use cx_data_ast::parse::value_type::{CXType, CXTypeKind};
use cx_data_bytecode::{BCFloatBinOp, BCFloatUnOp, BCFunctionMap, BCFunctionPrototype, BCIntBinOp, BCIntUnOp, BCParameter, BCPtrBinOp, BCTypeMap, VirtualInstruction};
use cx_data_bytecode::types::{BCType, BCTypeKind, BCTypeSize};
use crate::builder::BytecodeBuilder;
use crate::instruction_gen::generate_instruction;

impl BytecodeBuilder {
    pub(crate) fn convert_cx_type(&mut self, cx_type: &CXType) -> Option<BCType> {
        convert_type(self, cx_type)
    }
    
    pub(crate) fn convert_fixed_cx_type(&self, cx_type: &CXType) -> Option<BCType> {
        convert_fixed_type(&self.cx_type_map, cx_type)
    }
    
    pub(crate) fn convert_cx_prototype(&self, cx_proto: &CXFunctionPrototype) -> Option<BCFunctionPrototype> {
        convert_cx_prototype(&self.cx_type_map, cx_proto)
    }
    
    pub(crate) fn cx_type_to_prototype(
        &self,
        cx_type: &CXType
    ) -> Option<BCFunctionPrototype> {
        let Some(CXTypeKind::Function { prototype })
            = cx_type.intrinsic_type_kind(&self.cx_type_map) else {
            panic!("Expected function type, got: {cx_type:?}");
        };
        
        self.convert_cx_prototype(prototype)
    }

    pub(crate) fn cx_ptr_binop(
        &self,
        op: &CXBinOp
    ) -> Option<BCPtrBinOp> {
        Some(
            match op {
                CXBinOp::Add => BCPtrBinOp::ADD,
                CXBinOp::Subtract => BCPtrBinOp::SUB,

                CXBinOp::Less => BCPtrBinOp::LT,
                CXBinOp::Greater => BCPtrBinOp::GT,
                CXBinOp::LessEqual => BCPtrBinOp::LE,
                CXBinOp::GreaterEqual => BCPtrBinOp::GE,

                CXBinOp::Equal => BCPtrBinOp::EQ,
                CXBinOp::NotEqual => BCPtrBinOp::NE,

                _ => return None
            }
        )
    }

    pub(crate) fn cx_u_binop(
        &self,
        op: &CXBinOp
    ) -> Option<BCIntBinOp> {
        Some(
            match op {
                CXBinOp::Divide         => BCIntBinOp::UDIV,
                CXBinOp::Modulus        => BCIntBinOp::UREM,
                
                CXBinOp::Less           => BCIntBinOp::ULT,
                CXBinOp::Greater        => BCIntBinOp::UGT,
                CXBinOp::LessEqual      => BCIntBinOp::ULE,
                CXBinOp::GreaterEqual   => BCIntBinOp::UGE,
                
                _ => self.cx_i_binop(op)?
            }
        )
    }

    pub(crate) fn cx_i_binop(
        &self,
        op: &CXBinOp
    ) -> Option<BCIntBinOp> {
        Some(
            match op {
                CXBinOp::Add            => BCIntBinOp::ADD,
                CXBinOp::Subtract       => BCIntBinOp::SUB,
                CXBinOp::Multiply       => BCIntBinOp::MUL,
                CXBinOp::Divide         => BCIntBinOp::IDIV,
                CXBinOp::Modulus        => BCIntBinOp::IREM,
                
                CXBinOp::BitAnd         => BCIntBinOp::BAND,
                CXBinOp::BitOr          => BCIntBinOp::BOR,
                CXBinOp::BitXor         => BCIntBinOp::BXOR,
                
                CXBinOp::LShift         => BCIntBinOp::SHL,
                CXBinOp::RShift         => BCIntBinOp::ASHR,
                
                CXBinOp::LAnd           => BCIntBinOp::LAND,
                CXBinOp::LOr            => BCIntBinOp::LOR,
                
                CXBinOp::Less           => BCIntBinOp::ILT,
                CXBinOp::Greater        => BCIntBinOp::IGT,
                CXBinOp::LessEqual      => BCIntBinOp::ILE,
                CXBinOp::GreaterEqual   => BCIntBinOp::IGE,
                CXBinOp::Equal          => BCIntBinOp::EQ,
                CXBinOp::NotEqual       => BCIntBinOp::NE,
                
                CXBinOp::Comma |
                CXBinOp::Assign(_) |
                CXBinOp::Access |
                CXBinOp::MethodCall |
                CXBinOp::ArrayIndex => panic!("Invalid binary operation: {op:?}"),
            }
        )
    }
    
    pub(crate) fn cx_unop(
        &self,
        op: &CXUnOp
    ) -> Option<BCIntUnOp> {
        Some(
            match op {
                CXUnOp::Negative          => BCIntUnOp::NEG,
                CXUnOp::BNot              => BCIntUnOp::BNOT,
                CXUnOp::LNot              => BCIntUnOp::LNOT,
                
                _ => todo!("Unsupported unary operation: {:?}", op)
            }
        )
    }
    
    pub(crate) fn cx_float_binop(
        &self,
        op: &CXBinOp
    ) -> Option<BCFloatBinOp> {
        Some(
            match op {
                CXBinOp::Add            => BCFloatBinOp::ADD,
                CXBinOp::Subtract       => BCFloatBinOp::SUB,
                CXBinOp::Multiply       => BCFloatBinOp::FMUL,
                CXBinOp::Divide         => BCFloatBinOp::FDIV,
                
                _ => todo!("Unsupported binary operation: {:?}", op)
            }
        )
    }
    
    pub(crate) fn cx_float_unop(
        &self,
        op: &CXUnOp
    ) -> Option<BCFloatUnOp> {
        Some(
            match op {
                CXUnOp::Negative          => BCFloatUnOp::NEG,
                
                _ => todo!("Unsupported unary operation: {:?}", op)
            }
        )
    }
}

fn convert_type(builder: &mut BytecodeBuilder, cx_type: &CXType) -> Option<BCType> {
    Some(
        BCType {
            kind: convert_type_kind(builder, &cx_type.kind)?
        }
    )
}

fn convert_fixed_type(cx_type_map: &CXTypeMap, cx_type: &CXType) -> Option<BCType> {
    Some(
        BCType {
            kind: convert_fixed_type_kind(cx_type_map, &cx_type.kind)?
        }
    )
}

pub(crate) fn convert_cx_prototype(cx_type_map: &CXTypeMap, cx_proto: &CXFunctionPrototype) -> Option<BCFunctionPrototype> {
    Some(
        BCFunctionPrototype {
            name: cx_proto.name.as_string(),
            return_type: convert_fixed_type(cx_type_map, &cx_proto.return_type)?,
            params: cx_proto.params.iter()
                .map(|param| BCParameter {
                    name: None,
                    _type: convert_fixed_type(cx_type_map, &param.type_).unwrap()
                })
                .collect(),
            var_args: cx_proto.var_args
        }
    )
}

pub(crate) fn convert_cx_type_map(cx_type_map: &CXTypeMap) -> BCTypeMap {
    cx_type_map.iter()
        .map(|(name, cx_type)| {
            (name.clone(), convert_fixed_type(cx_type_map, cx_type).unwrap())
        })
        .collect::<BCTypeMap>()
}

pub(crate) fn convert_cx_func_map(cx_type_map: &CXTypeMap, cx_proto: &CXFunctionMap) -> BCFunctionMap {
    cx_proto.iter()
        .map(|(name, cx_proto)| {
            (name.clone(), convert_cx_prototype(cx_type_map, cx_proto).unwrap())
        })
        .collect::<BCFunctionMap>()
}

pub(crate) fn convert_type_kind(builder: &mut BytecodeBuilder, cx_type_kind: &CXTypeKind) -> Option<BCTypeKind> {
    Some(
        match cx_type_kind {
            CXTypeKind::VariableLengthArray { size, _type } => {
                let bc_type = builder.convert_cx_type(_type)?;
                let size_id = generate_instruction(builder, size)?;
                
                let type_size = match bc_type.size() {
                    BCTypeSize::Fixed(size) => 
                        builder.add_instruction_bt(
                            VirtualInstruction::Immediate {
                                value: size as i32,
                            },
                            BCTypeKind::Unsigned { bytes: 8 }.into()
                        )?,
                    BCTypeSize::Variable(id) => id
                };
                
                let total_size = builder.add_instruction_bt(
                    VirtualInstruction::IntegerBinOp {
                        left: size_id,
                        right: type_size,
                        op: BCIntBinOp::MUL,
                    },
                    BCTypeKind::Unsigned { bytes: 8 }.into()
                )?;
                
                BCTypeKind::VariableSized {
                    size: total_size,
                    alignment: bc_type.alignment()
                }
            },
            
            _ => convert_fixed_type_kind(&builder.cx_type_map, cx_type_kind)?
        }
    )
}

pub(crate) fn convert_fixed_type_kind(cx_type_map: &CXTypeMap, cx_type_kind: &CXTypeKind) -> Option<BCTypeKind> {
    Some(
        match cx_type_kind {
            CXTypeKind::Identifier { name, .. } => {
                let inner = cx_type_map.get(name.as_str())
                    .expect("PANIC: Identifier not found in type map");

                convert_fixed_type_kind(cx_type_map, &inner.kind)?
            }

            CXTypeKind::Opaque { size, .. } =>
                BCTypeKind::Opaque { bytes: *size },
            
            CXTypeKind::Bool => 
                BCTypeKind::Bool,

            CXTypeKind::Integer { signed: true, bytes} =>
                BCTypeKind::Signed { bytes: *bytes },
            CXTypeKind::Integer { signed: false, bytes } =>
                BCTypeKind::Unsigned { bytes: *bytes },
            CXTypeKind::Float { bytes } =>
                BCTypeKind::Float { bytes: *bytes },

            CXTypeKind::PointerTo { nullable: false, .. } |
            CXTypeKind::StrongPointer { .. } =>
                BCTypeKind::Pointer { nullable: false },
            
            CXTypeKind::Function { .. } |
            CXTypeKind::PointerTo { nullable: true, .. } =>
                BCTypeKind::Pointer { nullable: true },
            
            CXTypeKind::Array { _type, size } =>
                BCTypeKind::Array {
                    element: Box::new(convert_fixed_type(cx_type_map, _type)?),
                    size: *size
                },
            
            CXTypeKind::MemoryAlias(inner) => {
                match inner.intrinsic_type_kind(cx_type_map)? {
                    CXTypeKind::Structured { .. } => convert_fixed_type_kind(cx_type_map, &inner.kind)?,
                    CXTypeKind::Union { .. } => convert_fixed_type_kind(cx_type_map, &inner.kind)?,
                    
                    _ => BCTypeKind::Pointer { nullable: true }
                }
            },
            
            CXTypeKind::Structured { fields, name, .. } =>
                BCTypeKind::Struct {
                    name: match name {
                        Some(name) => name.as_string(),
                        None => "".to_string()
                    },
                    fields: fields.iter()
                        .map(|(_name, _type)| Some((_name.clone(), convert_fixed_type(cx_type_map, _type)?)))
                        .collect::<Option<Vec<_>>>()?
                },
            CXTypeKind::Union { fields, name } =>
                BCTypeKind::Union {
                    name: match name {
                        Some(name) => name.as_string(),
                        None => "".to_string()
                    },
                    fields: fields.iter()
                        .map(|(_name, _type)| Some((_name.clone(), convert_fixed_type(cx_type_map, _type)?)))
                        .collect::<Option<Vec<_>>>()?
                },

            CXTypeKind::Unit => BCTypeKind::Unit,
            
            CXTypeKind::VariableLengthArray { .. } =>
                panic!("Variable length arrays are not supported in bytecode generation"),
        }
    )
}