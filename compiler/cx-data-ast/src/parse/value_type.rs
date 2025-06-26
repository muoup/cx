use cx_util::log_error;
use serde::{Deserialize, Serialize};
use crate::parse::ast::{CXBinOp, CXExpr, CXExprKind, CXFunctionPrototype, CXTypeMap};
use crate::parse::identifier::CXIdent;

pub type CXTypeSpecifier = u8;

pub const CX_CONST: CXTypeSpecifier = 1 << 0;
pub const CX_VOLATILE: CXTypeSpecifier = 1 << 1;
pub const CX_RESTRICT: CXTypeSpecifier = 1 << 2;
pub const CX_THREAD_LOCAL: CXTypeSpecifier = 1 << 3;
pub const CX_UNION: CXTypeSpecifier = 1 << 4;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CXType {
    pub specifiers: CXTypeSpecifier,
    pub kind: CXTypeKind
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum CXTypeKind {
    Integer { bytes: u8, signed: bool },
    Float { bytes: u8 },
    Structured {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>
    },
    Union {
        name: Option<CXIdent>,
        fields: Vec<(String, CXType)>
    },
    Unit,

    PointerTo(Box<CXType>),
    MemoryAlias(Box<CXType>),
    Array {
        size: usize,
        _type: Box<CXType>
    },
    VariableLengthArray {
        size: Box<CXExpr>,
        _type: Box<CXType>,
    },
    Opaque {
        name: String,
        size: usize
    },
    Function {
        prototype: Box<CXFunctionPrototype>,
    },

    Identifier {
        name: CXIdent,
        predeclaration: PredeclarationType
    }
}

impl From<&str> for CXTypeKind {
    fn from(value: &str) -> Self {
        CXTypeKind::Identifier {
            name: CXIdent::from(value),
            predeclaration: PredeclarationType::None
        }
    }
}

impl From<CXIdent> for CXTypeKind {
    fn from(value: CXIdent) -> Self {
        CXTypeKind::Identifier {
            name: value,
            predeclaration: PredeclarationType::None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum PredeclarationType {
    None,
    Struct,
    Union,
    Enum
}

impl CXTypeKind {
    pub fn to_val_type(self) -> CXType {
        CXType::new(
            0,
            self
        )
    }
}

impl CXType {
    pub fn unit() -> Self {
        CXType {
            specifiers: 0,
            kind: CXTypeKind::Unit
        }
    }

    pub fn new(specifiers: CXTypeSpecifier, underlying_type: CXTypeKind) -> Self {
        CXType {
            specifiers,
            kind: underlying_type
        }
    }

    pub fn add_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXType {
            specifiers: self.specifiers | specifier,
            kind: self.kind.clone()
        }
    }

    pub fn remove_specifier(&self, specifier: CXTypeSpecifier) -> Self {
        CXType {
            specifiers: self.specifiers & !specifier,
            kind: self.kind.clone()
        }
    }

    pub fn get_specifier(&self, specifier: CXTypeSpecifier) -> bool {
        self.specifiers & specifier == specifier
    }

    pub fn intrinsic_type<'a>(&'a self, type_map: &'a CXTypeMap) -> Option<&'a CXTypeKind> {
        get_intrinsic_type(type_map, self)
    }
    
    pub fn is_structure_ref(&self, type_map: &CXTypeMap) -> bool {
        let Some(CXTypeKind::MemoryAlias(inner)) = self.intrinsic_type(type_map) else {
            return false;
        };
          
        inner.is_structured(type_map)
    }
    
    pub fn is_structured(&self, type_map: &CXTypeMap) -> bool {
        matches!(self.intrinsic_type(type_map), Some(CXTypeKind::Structured { .. }))
    }
    
    pub fn is_integer(&self, type_map: &CXTypeMap) -> bool {
        matches!(self.intrinsic_type(type_map), Some(CXTypeKind::Integer { .. }))
    }
    
    pub fn get_structure_ref(&self, type_map: &CXTypeMap) -> Option<CXTypeKind> {
        let Some(CXTypeKind::MemoryAlias(inner)) = self.intrinsic_type(type_map) else {
            return None;
        };

        let intrin = inner.intrinsic_type(type_map);
        if matches!(intrin, Some(CXTypeKind::Structured { .. })) {
            intrin.cloned()
        } else {
            panic!("Expected a structured type, found: {:?}", inner.kind);
        }
    }
    
    pub fn mem_ref_inner(&self, type_map: &CXTypeMap) -> Option<CXTypeKind> {
        let Some(CXTypeKind::MemoryAlias(inner)) = self.intrinsic_type(type_map) else {
            return None;
        };
        
        inner.intrinsic_type(type_map).cloned()
    }
    
    pub fn is_pointer(&self, type_map: &CXTypeMap) -> bool {
        matches!(self.intrinsic_type(type_map), Some(CXTypeKind::PointerTo(_)))
    }

    pub fn is_void(&self, type_map: &CXTypeMap) -> bool {
        matches!(self.intrinsic_type(type_map), Some(CXTypeKind::Unit))
    }

    pub fn intrin_eq(&self, other: &CXType, type_map: &CXTypeMap) -> bool {
        same_type(type_map, self, other)
    }

    pub fn is_intrinsic(&self, intrin: &CXTypeKind, type_map: &CXTypeMap) -> bool {
        matches!(get_intrinsic_type(type_map, self), intrin)
    }

    pub fn size(&self, type_map: &CXTypeMap) -> Option<TypeSize> {
        get_type_size(type_map, self)
    }

    pub fn pointer_to(self) -> Self {
        CXType {
            specifiers: 0,
            kind: CXTypeKind::PointerTo(Box::new(self))
        }
    }
}

pub fn same_type(type_map: &CXTypeMap, t1: &CXType, t2: &CXType) -> bool {
    match (&t1.kind, &t2.kind) {
        (CXTypeKind::Identifier { name: name1, .. },
            CXTypeKind::Identifier { name: name2, .. })
        if name1 == name2 =>
            true,

        (CXTypeKind::Identifier { name: name1, .. }, _) =>
            same_type(type_map, &type_map.get(name1.as_str()).unwrap(), t2),

        (_, CXTypeKind::Identifier { name: name2, .. }) =>
            same_type(type_map, t1, &type_map.get(name2.as_str()).expect(format!("Type not found: {name2}").as_str())),

        (CXTypeKind::Array { _type: t1_type, .. },
         CXTypeKind::Array { _type: t2_type, .. }) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeKind::PointerTo(t1_type),
         CXTypeKind::PointerTo(t2_type)) =>
            same_type(type_map, t1_type, t2_type),

        (CXTypeKind::Structured { fields: t1_fields, .. },
         CXTypeKind::Structured { fields: t2_fields, .. }) => {
            t1_fields.iter().zip(t2_fields.iter())
                .all(|(f1, f2)|
                    same_type(type_map, &f1.1, &f2.1))
        },

        (CXTypeKind::Function { prototype: p1 },
         CXTypeKind::Function { prototype: p2 }) =>
            same_type(type_map, &p1.return_type, &p2.return_type) &&
                p1.params.iter().zip(p2.params.iter())
                    .all(|(a1, a2)| same_type(type_map, &a1.type_, &a2.type_)),

        (CXTypeKind::Integer { bytes: t1_bytes, signed: t1_signed },
            CXTypeKind::Integer { bytes: t2_bytes, signed: t2_signed }) =>
            t1_bytes == t2_bytes && t1_signed == t2_signed,

        (CXTypeKind::Float { bytes: t1_bytes },
            CXTypeKind::Float { bytes: t2_bytes }) =>
            t1_bytes == t2_bytes,

        (CXTypeKind::Unit, CXTypeKind::Unit) =>
            true,

        _ => false,
    }
}

pub fn get_intrinsic_type<'a>(type_map: &'a CXTypeMap, type_: &'a CXType) -> Option<&'a CXTypeKind> {
    match &type_.kind {
        CXTypeKind::Identifier { name, .. }
            => type_map.get(name.as_str())
                .and_then(|_type| get_intrinsic_type(type_map, _type)),

        _ => Some(&type_.kind)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum TypeSize {
    Fixed(usize),
    Variable {
        elem_size: Box<TypeSize>,
        size_expr: Box<CXExpr>
    }
}

impl TypeSize {
    pub fn assert_fixed(&self, msg: &str) -> usize {
        match self {
            TypeSize::Fixed(size) => *size,
            TypeSize::Variable { .. } => panic!("{msg}: Expected fixed size, found variable size"),
        }
    }
}

pub fn get_type_size(type_map: &CXTypeMap, type_: &CXType) -> Option<TypeSize> {
    Some(
        match &type_.kind {
            CXTypeKind::Unit => TypeSize::Fixed(0),

            CXTypeKind::Float { bytes } |
            CXTypeKind::Integer { bytes, .. } => TypeSize::Fixed(*bytes as usize),

            CXTypeKind::MemoryAlias(inner) =>
                get_type_size(type_map, inner.as_ref())?,

            CXTypeKind::Array { _type, size } => {
                let elem_size = get_type_size(type_map, _type)?;
                
                match elem_size {
                    TypeSize::Fixed(field_size) 
                        => TypeSize::Fixed(field_size * size),

                    TypeSize::Variable { .. } => {
                        let size_literal = CXExprKind::IntLiteral {
                            val: *size as i64,
                            bytes: 8
                        }.into_expr(0, 0);
                        
                        TypeSize::Variable {
                            elem_size: Box::new(elem_size),
                            size_expr: Box::new(size_literal)
                        }
                    }
                }
            },

            CXTypeKind::Structured { fields, .. } =>
                TypeSize::Fixed(
                    fields.iter()
                        .map(|field|
                            get_type_size(type_map, &field.1).unwrap().assert_fixed("Structured type field size"))
                        .sum()
                ),
            CXTypeKind::Union { fields, .. } =>
                TypeSize::Fixed(
                    fields.iter()
                        .map(|field|
                            get_type_size(type_map, &field.1).unwrap().assert_fixed("Union type field size"))
                        .max()?
                ),

            CXTypeKind::PointerTo(_) |
            CXTypeKind::Function { .. } => TypeSize::Fixed(8),

            CXTypeKind::Opaque { size, .. } => TypeSize::Fixed(*size),
            CXTypeKind::Identifier { name, .. } =>
                type_map.get(name.as_str())
                    .map(|_type| get_type_size(type_map, _type))
                    .flatten()?,

            CXTypeKind::VariableLengthArray { _type, size } => {
                TypeSize::Variable {
                    elem_size: Box::new(get_type_size(type_map, _type)?),
                    size_expr: size.clone()
                }
            }
        }
    )
}

pub struct StructAccessRecord {
    pub field_type: CXType,
    pub field_offset: usize,
    pub field_index: usize,
    pub field_name: String
}

pub fn struct_field_access(
    type_map: &CXTypeMap,
    type_: &CXType,
    field: &str
) -> Option<StructAccessRecord> {
    let CXTypeKind::Structured { fields, .. } = get_intrinsic_type(type_map, type_)? else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    let mut offset = 0;

    for (i, (name, ty)) in fields.iter().enumerate() {
        if name == field {
            return Some(
                StructAccessRecord {
                    field_type: ty.clone(),
                    field_offset: offset,
                    field_index: i,
                    field_name: name.clone()
                }
            );
        }

        offset += get_type_size(type_map, ty)?.assert_fixed("Structured type field size");
    }

    None
}

pub fn struct_field_type(
    type_map: &CXTypeMap,
    type_: &CXType,
    field: &str
) -> Option<CXType> {
    let Some(CXTypeKind::Structured { fields, .. }) = type_.get_structure_ref(type_map) else {
        log_error!("Cannot access field {field} of non-structured type {type_}");
    };

    fields.iter()
        .find(|(name, _)| name == field)
        .map(|(_, ty)| ty.clone())
}
