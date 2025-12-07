use crate::mir::types::{CXFloatType, CXIntegerType, CXTypeKind};

pub fn is_intrinsic_type(name: &str) -> bool {
    for (intrinsic_name, _) in INTRINSIC_TYPES.iter() {
        if intrinsic_name == &name {
            return true;
        }
    }
    false
}

pub const INTRINSIC_IMPORTS: &[&str] = &[
    "std/intrinsic/assertion.cx",
];

pub const INTRINSIC_TYPES: &[(&str, CXTypeKind)] = &[
    ("void", CXTypeKind::Unit),
    ("bool", CXTypeKind::Bool),
    (
        "i8",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "i16",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I16,
        },
    ),
    (
        "i32",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "i64",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "u8",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "u16",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I16,
        },
    ),
    (
        "u32",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "u64",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "usize",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::from_bytes(std::mem::size_of::<usize>() as u8).unwrap(),
        },
    ),
    (
        "isize",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::from_bytes(std::mem::size_of::<isize>() as u8).unwrap(),
        },
    ),
    (
        "f32",
        CXTypeKind::Float {
            _type: CXFloatType::F32,
        },
    ),
    (
        "f64",
        CXTypeKind::Float {
            _type: CXFloatType::F64,
        },
    ),
    (
        "int",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "signed int",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "unsigned int",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "signed",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "unsigned",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "long",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long int",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "unsigned long",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long long",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long long int",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "unsigned long long",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "char",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "unsigned char",
        CXTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "signed char",
        CXTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "float",
        CXTypeKind::Float {
            _type: CXFloatType::F32,
        },
    ),
    (
        "double",
        CXTypeKind::Float {
            _type: CXFloatType::F64,
        },
    ),
];
