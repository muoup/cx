use crate::mir::types::{CXFloatType, CXIntegerType, MIRTypeKind};

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

pub const INTRINSIC_TYPES: &[(&str, MIRTypeKind)] = &[
    ("void", MIRTypeKind::Unit),
    ("bool", MIRTypeKind::Integer { signed: false, _type: CXIntegerType::I1 }),
    (
        "i8",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "i16",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I16,
        },
    ),
    (
        "i32",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "i64",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "u8",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "u16",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I16,
        },
    ),
    (
        "u32",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "u64",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "usize",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::from_bytes(std::mem::size_of::<usize>() as u8).unwrap(),
        },
    ),
    (
        "isize",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::from_bytes(std::mem::size_of::<isize>() as u8).unwrap(),
        },
    ),
    (
        "f32",
        MIRTypeKind::Float {
            _type: CXFloatType::F32,
        },
    ),
    (
        "f64",
        MIRTypeKind::Float {
            _type: CXFloatType::F64,
        },
    ),
    (
        "int",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "signed int",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "unsigned int",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "signed",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "unsigned",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I32,
        },
    ),
    (
        "long",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long int",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "unsigned long",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long long",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "long long int",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "unsigned long long",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I64,
        },
    ),
    (
        "char",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "unsigned char",
        MIRTypeKind::Integer {
            signed: false,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "signed char",
        MIRTypeKind::Integer {
            signed: true,
            _type: CXIntegerType::I8,
        },
    ),
    (
        "float",
        MIRTypeKind::Float {
            _type: CXFloatType::F32,
        },
    ),
    (
        "double",
        MIRTypeKind::Float {
            _type: CXFloatType::F64,
        },
    ),
];
