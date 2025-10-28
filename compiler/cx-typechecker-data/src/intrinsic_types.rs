use crate::cx_types::CXTypeKind;

pub fn is_intrinsic_type(name: &str) -> bool {
    for (intrinsic_name, _) in INTRINSIC_TYPES.iter() {
        if intrinsic_name == &name {
            return true;
        }
    }
    false
}

pub const INTRINSIC_IMPORTS: &[&str] = &["std/intrinsic/memory.cx", "std/intrinsic/assertion.cx"];

pub const INTRINSIC_TYPES: &[(&str, CXTypeKind)] = &[
    ("void", CXTypeKind::Unit),
    ("bool", CXTypeKind::Bool),
    (
        "i8",
        CXTypeKind::Integer {
            signed: true,
            bytes: 1,
        },
    ),
    (
        "i16",
        CXTypeKind::Integer {
            signed: true,
            bytes: 2,
        },
    ),
    (
        "i32",
        CXTypeKind::Integer {
            signed: true,
            bytes: 4,
        },
    ),
    (
        "i64",
        CXTypeKind::Integer {
            signed: true,
            bytes: 8,
        },
    ),
    (
        "u8",
        CXTypeKind::Integer {
            signed: false,
            bytes: 1,
        },
    ),
    (
        "u16",
        CXTypeKind::Integer {
            signed: false,
            bytes: 2,
        },
    ),
    (
        "u32",
        CXTypeKind::Integer {
            signed: false,
            bytes: 4,
        },
    ),
    (
        "u64",
        CXTypeKind::Integer {
            signed: false,
            bytes: 8,
        },
    ),
    (
        "usize",
        CXTypeKind::Integer {
            signed: false,
            bytes: std::mem::size_of::<usize>() as u8,
        },
    ),
    (
        "isize",
        CXTypeKind::Integer {
            signed: true,
            bytes: std::mem::size_of::<isize>() as u8,
        },
    ),
    ("f32", CXTypeKind::Float { bytes: 4 }),
    ("f64", CXTypeKind::Float { bytes: 8 }),
    (
        "int",
        CXTypeKind::Integer {
            signed: true,
            bytes: 4,
        },
    ),
    (
        "signed int",
        CXTypeKind::Integer {
            signed: true,
            bytes: 4,
        },
    ),
    (
        "unsigned int",
        CXTypeKind::Integer {
            signed: false,
            bytes: 4,
        },
    ),
    (
        "signed",
        CXTypeKind::Integer {
            signed: true,
            bytes: 4,
        },
    ),
    (
        "unsigned",
        CXTypeKind::Integer {
            signed: false,
            bytes: 4,
        },
    ),
    (
        "long",
        CXTypeKind::Integer {
            signed: true,
            bytes: 8,
        },
    ),
    (
        "long int",
        CXTypeKind::Integer {
            signed: true,
            bytes: 8,
        },
    ),
    (
        "unsigned long",
        CXTypeKind::Integer {
            signed: false,
            bytes: 8,
        },
    ),
    (
        "long long",
        CXTypeKind::Integer {
            signed: true,
            bytes: 8,
        },
    ),
    (
        "long long int",
        CXTypeKind::Integer {
            signed: true,
            bytes: 8,
        },
    ),
    (
        "unsigned long long",
        CXTypeKind::Integer {
            signed: false,
            bytes: 8,
        },
    ),
    (
        "char",
        CXTypeKind::Integer {
            signed: false,
            bytes: 1,
        },
    ),
    (
        "unsigned char",
        CXTypeKind::Integer {
            signed: false,
            bytes: 1,
        },
    ),
    (
        "signed char",
        CXTypeKind::Integer {
            signed: true,
            bytes: 1,
        },
    ),
    ("float", CXTypeKind::Float { bytes: 4 }),
    ("double", CXTypeKind::Float { bytes: 8 }),
];
