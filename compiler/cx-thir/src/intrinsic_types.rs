use crate::thir::data::{THIRFloatType, THIRIntType, THIRTypeKind};

pub fn is_intrinsic_type(name: &str) -> bool {
    for (intrinsic_name, _) in INTRINSIC_TYPES.iter() {
        if intrinsic_name == &name {
            return true;
        }
    }
    false
}

pub const INTRINSIC_IMPORTS: &[&str] = &["std/intrinsic/assertion.cx"];

pub const INTRINSIC_TYPES: &[(&str, THIRTypeKind)] = &[
    ("void", THIRTypeKind::Unit),
    (
        "bool",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I1,
        },
    ),
    (
        "_Bool",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I1,
        },
    ),
    (
        "i8",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I8,
        },
    ),
    (
        "i16",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        },
    ),
    (
        "i32",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        },
    ),
    (
        "i64",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "u8",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        },
    ),
    (
        "u16",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I16,
        },
    ),
    (
        "u32",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        },
    ),
    (
        "u64",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "usize",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "isize",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "f32",
        THIRTypeKind::Float {
            _type: THIRFloatType::F32,
        },
    ),
    (
        "f64",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    (
        "int",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        },
    ),
    (
        "signed int",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        },
    ),
    (
        "unsigned int",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        },
    ),
    (
        "unsigned short",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I16,
        },
    ),
    (
        "unsigned short int",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        },
    ),
    (
        "signed",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        },
    ),
    (
        "unsigned",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        },
    ),
    (
        "long",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "long int",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "long unsigned int",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "unsigned long int",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "unsigned long",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "long long",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "long long int",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "signed long long int",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        },
    ),
    (
        "unsigned long long int",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "unsigned long long",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        },
    ),
    (
        "char",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        },
    ),
    (
        "unsigned char",
        THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        },
    ),
    (
        "signed char",
        THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I8,
        },
    ),
    (
        "float",
        THIRTypeKind::Float {
            _type: THIRFloatType::F32,
        },
    ),
    (
        "double",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    // C header compatibility shims until MIR has long-double, complex, and
    // target ABI va_list types.
    (
        "__builtin_va_list",
        THIRTypeKind::Opaque {
            size: 24,
            alignment: 8,
        },
    ),
    (
        "long double",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    (
        "_Complex float",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    (
        "_Complex double",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    (
        "_Complex long double",
        THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        },
    ),
    ("_str", THIRTypeKind::Str),
];
