use cx_target::ArchitectureConfig;

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

// TODO: Better architecture-specific handling of integer-like types and other intrinsics
pub const INTRINSIC_TYPES: &[(&str, fn(&ArchitectureConfig) -> Option<THIRTypeKind>)] = &[
    ("void", |_| Some(THIRTypeKind::Void)),
    ("unreachable", |_| Some(THIRTypeKind::Unreachable)),
    ("bool", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I1,
        })
    }),
    ("_Bool", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I1,
        })
    }),
    ("i8", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I8,
        })
    }),
    ("i16", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        })
    }),
    ("i32", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        })
    }),
    ("i64", |arch| {
        (arch.pointer_size() >= 8).then(|| THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("u8", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        })
    }),
    ("u16", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I16,
        })
    }),
    ("u32", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        })
    }),
    ("u64", |arch| {
        (arch.pointer_size() >= 8).then(|| THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("usize", |arch| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::from_bytes(arch.pointer_size() as u8)
                .expect("pointer size should be 1, 2, 4, or 8 bytes"),
        })
    }),
    ("isize", |arch| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::from_bytes(arch.pointer_size() as u8)
                .expect("pointer size should be 1, 2, 4, or 8 bytes"),
        })
    }),
    ("f32", |_| {
        Some(THIRTypeKind::Float {
            _type: THIRFloatType::F32,
        })
    }),
    ("f64", |arch| {
        (arch.pointer_size() >= 8).then(|| THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        })
    }),
    ("int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        })
    }),
    ("signed int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        })
    }),
    ("unsigned int", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        })
    }),
    ("unsigned short", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I16,
        })
    }),
    ("short", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        })
    }),
    ("short int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        })
    }),
    ("signed short", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        })
    }),
    ("signed short int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I16,
        })
    }),
    ("unsigned short int", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        })
    }),
    ("signed", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I32,
        })
    }),
    ("unsigned", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I32,
        })
    }),
    ("long", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("signed long", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("signed long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("long unsigned int", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("unsigned long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("unsigned long", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("long long", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("long long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("signed long long", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("signed long long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I64,
        })
    }),
    ("unsigned long long int", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("unsigned long long", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I64,
        })
    }),
    ("char", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        })
    }),
    ("unsigned char", |_| {
        Some(THIRTypeKind::Integer {
            signed: false,
            _type: THIRIntType::I8,
        })
    }),
    ("signed char", |_| {
        Some(THIRTypeKind::Integer {
            signed: true,
            _type: THIRIntType::I8,
        })
    }),
    ("float", |_| {
        Some(THIRTypeKind::Float {
            _type: THIRFloatType::F32,
        })
    }),
    ("double", |arch| {
        (arch.pointer_size() >= 8).then(|| THIRTypeKind::Float {
            _type: THIRFloatType::F64,
        })
    }),
    // TODO: C header compatibility shims until MIR has long-double, complex, and
    // target ABI va_list types.
    ("__builtin_va_list", |_| Some(THIRTypeKind::Opaque {
        size: 24,
        alignment: 8,
    })),
    ("long double", |_| Some(THIRTypeKind::Float {
        _type: THIRFloatType::F64,
    })),
    ("_Complex float", |_| Some(THIRTypeKind::Float {
        _type: THIRFloatType::F64,
    })),
    ("_Complex double", |_| Some(THIRTypeKind::Float {
        _type: THIRFloatType::F64,
    })),
    ("_Complex long double", |_| Some(THIRTypeKind::Float {
        _type: THIRFloatType::F64,
    })),
    ("_str", |_| Some(THIRTypeKind::Str)),
];
