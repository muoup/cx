use cx_target::ArchitectureConfig;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HIRIntrinsicType {
    Void,
    Unreachable,
    Str,
    Bool,
    Integer { signed: bool, bytes: u8 },
    Float { bytes: u8 },
    Opaque { size: usize, alignment: usize },
}

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
pub const INTRINSIC_TYPES: &[(&str, fn(&ArchitectureConfig) -> Option<HIRIntrinsicType>)] = &[
    ("void", |_| Some(HIRIntrinsicType::Void)),
    ("unreachable", |_| Some(HIRIntrinsicType::Unreachable)),
    ("bool", |_| Some(HIRIntrinsicType::Bool)),
    ("_Bool", |_| Some(HIRIntrinsicType::Bool)),
    ("i8", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 1,
        })
    }),
    ("i16", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 2,
        })
    }),
    ("i32", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 4,
        })
    }),
    ("i64", |arch| {
        (arch.pointer_size() >= 8).then(|| HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("u8", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 1,
        })
    }),
    ("u16", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 2,
        })
    }),
    ("u32", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 4,
        })
    }),
    ("u64", |arch| {
        (arch.pointer_size() >= 8).then(|| HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("usize", |arch| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: arch.pointer_size() as u8,
        })
    }),
    ("isize", |arch| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: arch.pointer_size() as u8,
        })
    }),
    ("f32", |_| Some(HIRIntrinsicType::Float { bytes: 4 })),
    ("f64", |arch| {
        (arch.pointer_size() >= 8).then(|| HIRIntrinsicType::Float { bytes: 8 })
    }),
    ("int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 4,
        })
    }),
    ("signed int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 4,
        })
    }),
    ("unsigned int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 4,
        })
    }),
    ("unsigned short", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 2,
        })
    }),
    ("short", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 2,
        })
    }),
    ("short int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 2,
        })
    }),
    ("signed short", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 2,
        })
    }),
    ("signed short int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 2,
        })
    }),
    ("unsigned short int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 2,
        })
    }),
    ("signed", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 4,
        })
    }),
    ("unsigned", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 4,
        })
    }),
    ("long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("signed long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("signed long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("long unsigned int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("unsigned long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("unsigned long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("long long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("long long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("signed long long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("signed long long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 8,
        })
    }),
    ("unsigned long long int", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("unsigned long long", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 8,
        })
    }),
    ("char", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 1,
        })
    }),
    ("unsigned char", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: false,
            bytes: 1,
        })
    }),
    ("signed char", |_| {
        Some(HIRIntrinsicType::Integer {
            signed: true,
            bytes: 1,
        })
    }),
    ("float", |_| Some(HIRIntrinsicType::Float { bytes: 4 })),
    ("double", |arch| {
        (arch.pointer_size() >= 8).then(|| HIRIntrinsicType::Float { bytes: 8 })
    }),
    // TODO: C header compatibility shims until MIR has long-double, complex, and
    // target ABI va_list types.
    ("__builtin_va_list", |_| {
        Some(HIRIntrinsicType::Opaque {
            size: 24,
            alignment: 8,
        })
    }),
    ("long double", |_| {
        Some(HIRIntrinsicType::Float { bytes: 8 })
    }),
    ("__float128", |_| {
        Some(HIRIntrinsicType::Opaque {
            size: 16,
            alignment: 16,
        })
    }),
    ("_Complex float", |_| {
        Some(HIRIntrinsicType::Float { bytes: 8 })
    }),
    ("_Complex double", |_| {
        Some(HIRIntrinsicType::Float { bytes: 8 })
    }),
    ("_Complex long double", |_| {
        Some(HIRIntrinsicType::Float { bytes: 8 })
    }),
    ("_str", |_| Some(HIRIntrinsicType::Str)),
];
