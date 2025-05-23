use cx_data_ast::parse::ast::CXTypeMap;
use cx_data_ast::parse::value_type::CXTypeKind;

pub fn add_internal_types(type_map: &mut CXTypeMap) {
    macro_rules! add {
        ($name:expr, $type_:expr) => {
            type_map.insert(
                $name.to_string(),
                $type_.to_val_type()
            );
        };
    }

    add!("void", CXTypeKind::Unit);

    add!("i8", CXTypeKind::Integer { bytes: 1, signed: true });
    add!("i16", CXTypeKind::Integer { bytes: 2, signed: true });
    add!("i32", CXTypeKind::Integer { bytes: 4, signed: true });
    add!("i64", CXTypeKind::Integer { bytes: 8, signed: true });
    add!("isize", CXTypeKind::Integer { bytes: 8, signed: true });
    
    add!("u8", CXTypeKind::Integer { bytes: 1, signed: false });
    add!("u16", CXTypeKind::Integer { bytes: 2, signed: false });
    add!("u32", CXTypeKind::Integer { bytes: 4, signed: false });
    add!("u64", CXTypeKind::Integer { bytes: 8, signed: false });
    add!("usize", CXTypeKind::Integer { bytes: 8, signed: false });
    
    add!("f32", CXTypeKind::Float { bytes: 4 });
    add!("f64", CXTypeKind::Float { bytes: 8 });

    add!("int", CXTypeKind::Integer { bytes: 4, signed: true });
    add!("signed int", CXTypeKind::Integer { bytes: 4, signed: true });
    add!("unsigned int", CXTypeKind::Integer { bytes: 4, signed: false });
    add!("signed", CXTypeKind::Integer { bytes: 4, signed: true });
    add!("unsigned", CXTypeKind::Integer { bytes: 4, signed: false });
    add!("long", CXTypeKind::Integer { bytes: 8, signed: true });
    add!("long int", CXTypeKind::Integer { bytes: 8, signed: true });
    add!("unsigned long", CXTypeKind::Integer { bytes: 8, signed: false });
    add!("long long", CXTypeKind::Integer { bytes: 8, signed: true });
    add!("long long int", CXTypeKind::Integer { bytes: 8, signed: true });
    add!("unsigned long long", CXTypeKind::Integer { bytes: 8, signed: false });

    add!("char", CXTypeKind::Integer { bytes: 1, signed: false });
    add!("unsigned char", CXTypeKind::Integer { bytes: 1, signed: false });
    add!("signed char", CXTypeKind::Integer { bytes: 1, signed: true });

    add!("float", CXTypeKind::Float { bytes: 4 });
    add!("double", CXTypeKind::Float { bytes: 8 });
}